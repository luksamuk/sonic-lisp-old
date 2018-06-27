;;;; player.lisp
;;;; Copyright (c) 2018 Lucas Vieira <lucasvieira@lisp.com.br>
;;;; This file is distributed under the MIT License.
;;;; See LICENSE for details.


(in-package #:sonic-lisp)

(defvar *sonic-default-vars*
  '(:gravity      0.21875
    :accel        0.046875
    :air-accel    0.09375
    :friction     0.046875
    :decel        0.5
    :max-x-spd    12.0
    :top-x-spd    6.0
    :jump-str     6.5
    :min-jump     4.0
    :air-drag     0.96875
    :drag-min-x   0.0125
    :drag-min-y   4.0
    :slope-factor 0.125
    :roll-frict   0.0234375
    :roll-decel   0.125
    :roll-top-x   16.0
    :roll-min-x   1.03125
    :unroll-min-x 0.046875))

(defmacro get-default (key)
  `(getf *sonic-default-vars* ,key))

;; Player stuff
(defclass player ()
  ((animator
    :initarg :animator
    :accessor animator
    :initform nil)
   (position
    :initarg :position
    :accessor player-pos
    :initform (gamekit:vec2 0 0))
   (direction
    :accessor direction
    :initform 1)
   (velocity
    :initarg :velocity
    :accessor player-spd
    :initform (gamekit:vec2 0 0))
   (state
    :accessor state
    :initform :none)
   (ground
    :accessor ground
    :initform t)))

(defmethod initialize-instance :after ((player player) &key)
  (setf (animator player) (make-instance 'animator :image :sonic-sprites)))


(defun create-player (&optional (where (gamekit:vec2 0 0)))
  (make-instance 'player :position where))

(defgeneric update-player-animation (player delta-time))
(defgeneric update-player-actions (player delta-time))
(defgeneric update-player-movement (player delta-time))
(defgeneric update-player (player delta-time))
(defgeneric draw-player (player))


(defmethod update-player-animation ((player player) delta-time)
  (update-animation (animator player) delta-time)
  ;; Change animations accordingly
  (let ((x-speed (abs (gamekit:x (player-spd player)))))
    (set-animation-q
     (animator player)
     (if (ground player)
	 (case (state player)
	   (:none (cond ((= x-speed 0)    :idle)
			((< x-speed 5.9)  :walk)
			((< x-speed 9.95) :run)
			(t :super-run)))
	   (:look-up :look-up)
	   (:crouch :crouch)
	   (:skid :skid)
	   ((:spindash :roll) :roll)
	   (otherwise :keep))
	 ;; On air
	 (case (state player)
	   ((:jump :roll) :roll)
	   (otherwise :keep))))))


(defmethod update-player-actions ((player player) delta-time)
  (let ((x-speed (gamekit:x (player-spd player)))
	(y-speed (gamekit:y (player-spd player)))
	(on-ground (ground player))
	(state (state player)))
    ;; Ground player actions
    (if on-ground
      ;;     Crouch down
      (cond ((and (eq state :none)
		  (= x-speed 0)
		  (pressing-p :down))
	     (setf (state player) :crouch))
	    ;; Look up
	    ((and (eq state :none)
		  (= x-speed 0)
		  (pressing-p :up))
	     (setf (state player) :look-up))
	    ;; Reset state to normal when not looking
	    ;; up or crouching down anymore
	    ((and (or (eq state :look-up)
		      (eq state :crouch))
		  (and (not (pressing-p :down))
		       (not (pressing-p :up))))
	     (setf (state player) :none))
	    ;; Jump
	    ((and (not (eq state :crouch))
		  (not (eq state :spindash))
		  (pressed-p :a))
	     (incf (gamekit:y (player-spd player))
		   (get-default :jump-str))
	     (setf (ground player) nil
		   (state player) :jump)
	     (gamekit:play-sound :sfx-jump))
	    ;; Skidding
	    ((and (eq state :none)
		  (or (and (> x-speed (get-default :decel))
			   (pressing-p :left))
		      (and (< x-speed (- (get-default :decel)))
			   (pressing-p :right))))
	     (setf (state player) :skid)
	     ;; Skidding sound effect can be irritating if
	     ;; triggered at low speeds
	     (unless (<= (abs x-speed) 3.0)
	       (gamekit:play-sound :sfx-skidding)))
	    ;; Reset state when stopped skidding, or when
	    ;; changing sides while skidding
	    ((and (eq state :skid)
		  (or (and (not (pressing-p :left))
			   (not (pressing-p :right)))
		      (= x-speed 0)))
	     (setf (state player) :none))
	    ;; Begin spindash
	    ((and (eq state :crouch)
		  (pressed-p :a))
	     (setf (state player) :spindash)
	     (gamekit:play-sound :sfx-spindash))
	    ;; Release spindash when not holding
	    ;; down anymore
	    ((and (eq state :spindash)
		  (not (pressing-p :down)))
	     (setf (state player) :roll
		   (gamekit:x (player-spd player)) (* 8 (direction player)))
	     (gamekit:play-sound :sfx-release))
	    ;; Uncurl after rolling
	    ((and (eq state :roll)
		  (< (abs x-speed) (get-default :unroll-min-x)))
	     (setf (state player) :none
		   (gamekit:x (player-spd player)) 0))
	    ;; TODO: Spindash revolutions
	    )
      ;; Air player actions
      ;;    Short jump
      (cond ((and (eq state :jump)
		  (not (pressing-p :a))
		  (> y-speed (get-default :min-jump)))
	     (setf (gamekit:y (player-spd player))
		   (get-default :min-jump)))))))


(defmethod update-player-movement ((player player) delta-time)
  (let ((x-speed (gamekit:x (player-spd player)))
	(y-speed (gamekit:y (player-spd player)))
	(on-ground (ground player))
	(state (state player)))
    ;; Acceleration
    (when (not (or (eq state :look-up)
		   (eq state :crouch)
		   (eq state :spindash)
		   (eq state :skid)))
      (cond ((pressing-p :right)
	     (setf (direction player) 1)
	     (incf (gamekit:x (player-spd player))
		   (* (get-default :accel)
		      90.0
		      delta-time)))
	    ((pressing-p :left)
	     (setf (direction player) -1)
	     (decf (gamekit:x (player-spd player))
		   (* (get-default :accel)
		      90.0
		      delta-time)))))
    ;; Deceleration
    (when (or (and on-ground
		   (not (or (pressing-p :left)
			    (pressing-p :right))))
	      ;; Also apply acceleration when skidding
	      (eq state :skid))
      ;; When skidding, deceleration is stronger
      (let* ((deceleration-factor (if (eq state :skid) 60 10))
	     ;; Pre-calculate the acceleration value depending
	     ;; on the direction we're moving (not the direction
	     ;; we're necessarily facing)
	     (deceleration-value (* (if (> x-speed 0) -1 1)
				    (get-default :decel)
				    deceleration-factor
				    delta-time)))
	(setf (gamekit:x (player-spd player))
	      ;; If deceleration is gonna make us cross
	      ;; the 0.0 line, we just need a full stop
	      (cond ((<= (abs x-speed)
			 (get-default :decel))
		     0.0)
		    ;; Else we just apply deceleration accordingly.
		    (t (+ x-speed deceleration-value))))))
    ;; Gravity
    (unless on-ground
      (setf (gamekit:y (player-spd player))
	    (- y-speed
	       (* (get-default :gravity)
		  60
		  delta-time))))))
				
		       

(defmethod update-player ((player player) delta-time)
  ;; Animation
  (update-player-animation player delta-time)
  ;; Actions
  (update-player-actions player delta-time)
  ;; Movement
  (update-player-movement player delta-time)
  ;; Small test code for a fake ground detection.
  ;; Remove this later.
  (when (and (< (gamekit:y (player-pos player)) 100)
	     (not (ground player)))
    (setf (gamekit:y (player-pos player)) 100
	  (gamekit:y (player-spd player)) 0
	  (ground player) t
	  (state player) :none))
  ;; Update position
  (setf (gamekit:x (player-pos player))
	(+ (gamekit:x (player-pos player)) (gamekit:x (player-spd player)))
	(gamekit:y (player-pos player))
	(+ (gamekit:y (player-pos player)) (gamekit:y (player-spd player)))))


(defmacro debugger-text-draw (string position)
  `(gamekit:draw-text ,string ,position
		      :fill-color +white+
		      :font (gamekit:make-font :gohufont 11)))

(defmethod draw-player ((player player))
  (gamekit:with-pushed-canvas ()
    (gamekit:translate-canvas 10 340)
    (debugger-text-draw (format nil "pos >> ~a ~a"
				(floor (gamekit:x (player-pos player)))
				(floor (gamekit:y (player-pos player))))
			(gamekit:vec2 0 0))
    (debugger-text-draw (format nil "spd >> ~a ~a"
				(floor (gamekit:x (player-spd player)))
				(floor (gamekit:y (player-spd player))))
			(gamekit:vec2 0 -10))
    (debugger-text-draw (format nil "fps >> ~a"
				(if (not (= *dt* 0))
				    (floor (/ 1 *dt*))
				    0))
			(gamekit:vec2 0 -20)))
  (gamekit:with-pushed-canvas ()
    (gamekit:translate-canvas (gamekit:x (player-pos player))
			      (gamekit:y (player-pos player)))
    (gamekit:with-pushed-canvas ()
      (gamekit:scale-canvas (direction player) 1)
      (draw-animation (animator player)
		      (gamekit:vec2 -30 -30)))))
