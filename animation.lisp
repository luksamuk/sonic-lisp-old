;;;; animation.lisp
;;;; Copyright (c) 2018 Lucas Vieira <lucasvieira@lisp.com.br>
;;;; This file is distributed under the MIT License.
;;;; See LICENSE for details.


(in-package #:sonic-lisp)

;; Animation stuff
(defstruct animation-properties
  keyframes
  (time-per-frame 0.16 :type single-float)
  (loopback       nil))


(defclass animator ()
  ((image
    :initarg :image
    :accessor texture)
   (texture-size
    :initarg :texture-size
    :accessor texture-size
    :initform (gamekit:vec2 360 360))
   (frames-per-line
    :initarg :frames-per-line
    :accessor frames-per-line
    :initform 6)
   (current-animation
    :accessor which-anim
    :initform nil)
   (current-animation-timer
    :accessor anim-timer
    :initform 0)
   (current-frame
    :accessor frame
    :initform 0)
   (current-frame-index
    :accessor frame-index
    :initform 0)
   (animations
    :initarg :animations
    :accessor animations
    :initform nil)))

(defgeneric set-animation-q (animator animation-name))
(defgeneric reg-animation-q (animator &key name keyframes time-per-frame loopback-index))
(defgeneric update-animation (animator delta-time))
(defgeneric draw-animation (animator gamekit:vec2))


(defmethod set-animation-q ((animator animator) animation-name)
  (if (and (animations animator)
	   (gethash animation-name (animations animator)))
      (unless (eql animation-name (which-anim animator))
	(setf (frame animator) 0
	      (anim-timer animator) 0
	      (which-anim animator) animation-name))
      (setf (frame animator) 0
	    (anim-timer animator) 0
	    (which-anim animator) (if (eq animation-name :keep)
				      (which-anim animator)
				      nil))))

(defmethod reg-animation-q ((animator animator) &key name keyframes (time-per-frame 0.16) (loopback-index 0))
  (let ((keyframes (make-array (length keyframes)
			       :initial-contents keyframes)))
    ;; Initialize animation array if we didn't
    (unless (animations animator)
      (setf (animations animator)
	    (make-hash-table)))
    (setf (gethash name (animations animator))
	  (make-animation-properties :keyframes keyframes
				     :time-per-frame time-per-frame
				     :loopback loopback-index))))

(defmethod update-animation ((animator animator) delta-time)
  (let ((animation-props (gethash (which-anim animator)
				  (animations animator))))
    (when animation-props
      ;; If we surpassed the frame duration for the animation,
      ;; calculate the amount of frames to skip and then wrap
      ;; the timer around.
      (incf (anim-timer animator) delta-time)
      (when (>= (anim-timer animator)
		(animation-properties-time-per-frame animation-props))
	(let ((frames-skipped (floor (/ (anim-timer animator)
					(animation-properties-time-per-frame animation-props)))))
	  ;; Restore timer 
	  (setf (anim-timer animator)
		(rem (anim-timer animator)
		     (animation-properties-time-per-frame animation-props)))
	  ;; Increment current frame so that it changes the frame
	  (incf (frame animator) frames-skipped)
	  ;; If we're beyond the last frame, wrap around
	  (when (>= (frame animator) (length (animation-properties-keyframes animation-props)))
	    ;; We need to determine at what frame should we stop.
	    ;; We take the loopback frame into account and consider only the
	    ;; [loopback, last-frame] range for another `rem` operation.
	    (let ((loopback-range (- (length (animation-properties-keyframes animation-props))
				     (animation-properties-loopback animation-props))))
	      (setf (frame animator)
		    (+ (animation-properties-loopback animation-props)
		       (rem (frame animator) loopback-range))))))))))
	    
      


(defmethod draw-animation ((animator animator) (position gamekit:vec2))
  (let ((animation-props (gethash (which-anim animator)
				  (animations animator))))
    (when animation-props
      (let* ((frame (aref (animation-properties-keyframes animation-props)
			  (frame animator)))
	     (frame-x-index (rem frame
				 (frames-per-line animator)))
	     (frame-y-index (floor (/ frame
				      (frames-per-line animator))))
	     (frame-side (/ (gamekit:x (texture-size animator))
			    (frames-per-line animator))))
	(gamekit:draw-image position
			    (texture animator)
			    :origin (gamekit:vec2 (* frame-x-index frame-side)
						  (- (- (gamekit:y (texture-size animator))
							frame-side)
						     (* frame-y-index frame-side)))
			    :width frame-side :height frame-side)))))
