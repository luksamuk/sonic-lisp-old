;;;; sonic-lisp.lisp
;;;; Copyright (c) 2018 Lucas Vieira <lucasvieira@lisp.com.br>
;;;; This file is distributed under the MIT License.
;;;; See LICENSE for details.


(in-package #:sonic-lisp)

;; Game stuff
(defvar *sonic* (create-player (gamekit:vec2 100 100)))

(defun reset-sonic ()
  (setf (player-pos *sonic*) (gamekit:vec2 100 100)
	(player-spd *sonic*) (gamekit:vec2 0 0)
	(state *sonic*) :none
	(ground *sonic*) t))


(reg-animation-q (animator *sonic*)
		 :name :idle
		 :keyframes '(0 0 0 0 0 0 0 0 0 0 1 2 3 3 4 4)
		 :time-per-frame 0.24
		 :loopback-index 12)
(reg-animation-q (animator *sonic*)
		 :name :walk
		 :keyframes '(5 6 7 8 9 10)
		 :time-per-frame 0.12)
(reg-animation-q (animator *sonic*)
		 :name :run
		 :keyframes '(11 12 13 14)
		 :time-per-frame 0.12)
(reg-animation-q (animator *sonic*)
		 :name :roll
		 :keyframes '(15 16 17 18 19 20 21 22)
		 :time-per-frame 0.12)
(reg-animation-q (animator *sonic*)
		 :name :skid
		 :keyframes '(23))
(reg-animation-q (animator *sonic*)
		 :name :super-run
		 :keyframes '(24 25 26 27)
		 :time-per-frame 0.08)
(reg-animation-q (animator *sonic*)
		 :name :push
		 :keyframes '(28 29 30 31)
		 :time-per-frame 0.48)
(reg-animation-q (animator *sonic*)
		 :name :crouch
		 :keyframes '(32))
(reg-animation-q (animator *sonic*)
		 :name :look-up
		 :keyframes '(33))
(reg-animation-q (animator *sonic*)
		 :name :death
		 :keyframes '(34))
		 
(set-animation-q (animator *sonic*) :idle)



(defmethod gamekit:post-initialize ((app sonic-game))
  ;;(gamekit:play-sound :level-music :looped-p t)
  (gamekit:bind-button :up :pressed  (lambda () (setf (getf *input-async* :up) t)))
  (gamekit:bind-button :up :released (lambda () (setf (getf *input-async* :up) nil)))

  (gamekit:bind-button :down :pressed  (lambda () (setf (getf *input-async* :down) t)))
  (gamekit:bind-button :down :released (lambda () (setf (getf *input-async* :down) nil)))

  (gamekit:bind-button :left :pressed  (lambda () (setf (getf *input-async* :left) t)))
  (gamekit:bind-button :left :released (lambda () (setf (getf *input-async* :left) nil)))

  (gamekit:bind-button :right :pressed  (lambda () (setf (getf *input-async* :right) t)))
  (gamekit:bind-button :right :released (lambda () (setf (getf *input-async* :right) nil)))

  (gamekit:bind-button :enter :pressed  (lambda () (setf (getf *input-async* :start) t)))
  (gamekit:bind-button :enter :released (lambda () (setf (getf *input-async* :start) nil)))

  (gamekit:bind-button :s :pressed  (lambda () (setf (getf *input-async* :a) t)))
  (gamekit:bind-button :s :released (lambda () (setf (getf *input-async* :a) nil)))
  )

(defun update-delta-time ()
  ;; Update real delta-time
  (let ((current-time (get-internal-real-time)))
    (setf *dt* (/ (- current-time *last-checked-time*)
		     internal-time-units-per-second)
	  *last-checked-time* current-time
	  *fps* (+ (* *fps* 0.99)
		   (* (/ 1 *dt*) (- 1.0 0.99))))))

(defmethod gamekit:act ((app sonic-game))
  (update-delta-time)
  (update-input)
  (update-player *sonic* *dt*))


(defun draw-bg ()
  (gamekit:with-pushed-canvas ()
    (gamekit:draw-image +origin+ :bg-layer0)))


(defmethod gamekit:draw ((app sonic-game))
  (draw-bg)
  (draw-player *sonic*))




;;;; =====

(defun start ()
  (gamekit:start 'sonic-game))

(defun stop ()
  (gamekit:stop))

(defun reset ()
  (reset-sonic))
