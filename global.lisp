;;;; global.lisp
;;;; Copyright (c) 2018 Lucas Vieira <lucasvieira@lisp.com.br>
;;;; This file is distributed under the MIT License.
;;;; See LICENSE for details.


(in-package #:sonic-lisp)

(defvar +black+ (gamekit:vec4 0 0 0 1))
(defvar +origin+ (gamekit:vec2 0 0))

(defvar *dt* 0)
(defvar *last-checked-time* (get-internal-real-time))

(defvar *game-properties*
  (list :title "Sonic Engine"
	:window-width  640
	:window-height 360
	:background +black+))


;; Game stuff
(gamekit:defgame sonic-game () ()
		 (:viewport-title  (getf *game-properties* :title))
		 (:viewport-width  (getf *game-properties* :window-width))
		 (:viewport-height (getf *game-properties* :window-height)))

;;(gamekit:start 'sonic-game)

;; Resource initialization
(gamekit:register-resource-package
 :keyword (merge-pathnames
	   "resources/"
	   (asdf:system-relative-pathname :sonic-lisp "")))
(gamekit:define-image :sonic-sprites "sprites/sonic.png")
;;(gamekit:define-sound :level-music   "bgm/chaoticcanyon.ogg")
(gamekit:define-sound :sfx-jump "sfx/02_jump.ogg")
(gamekit:define-sound :sfx-skidding "sfx/00_skidding.ogg")

(gamekit:define-image :bg-layer0 "bg/level6/layer0.png")
;;(gamekit:define-image :bg-layer1 "bg/level6/layer1.png")
;;(gamekit:define-image :bg-layer2 "bg/level6/layer2.png")
;;(gamekit:define-image :bg-layer3 "bg/level6/layer3.png")
;;(gamekit:define-image :bg-layer4 "bg/level6/layer4.png")
;;(gamekit:define-image :bg-layer5 "bg/level6/layer5.png")

