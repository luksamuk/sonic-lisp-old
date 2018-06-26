;;;; input.lisp
;;;; Copyright (c) 2018 Lucas Vieira <lucasvieira@lisp.com.br>
;;;; This file is distributed under the MIT License.
;;;; See LICENSE for details.


(in-package #:sonic-lisp)

;; Input stuff
(defvar *input-async* '(:up    nil
			:down  nil
			:left  nil
			:right nil
			:start nil
			:a     nil
			:b     nil
			:x     nil
			:y     nil))

(defvar *input* '(:up    nil
		  :down  nil
		  :left  nil
		  :right nil
		  :start nil
		  :a     nil
		  :b     nil
		  :x     nil
		  :y     nil))
(defvar *old-input* '(:up    nil
		      :down  nil
		      :left  nil
		      :right nil
		      :start nil
		      :a     nil
		      :b     nil
		      :x     nil
		      :y     nil))

(defun pressing-p (key)
  (getf *input* key))

(defun pressed-p (key)
  (and (getf *input* key)
       (not (getf *old-input* key))))

(defun update-input ()
  ;; copy *input* to *old-input*,
  ;; copy *input-async* to *input*
  (setf *old-input* (copy-list *input*)
	*input* (copy-list *input-async*)))

