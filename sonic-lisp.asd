;;;; sonic-lisp.asd
;;;; Copyright (c) 2018 Lucas Vieira <lucasvieira@lisp.com.br>
;;;; This file is distributed under the MIT License.
;;;; See LICENSE for details.


(asdf:defsystem #:sonic-lisp
  :description "Clone of Sonic The Hedgehog's Engine, written in Common Lisp"
  :author "Lucas Vieira <lucasvieira@lisp.com.br>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:trivial-gamekit)
  :components ((:file "package")
	       (:file "global")
	       (:file "input")
	       (:file "animation")
	       (:file "player")
               (:file "sonic-lisp")))

(asdf:defsystem #:sonic-lisp/bundle
    :description "Bundles sonic-lisp into a standalone executable"
    :author "Lucas Vieira <lucasvieira@lisp.com.br>"
    :license "MIT"
    :version "0.0.1"
    :serial t
    :depends-on (#:trivial-gamekit/distribution #:sonic-lisp)
    :components ((:file "bundle")))

