;;;; -*- Mode: Lisp -*-
;;;; Author: Philippe Brochard <hocwp@free.fr>
;;;; ASDF System Definition

(in-package #:asdf)

(defsystem cl-newworld-sdl
    :description "CL-NewWorld-SDL: a simple MUD server - SDL client part"
    :version "0.1"
    :author "Philippe Brochard  <hocwp@free.fr>"
    :licence "GNU Public License (GPL)"
    :components ((:file "net")
		 (:file "tools")
		 (:file "package"
			:depends-on ("net" "tools"))
		 (:file "utils"
			:depends-on ("package"))
		 (:file "frame"
			:depends-on ("tools" "package" "utils"))
		 (:file "help-screen"
		 	:depends-on ("package" "frame"))
		 (:file "main-screen"
			:depends-on ("package" "frame"))
		 (:file "login-screen"
			:depends-on ("package" "frame" "main-screen"))
		 (:file "cl-newworld-sdl"
			:depends-on ("package" "frame" "help-screen" "login-screen" "main-screen")))
    :depends-on (:cffi :lispbuilder-sdl))








