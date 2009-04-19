;;;; -*- Mode: Lisp -*-
;;;; Author: Philippe Brochard <hocwp@free.fr>
;;;; ASDF System Definition
;;;
;;; #date#: Wed Jan  4 22:43:28 2006

(in-package #:asdf)

(defsystem cl-newworld
    :description "CL-NewWorld: a simple MUD server"
    :version "0.1"
    :author "Philippe Brochard  <hocwp@free.fr>"
    :licence "GNU Public License (GPL)"
    :components ((:file "net")
		 (:file "tools")
		 (:file "transfer-stream"
			:depends-on ("tools"))
		 (:file "my-http"
			:depends-on ("net" "tools" "transfer-stream"))
		 (:file "package"
			:depends-on ("net" "tools" "my-http" "transfer-stream"))
		 (:file "cl-newworld-internal"
			:depends-on ("package"))
		 (:file "cl-newworld-http"
			:depends-on ("cl-newworld-internal" "net" "my-http" "package"))
		 (:file "cl-newworld"
			:depends-on ("cl-newworld-internal"  "net" "cl-newworld-http" "package"))
		 (:file "extension"
			:depends-on ("cl-newworld-internal" "cl-newworld"))
		 (:file "make-map"
			:depends-on ("cl-newworld" "cl-newworld-internal" "extension"))))








