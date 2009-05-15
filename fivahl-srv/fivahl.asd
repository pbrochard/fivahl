;;;; -*- Mode: Lisp -*-
;;;; Author: Philippe Brochard <hocwp@free.fr>
;;;; ASDF System Definition
;;;
;;; #date#: Wed Jan  4 22:43:28 2006

(in-package #:asdf)

(defsystem fivahl
    :description "Fivahl: a simple MUD server"
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
		 (:file "fivahl-internal"
			:depends-on ("package"))
		 (:file "fivahl-http"
			:depends-on ("fivahl-internal" "net" "my-http" "package"))
		 (:file "fivahl"
			:depends-on ("fivahl-internal"  "net" "fivahl-http" "package"))
		 (:file "extension"
			:depends-on ("fivahl-internal" "fivahl"))
		 (:file "make-map"
			:depends-on ("fivahl" "fivahl-internal" "extension"))))








