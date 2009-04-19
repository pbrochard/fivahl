
(in-package :common-lisp)

(defpackage :cl-newworld-sdl
  (:use :common-lisp :port :tools)
  (:export :main-loop))

(in-package :cl-newworld-sdl)


(defparameter *server-sock* nil)

(defparameter *font-name* sdl:*font-8x13*)
(defparameter *margin-x* 8)
(defparameter *margin-y* 15)
(defparameter *font* nil)


(defparameter *fulscreen-mode* nil)

(defparameter *all-resolution* '((640 480) (800 600) (1024 768)))
(defparameter *current-resolution* '((800 600) (1024 768)))

(defparameter *back-surface* nil)

(defparameter *redraw-needed* nil)
(defparameter *back-redraw-needed* nil)

; window or screen height
(defparameter *window-width* 1)
(defparameter *window-height* 1)

(defparameter *command-rect* nil)
(defparameter *inventory-rect* nil)
(defparameter *view-rect* nil)
(defparameter *log-rect* nil)

(defparameter *key-down-fun* nil)
(defparameter *mouse-button-fun* nil)
(defparameter *display-fun* nil)


(defstruct frame visible rect y-begin from title input content
	   title-color input-color content-color
	   foreground background foreground-sel
	   link-color link-color-sel rect-link-color
	   alpha
	   valid-fun)

(defstruct item frame link rect sel-p)


(defparameter *current-frame* nil)
(defparameter *visible-frames* nil)

(defparameter *item-list* nil)
(defparameter *current-item* nil)

(defparameter *hostname-frame* nil)
(defparameter *port-frame* nil)

(defparameter *error-frame* nil)

(defparameter *help-frame* nil)

(defparameter *input-frame* nil)
(defparameter *look-frame* nil)
(defparameter *log-frame* nil)
(defparameter *inventory-frame* nil)
(defparameter *command-frame* nil)


