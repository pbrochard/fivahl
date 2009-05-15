
(in-package :common-lisp)

(defpackage :fivahl
  (:use :common-lisp :port :my-http :transfer-stream :tools)
  (:export :main-loop))

(in-package :fivahl)

(defparameter *compteur-id* 0)


(defparameter *list-command* ())
(defparameter *list-command-detail* ())


(defvar *newline-default* (format nil "~%"))
(defvar *prompt-default* (format nil ".~%"))

(defvar *debug-log* nil)

(defparameter *monde* nil)

(defclass objet ()
  ((id :initform (incf *compteur-id*) :reader id)
   (nom :initarg :nom :initform nil :accessor nom)
   (descr :initarg :descr :initform nil :accessor descr)
   (contenu :initarg :contenu :initform nil :accessor contenu)))


;;; Definition des objets

(defclass monde (objet)
  ((en-attente :initform nil :accessor en-attente)
   (serveur-sock :initarg :serveur-sock :accessor serveur-sock)
   (serveur-http-sock :initarg :serveur-http-sock :accessor serveur-http-sock)
   (lieu-depart :initarg :lieu-depart :initform nil :accessor lieu-depart)))


(defclass lieu (objet)
  ())


(defvar *std-stream* *query-io*)

(defclass perso (objet)
  ((sock :initarg :sock :initform *std-stream* :accessor sock)
   (password :initarg :password :initform nil :accessor password)
   (hp :initarg :hp :initform 100 :accessor hp)
   (force :initarg :force :initform 1 :accessor force)
   (newline :initarg :newline :initform *newline-default* :accessor newline)
   (prompt :initarg :promp :initform *prompt-default* :accessor prompt)
   (memory :initarg :memory :initform nil :accessor memory)
   (style :initarg :style :initform nil :accessor perso-style)))
