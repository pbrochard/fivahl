(in-package :fivahl)


(defun one-in (elems)
  (if (consp elems)
      (nth (random (length elems)) elems)
      elems))

(defmacro transfert (obj orig dest)
  `(when (member ,obj ,orig)
    (setf ,orig (remove ,obj ,orig))
    (setf ,dest (cons ,obj ,dest))))


;;; Definition des objets en general

(defgeneric objet-p (objet))
(defgeneric defaut (objet))
(defgeneric trouve-contenant (objet contenant))
(defgeneric trouve-objet (objet contenant))
(defgeneric localise-objet (objet contenant))

(defmethod objet-p ((objet objet))
  objet)

(defmethod objet-p (objet)
  (declare (ignore objet))
  nil)

(defmethod defaut ((objet objet))
  (when (consp (contenu objet))
    (dolist (obj (contenu objet))
      (defaut obj))))

(defmethod defaut (objet)
  (declare (ignore objet)))

(defmethod contenu (objet)
  (declare (ignore objet)))
(defmethod id (objet)
  (declare (ignore objet)))
(defmethod nom (objet)
  (declare (ignore objet)))
(defmethod descr (objet)
  (declare (ignore objet)))

(defun id-member (id list)
  (dolist (i list)
    (when (and (objet-p i) (= id (id i)))
      (return-from id-member t))))

(defun nom-member (nom list)
  (dolist (i list)
    (when (and (objet-p i) (string-equal nom (nom i)))
      (return-from nom-member t))))


(defmacro create-trouve-contenant (type member-fun)
  `(defmethod trouve-contenant ((obj ,type) contenant)
    (cond ((objet-p contenant)
	   (cond ((,member-fun obj (contenu contenant)) contenant)
		 ((consp (contenu contenant))
		  (or (trouve-contenant obj (car (contenu contenant)))
		      (trouve-contenant obj (cdr (contenu contenant)))))))
	  ((consp contenant)
	   (or (trouve-contenant obj (car contenant))
	       (trouve-contenant obj (cdr contenant)))))))



(create-trouve-contenant objet member)
(create-trouve-contenant integer id-member)
(create-trouve-contenant string nom-member)



(defmacro create-trouve-objet (type test field)
  `(defmethod trouve-objet ((obj ,type) contenant)
    (cond ((objet-p contenant)
	   (if (,test obj (,field contenant))
	       contenant
	       (when (consp (contenu contenant))
		 (or (trouve-objet obj (car (contenu contenant)))
		     (trouve-objet obj (cdr (contenu contenant)))))))
	  ((consp contenant)
	   (or (trouve-objet obj (car contenant))
	       (trouve-objet obj (cdr contenant)))))))


(create-trouve-objet objet equal identity)
(create-trouve-objet integer = id)
(create-trouve-objet string string-equal nom)


(defmethod localise-objet (objet (contenant objet))
  (cons (trouve-objet objet contenant)
	(unless (equal objet contenant)
	  (localise-objet (trouve-contenant objet contenant) contenant))))


;;; TEST
(defparameter *test* '(1 2 (3 4 toto titi (10 azerty) klmpoio) "toto" "tlpop" toto 5 6))

(defun collect-all-object (type monde)
  (let ((acc nil))
    (labels ((rec (monde)
	       (if (consp monde)
		   (progn
		     (rec (car monde))
		     (rec (cdr monde)))
		   (when (equal (class-of monde) (find-class type))
		     (push monde acc)))))
      (rec monde))
    (nreverse acc)))





(defgeneric perso-p (perso))
(defmethod perso-p ((perso perso))
  perso)
(defmethod perso-p (perso)
  (declare (ignore perso))
  nil)


(defmacro do-for-all-perso ((perso obj) &body body)
  `(dolist (,obj (contenu (trouve-contenant ,perso *monde*)))
    (when (and (perso-p ,obj)
	       (not (eql ,obj ,perso)))
      ,@body)))


(defun action-from-line (perso line)
  (unless (or (null line) (string= line ""))
    (let* ((line (ignore-errors
		   (parse-string perso line
				 (trouve-contenant perso *monde*))))
	   (cmd (find-command (first line))))
      (if (= (length cmd) 1)
	  (multiple-value-bind (ret err)
	      (ignore-errors
		(apply (first cmd) perso (rest line)))
	    (when (or ret err)
	      (send-to perso "Huh, je ne sais pas faire ca...")
	      (setf *debug-log* (list line cmd ret err))))
	  (if (null cmd)
	      (send-to perso "Huh, je ne sais pas faire ca...")
	      (send-to perso "Que voulez vous faire ? " cmd))))
    (show-prompt perso)))


(defmacro create-command (name &rest args)
  `(progn
    (pushnew ',name *list-command*)
    (pushnew '(,name ,@args) *list-command-detail*)
    (setf *list-command*
     (sort (copy-list *list-command*)
      #'(lambda (x y)
	  (if (eql (char (symbol-name x) 0) #\/)
	      (if (eql (char (symbol-name y) 0) #\/)
		  (string< (symbol-name x) (symbol-name y))
		  nil)
	      (if (eql (char (symbol-name y) 0) #\/)
		  t
		  (string< (symbol-name x) (symbol-name y)))))))
    (setf *list-command-detail*
     (sort (copy-list *list-command-detail*)
      #'(lambda (x y)
	  (if (eql (char (symbol-name (first x)) 0) #\/)
	      (if (eql (char (symbol-name (first y)) 0) #\/)
		  (string< (symbol-name (first x)) (symbol-name (first y)))
		  nil)
	      (if (eql (char (symbol-name (first y)) 0) #\/)
		  t
		  (string< (symbol-name (first x)) (symbol-name (first y))))))))
    (defgeneric ,name ,@args)))



(defun find-command (cmd)
  (when (symbolp cmd)
    (loop for i in *list-command*
	  when (string= (string-upcase (symbol-name i))
			(string-upcase (symbol-name cmd)))
	  do (return-from find-command (list i)))
    (loop for i in *list-command*
	  with len = (length (symbol-name cmd))
	  as f = (string>= (string-upcase (symbol-name i))
			   (string-upcase (symbol-name cmd)))
	  when (and f (>= f len)) collect i)))

