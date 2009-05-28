;;; #Date#: Fri Feb 17 18:07:18 2006 ;;;fdskfjkdslf
;;; fdslkfmldskflds

(in-package :fivahl)


;;; Fonctions generiques

(defgeneric send-newline-to-perso (perso))
(defgeneric send-to (perso &rest args))
(defgeneric read-from (perso))
(defgeneric show-prompt (perso))

;;; Fonction voir

(create-command voir (perso objet))

(defmethod voir (perso objet)
  (declare (ignore objet))
  (send-to perso "re| il n'y a rien ici"))

(defmethod voir (perso (objet (eql nil)))
  (send-to perso "re| il n'y a vraiment rien ici"))

(defmethod voir (perso (objet objet))
  (send-to perso "re| " objet " : " (descr objet)))






;;; Le monde





(defmethod check-new-connexion ((monde monde))
  (let ((new-sock (port:socket-accept (serveur-sock monde) :wait 0.01d0)))
    (when new-sock
      (let ((new-perso (make-instance 'perso :nom "SansNom" :descr "..."
				      :sock new-sock :password nil
				      :style :telnet)))
	(push new-perso (en-attente monde))
	(send-to new-perso "Bonjour, utilisez la commande identifie <nom> <mot de passe>")
	(show-prompt new-perso)))))



(defmethod defaut ((monde monde))
  (check-new-connexion monde)
  ;;(check-new-http-connexion monde)
  (dolist (perso (en-attente monde))
    (defaut perso))
  (when (consp (contenu monde))
    (dolist (obj (contenu monde))
      (defaut obj))))


(defmethod voir (perso (monde monde))
  (send-to perso "re$ " monde " : " (descr monde))
  (dolist (c (contenu monde))
    (send-to perso "  " c)))

(defun detruit (objet)
  (let ((contenant (trouve-contenant (trouve-objet objet *monde*)
				     *monde*)))
    (setf (contenu contenant)
	(remove objet (contenu contenant)))))



;;; Un lieu

(defmethod voir (perso (lieu lieu))
  (send-to perso "re$ Vous etes dans " lieu " : " (descr lieu))
  (when (contenu lieu)
    (send-to perso "re|  Ici, il y a :")
    (loop for c in (contenu lieu) do
	  (send-to perso "re|    " c (if (equal perso c) " (Vous)" "")))))


;;; Objets


;;; Un personnage

(defmethod send-newline-to-perso ((perso perso))
  (ignore-errors
    (format (sock perso) "~A" (newline perso))
    (force-output (sock perso))))


(defun format-args (style args)
  (with-output-to-string (str)
    (dolist (a args)
      (typecase a
	(objet (if (eql style :http)
		   (format str "<a href=\"javascript:ajouter('*~A ')\">*~A:~A*</a>~%" (id a) (id a) (nom a))
		   (format str "*~A:~A*" (id a) (nom a))))
	(t (princ a str))))))

(defmethod send-to ((perso perso) &rest args)
  (ignore-errors
    (let ((str (format-args (perso-style perso) args)))
      (format (sock perso) str)
      (format (sock perso) "~A" (newline perso))
      (setf (memory perso) (forget-list (nconc (memory perso) (list str)))))
    (force-output (sock perso))))

(defmethod read-from ((perso perso))
  (ignore-errors
    (string-trim '(#\Newline #\Return) (read-line (sock perso)))))


(defmethod show-prompt ((perso perso))
  (ignore-errors
    (format (sock perso) "~A" (prompt perso))
    (force-output (sock perso))))

(defmethod clear-screen ((perso perso))
  (dotimes (i 80)
    (send-newline-to-perso perso)))







(defgeneric parse-string (perso string contenant))
(defmethod parse-string ((perso perso) string contenant)
  (unless (or (null string) (string= string ""))
    (let (elem pos)
      (let ((obj (if (eql (char string 0) #\*)
		     (progn
		       (multiple-value-setq (elem pos)
			 (read-from-string (subseq string 1)))
		       (incf pos)
		       (trouve-objet elem contenant))
		     (progn
		       (multiple-value-setq (elem pos)
			 (read-from-string string))
		       (trouve-objet (format nil "~A" elem) contenant)))))
	(cons (if (and contenant
		       (or (member obj (contenu contenant))
			   (member obj (contenu perso))))
		  obj elem)
	      (parse-string perso
			    (string-trim '(#\Space #\Newline)
					 (subseq string pos))
			    contenant))))))


(defmethod defaut ((perso perso))
  (when (and (sock perso)
	     (ignore-errors (listen (sock perso))))
    (action-from-line perso (read-from perso))))


(create-command quitter (perso))
(defmethod quitter ((perso perso))
  (send-to perso "Au revoir...")
  (if (equal (sock perso) *std-stream*)
      #+SBCL (sb-ext:quit)
      #-SBCL (ext:quit)
      (progn
	(close (sock perso))
	(setf (sock perso) nil))))

(create-command /debug (perso))
(defmethod /debug ((perso perso))
  (send-to perso "  Line = " (first *debug-log*)
	   " Command = " (second *debug-log*)
	   " Return = " (third *debug-log*)
	   " Error = " (fourth *debug-log*)))

(create-command /clean-attente (perso))
(defmethod /clean-attente ((perso perso))
  (send-to perso "Effacement de la liste d'attente :")
  (send-to perso "  Ancienne liste : " (en-attente *monde*))
  (setf (en-attente *monde*)
	(remove-if #'(lambda (obj) (null (sock obj)))
		   (en-attente *monde*)))
  (send-to perso "  Nouvelle liste : " (en-attente *monde*)))


(create-command aide (perso))
(defmethod aide ((perso perso))
  (send-to perso "cmd$ Les commandes sont : " (format nil "~:(~{~A ~}~)" *list-command*)))

(create-command aide-detail (perso))
(defmethod aide-detail ((perso perso))
  (send-to perso "Les commandes sont :")
  (dolist (i *list-command-detail*)
    (send-to perso (format nil "  ~:(~A~) ~(~{~A ~}~)" (first i) (rest (second i))))))



(defmethod voir (perso (objet perso))
  (send-to perso "re| " objet " : " (descr objet) " (HP=" (hp objet) ")"))



(create-command regarder (perso &optional objet))
(defmethod regarder ((perso perso) &optional objet)
  (voir perso (if objet
		  objet
		  (trouve-contenant perso *monde*))))

(create-command inventaire (perso))
(defmethod inventaire ((perso perso))
  (if (contenu perso)
      (progn
	(send-to perso "inv$ Vous possedez les objets suivants :")
	(dolist (obj (contenu perso))
	  (send-to perso "inv|  " obj)))
      (send-to perso "inv$ Vous ne possedez aucun objet")))

(create-command carte (perso))
(defmethod carte ((perso perso))
  (send-to perso (format nil "Vous etes : ~{~(~A~) ~^dans ~}"
			 (loop for i in (localise-objet perso *monde*)
			    collect (format nil "~A (+~A)" (nom i) (id i))))))

(create-command identifie (perso nom password))
(defmethod identifie ((perso perso) nom password)
  (when (member perso (en-attente *monde*))
    (clear-screen perso)
    (let ((ancien (trouve-objet (format nil "~A" nom) *monde*)))
      (if ancien
	  (if (equal (password ancien) password)
	      (progn
		(when (and (sock ancien)
			   (not (equal (sock ancien) *std-stream*)))
		  (close (sock ancien)))
		(setf (sock ancien) (sock perso)
		      (perso-style ancien) :telnet
		      (en-attente *monde*) (remove perso (en-attente *monde*)))
		(regarder ancien))
	      (send-to perso "Desole, mauvais mot de passe..."))
	  (progn
	    (setf (nom perso) (format nil "~:(~A~)" nom)
		  (password perso) password)
	    (push perso (contenu (one-in (lieu-depart *monde*))))
	    (setf (en-attente *monde*) (remove perso (en-attente *monde*)))
	    (regarder perso)
	    (do-for-all-perso (perso obj)
	      (send-to obj perso "---> vient de rentrer.")
	      (show-prompt obj)))))))

(create-command decrire (perso descr))
(defmethod decrire ((perso perso) descr)
  (setf (descr perso) descr)
  (voir perso perso))


(defun parler-a (a-qui origine cible message)
  (send-to a-qui " De " origine " a " cible " : "
	     (with-output-to-string (str)
				    (dolist (i message)
				      (format str "~A " (if (objet-p i)
							    (string-capitalize (nom i))
							    (string-downcase i)))))))


(create-command parler (perso a-perso &rest message))
(defmethod parler ((perso perso) (a-perso perso) &rest message)
  (parler-a perso perso a-perso message)
  (parler-a a-perso perso a-perso message)
  (show-prompt a-perso))




(create-command crier (perso &rest message))
(defmethod crier ((perso perso) &rest message)
  (do-for-all-perso (perso obj)
    (apply #'parler perso obj message)))


(defun insert-space (char prof)
  (with-output-to-string (str)
    (dotimes (i prof)
      (princ char str))))


(create-command carte-globale (perso))
(defmethod carte-globale ((perso perso))
  (let ((char (if (eql (perso-style perso) :http) "&nbsp;" " ")))
    (labels ((recmap (monde prof)
	       (if (consp (contenu monde))
		   (progn
		     (when (objet-p monde)
		       (send-to perso (insert-space char (* prof 2)) monde " : " (descr monde)))
		     (dolist (c (contenu monde))
		       (recmap c (1+ prof))))
		   (when (objet-p monde)
		     (send-to perso (insert-space char (* prof 2)) monde " : " (descr monde))))))
      (recmap *monde* 0))))




(defparameter *root-password* "1234")

(create-command /load (perso file password))
(defmethod /load ((perso perso) file (password string))
  (clear-screen perso)
  (if (string= password *root-password*)
      (progn
	(send-to perso "Je recharge " file)
	(load file))
      (send-to perso "Va te faire voir"))
  nil)


(create-command /eval (perso form password))
(defmethod /eval ((perso perso) form (password string))
  (clear-screen perso)
  (if (string= password *root-password*)
      (progn
	(send-to perso form)
	(dolist (r (multiple-value-list (ignore-errors (eval form))))
	  (send-to perso "=> " r)))
      (send-to perso "Va te faire voir"))
  nil)

(create-command /set-prompt (perso &optional type))
(defmethod /set-prompt ((perso perso) &optional type)
  (case type
    (:unix (setf (newline perso) (format nil "~%")
		 (prompt perso) (format nil ".~%")))
    (:dos (setf (newline perso) (format nil "~C~C" #\Newline #\Return)
		(prompt perso) (format nil "> ")))
    (t (setf (newline perso) (format nil "~%")
	     (prompt perso) (format nil "> "))))
  nil)





(create-command poser (perso objet))
(create-command prendre (perso objet))

(defmacro rend-prenable (type)
  `(progn
    (defmethod poser (perso (objet ,type))
      (when (member objet (contenu perso))
	(let ((contenant (trouve-contenant perso *monde*)))
	  (transfert objet (contenu perso) (contenu contenant)))
	(send-to perso "Vous posez " objet)
	(do-for-all-perso (perso obj)
	  (send-to obj "<" perso " pose " objet ">")
	  (show-prompt obj))))
    (defmethod prendre (perso (objet ,type))
      (let ((contenant (trouve-contenant perso *monde*)))
	(when (member objet (contenu contenant))
	  (transfert objet (contenu contenant) (contenu perso))
	  (send-to perso "Vous prenez " objet)
	  (do-for-all-perso (perso obj)
	    (send-to obj "<" perso " prend " objet ">")
	    (show-prompt obj)))))))



;;; Une porte
(defclass porte (objet)
  ((etat :initarg :etat :initform 'fermee :accessor etat)
   (depart :initarg :depart :initform nil :accessor depart)
   (arrivee :initarg :arrivee :initform nil :accessor arrivee)))

(defmethod voir (perso (porte porte))
  (send-to perso porte " : " (descr porte))
  (send-to perso (format nil "La porte est ~(~A~)" (etat porte))))

(create-command ouvrir (perso porte))
(defmethod ouvrir (perso (porte porte))
  (setf (etat porte) 'ouverte)
  (voir perso porte)
  (do-for-all-perso (perso obj)
    (send-to obj "<" perso " ouvre " porte ">")
    (show-prompt obj)))

(create-command fermer (perso porte))
(defmethod fermer (perso (porte porte))
  (setf (etat porte) 'fermee)
  (voir perso porte)
  (do-for-all-perso (perso obj)
    (send-to obj "<" perso " ferme " porte ">")
    (show-prompt obj)))

(create-command traverser (perso porte))
(defmethod traverser (perso (porte porte))
  (if (eql (etat porte) 'fermee)
      (progn
	(send-to perso "Oups, la porte est fermee...")
	(do-for-all-perso (perso obj)
	  (send-to obj "<Bong, " perso " se cogne dans " porte ">")
	  (show-prompt obj)))
      (progn
	(send-to perso "Vous passez la porte")
	(do-for-all-perso (perso obj)
	  (send-to obj "<--- " perso " vient de sortir.")
	  (show-prompt obj))
	(transfert perso (contenu (depart porte)) (contenu (arrivee porte)))
	(voir perso (arrivee porte))
	(do-for-all-perso (perso obj)
	  (send-to obj "---> " perso " vient de rentrer.")
	  (show-prompt obj)))))


;;; Un livre
(defclass livre (objet)
  ())

(create-command lire (perso livre))
(defmethod lire (perso (livre livre))
  (send-to perso "Vous ouvrez " (descr livre) " et vous lisez :")
  (apply #'send-to perso (contenu livre)))

(rend-prenable livre)


;;(defun parcour-monde (monde)
;;  (if (consp (contenu monde))
;;      (dolist (c (contenu monde))
;;	(parcour-monde c))
;;      (format t "~A~%" monde)))


(defun main-loop ()
  (make-map)
  (loop
   (defaut *monde*)
   (sleep 0.1)))


;;(parcour-monde *monde*)
