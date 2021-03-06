;;; #Date#: Thu Jan 12 21:01:34 2006 test
;; Test de Baffan
(in-package :fivahl)

;;; Definition d'une epee

(defclass epee (objet-prenable objet-tapant)
  ())



(defmethod taper ((perso perso) (epee epee) (cible perso))
  (let ((degat (+ (attaque epee) (force perso))))
    (decf (hp cible) degat)
    (send-to perso "Vous tapez sur " cible " avec " degat " (HP=" (hp cible) ")")
    (send-to cible perso " vous tape dessus avec " degat " (HP=" (hp cible) ")")
    (show-prompt cible)))

; (rend-prenable epee)

;;; Definition d'une pomme
(defclass pomme (objet-prenable)
  ((qualite :initarg :qualite :initform 1 :accessor qualite)))

(create-command manger (perso objet))

(defmethod manger ((perso perso) (pomme pomme))
  (incf (hp perso) (qualite pomme))
  (detruit pomme)
  (send-to perso "La pomme etait tres bonne (HP=" (hp perso) ")"))

; (rend-prenable pomme)


(defmethod manger ((perso perso) (m-perso perso))
  (unless (equal perso m-perso)
    (incf (hp perso) 20))
  (decf (hp m-perso) 20)
  (send-to perso m-perso " etait tres bon (HP=" (hp perso) ")")
  (send-to m-perso perso " est en train de vous manger (HP=" (hp m-perso) ")")
  (show-prompt m-perso))


  
(defclass panneau (objet)
  ())

(defmethod lire ((perso perso) (panneau panneau))
  (send-to perso "Sur le panneau est ecrit :")
  (apply #'send-to perso (contenu panneau)))

(defclass cure-dent (objet-prenable objet-tapant)
  ())
  
(defmethod manger ((perso perso) (cure-dent cure-dent))
  (decf (hp perso) 13)
  (send-to perso "Vous vous étranglez avec le cure-dent !
(HP=" (hp perso) ")"))
  
; (defmethod taper ((perso perso) (cure-dent cure-dent) (cible perso))
  ; (let ((degat (+ (attaque cure-dent) (force perso))))
    ; (decf (hp cible) degat)
    ; (send-to perso "Vous tapez sur " cible " avec " degat " (HP=" (hp cible) ")")
    ; (send-to cible perso " vous tape dessus avec " degat " (HP=" (hp cible) ")")
    ; (show-prompt cible)))

; (rend-prenable cure-dent)

; (defmacro rend-tapant (objet type)
	; `(defmethod taper ((perso perso) (,objet ,type) (cible perso))
	   ; (let ((degat (+ (attaque ,objet) (force perso))))
	    ; (decf (hp cible) degat)
		; (send-to perso "Vous tapez sur " cible " avec " degat " (HP=" (hp cible) ")")
		; (send-to cible perso " vous tape dessus avec " degat " (HP=" (hp cible) ")")
		; (show-prompt cible))))		
; (rend-tapant cd cure-dent)

(create-command taper (perso objet cible))

(defmethod taper ((perso perso) (objet objet-tapant) (cible perso))
	(let ((degat (+ (attaque objet) (force perso))))
	    (decf (hp cible) degat)
		(send-to perso "Vous tapez sur " cible " avec " degat " (HP=" (hp cible) ")")
		(send-to cible perso " vous tape dessus avec " degat " (HP=" (hp cible) ")")
		(show-prompt cible)))