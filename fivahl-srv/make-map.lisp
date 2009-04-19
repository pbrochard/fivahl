;;; #Date#: Thu Feb 16 22:01:53 2006

(in-package :cl-newworld)


(defun make-map ()
  (setf *monde*  (make-instance 'monde :nom "l'Univers"
				:descr "Le monde de depart"
				:serveur-sock (progn
						(format t "~%~%*** Demarrage du serveur sur le port 30000 ***~%")
						(port:open-socket-server 30000))
				:serveur-http-sock (progn
						     (format t "*** Demarrage du serveur HTTP sur le port 30001 ***~%~%")
						     (port:open-socket-server 30001))))
  (let* ((livre-1 (make-instance 'livre :nom "Le grand livre"
				 :descr "Un vieux livre tout poussiereux"
				 :contenu '("Le sort est quelque part...")))
	 (perso-1 (make-instance 'perso :nom "Tolwen"
				 :descr "Un barbare"
				 :password "toto"
				 :contenu (list livre-1)))
	 (perso-2 (make-instance 'perso :nom "Klock"
				 :descr "Un troll avec une GRANDE HACHE !!!"
				 :password 'klock
				 :contenu nil
				 :sock nil))
	 (lieu-1 (make-instance 'lieu :nom "Une place"
				:descr "Une grande place"
				:contenu (list perso-1 perso-2)))
	 (lieu-2 (make-instance 'lieu :nom "Une maison"
				:descr "Une maison en bois"))
	 (porte-1 (make-instance 'porte :nom "Une porte"
				 :descr "Une porte en bois"
				 :depart lieu-1
				 :arrivee lieu-2))
	 (porte-2 (make-instance 'porte :nom "Une porte"
				 :descr "Une porte en fer"
				 :depart lieu-2
				 :arrivee lieu-1))
	 (panneau (make-instance 'panneau :nom "Le panneau d'affichage"
				 :descr "Le grand panneau d'affichage en bois de la Grand-Place,"
				 :contenu '("Le net en France ... Black-Out !"))))
    (/set-prompt perso-1)
    (push porte-1 (contenu lieu-1))
    (push porte-2 (contenu lieu-2))
    (push lieu-1 (contenu *monde*))
    (push lieu-2 (contenu *monde*))
    (push lieu-1 (lieu-depart *monde*))
    (push lieu-2 (lieu-depart *monde*))
    (push panneau (contenu lieu-1))
    (dotimes (i 7)
      (push (make-instance 'cure-dent :nom "Cure-dent"
				  :descr "Un simple cure-dent.")
	    (contenu lieu-2)))
    (push (make-instance 'epee :nom "Une epee" :descr "Une petite epee") (contenu lieu-1))
    (push (make-instance 'pomme :nom "Une pomme" :descr "Une petite pomme pourrie") (contenu lieu-1))
    (push (make-instance 'pomme :nom "Une pomme" :descr "Une grosse pomme juteuse" :qualite 10) (contenu lieu-1))
    (send-to perso-1 "Bonjour, bienvenue dans CL-NewWorld")
    (regarder perso-1)
    (show-prompt perso-1)))
