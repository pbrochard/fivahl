;;; #Date#: Tue Jan  3 14:34:38 2006
;; test hebi

(in-package :fivahl)

(create-command plop (perso))
(defmethod plop ((perso perso))
  (send-to perso "Coucou, comment vas tu ?"))
