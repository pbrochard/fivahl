(format t "toto ~A~%" 'toto)

(defun cstring (&rest args)
  (format nil "~{~A~}" args))

(princ (cstring "toto titi
plop ploop"))