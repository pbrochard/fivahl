(in-package :fivahl-sdl)

(defun close-error-frame ()
  (setf *visible-frames* (remove *error-frame* *visible-frames*)
	*current-frame* (current-frame))
  (need-back-redraw))

(defun treat-error-frame (item)
  (when (equal (item-frame item) *error-frame*)
    (close-error-frame)
    t))



(defun valid-host/port ()
  (let ((port (parse-integer (frame-input *port-frame*) :junk-allowed t)))
    (handler-case
	(progn
	  (setf *server-sock* (port:open-socket (frame-input *hostname-frame*) port))
	  (setf *visible-frames* (list *look-frame* *inventory-frame* *log-frame* *command-frame* *input-frame*)
		*current-frame* (current-frame))
	  (send-to *server-sock* "aide")
	  (send-to *server-sock* "inventaire")
	  (need-back-redraw))
      (error (c)
	(display-error-frame c)))))


(defun valid-hostname ()
  (setf *visible-frames* (append (remove *port-frame* *visible-frames*) (list *port-frame*))
	*current-frame* (current-frame))
  (need-back-redraw))

(defun valid-port ()
  (setf *visible-frames* (append (remove *hostname-frame* *visible-frames*) (list *hostname-frame*))
	*current-frame* (current-frame))
  (need-back-redraw)
  (valid-host/port))


(defun treat-hostname-frame (item)
  (when (equal (item-frame item) *hostname-frame*)
    (cond ((link-equal item "Valider") (valid-host/port))
	  ((link-equal item "Effacer") (setf (frame-input *hostname-frame*) "")
	   (need-back-redraw)))
    t))


(defun treat-port-frame (item)
  (when (equal (item-frame item) *port-frame*)
    (cond ((link-equal item "Valider") (valid-host/port))
	  ((link-equal item "Effacer") (setf (frame-input *port-frame*) "0")
	   (need-back-redraw)))
    t))
