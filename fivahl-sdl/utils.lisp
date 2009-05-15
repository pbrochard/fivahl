(in-package :fivahl-sdl)


(defun screen-center-x() (ash *window-width* -1))
(defun screen-center-y() (ash *window-height* -1))


(defun sdl-color (color)
  (sdl:color :r (logand (ash color -16) #xFF)
	     :g (logand (ash color -8) #xFF)
	     :b (logand color #xFF)))


(defun in-rect (rect x y)
  (multiple-value-bind (rx ry rw rh)
      (sdl:rectangle-* rect)
    (and (<= rx x (+ rx rw))
	 (<= ry y (+ ry rh)))))

(defun find-front-frame (x y)
  (let ((front-frame nil))
    (dolist (frame *visible-frames*)
      (when (in-rect (frame-rect frame) x y)
	(setf front-frame frame)))
    front-frame))





(defun display-error-frame (c)
  (setf *visible-frames* (list *port-frame* *hostname-frame* *error-frame*)
	(frame-input *input-frame*) ""
	(frame-content *look-frame*) nil
	(frame-content *inventory-frame*) nil
	(frame-content *log-frame*) nil
	(frame-content *command-frame*) nil
	*current-frame* (current-frame)
	(frame-content *error-frame*) (list ""
					    (format nil "Serveur : ~A   Port : ~A" (frame-input *hostname-frame*)
						    (frame-input *port-frame*))
					    ""
					    (format nil "~A" c)
					    ""
					    "   *Fermer*"))
  (need-back-redraw))


(defun send-to (sock formatter &rest args)
  (handler-case
      (progn
	(apply #'format sock formatter args)
	(format sock "~c" #\linefeed)
	(force-output sock))
    (error (c)
      (display-error-frame c))))



(defun read-from (sock)
  (handler-case
      (string-trim '(#\newline #\linefeed #\return) (read-line sock nil nil))
    (error (c)
      (display-error-frame c))))






(defun draw-string-name (name rect)
  (multiple-value-bind (x y) (sdl:rectangle-* rect)
    (sdl:draw-string-shaded-* name
			      (+ x 2)
			      (+ y 2)
			      sdl:*green* sdl:*black* :font *font*)))

(defun draw-strings (strings rect)
  (sdl:with-surface (surface sdl:*default-display*)
    (sdl:set-clip-rect rect)
    (multiple-value-bind (x y) (sdl:rectangle-* rect)
      (loop for line in strings
	 for i from 0 do
	   (sdl:draw-string-solid-* line
				    (+ x *margin-x*)
				    ;;(+ y (* i *margin-y*) *margin-y* 2)
				    (+ y (* i *margin-y*))
				    :color sdl:*white* :font *font*)))
    (sdl:clear-clip-rect)))




(defun set-size (width height fullscreen-p)
  (when (and width height)
    (setf *window-width* width
	  *window-height* height))
  (setf *fulscreen-mode* fullscreen-p
	*command-rect* (sdl:rectangle :x 0 :y 0 :w (round (/ *window-width* 5)) :h *window-height*)
	*inventory-rect* (sdl:rectangle :x (- *window-width* (round (/ *window-width* 5)))
					:y 0 :w (round (/ *window-width* 5)) :h *window-height*)
	*view-rect* (sdl:rectangle :x (round (/ *window-width* 5))
				   :y 0
				   :w (round (* *window-width* 3/5))
				   :h (round (/ *window-height* 2)))
	*log-rect* (sdl:rectangle :x (round (/ *window-width* 5))
				  :y (round (/ *window-height* 2))
				  :w (round (* *window-width* 3/5))
				  :h (round (/ *window-height* 2)))))


(defun need-redraw ()
  (setf *redraw-needed* t))

(defmacro with-redraw (() &body body)
  `(when *redraw-needed*
     ,@body
     (setf *redraw-needed* nil)))


(defun need-back-redraw ()
  (setf *back-redraw-needed* t
	*redraw-needed* t))

(defmacro with-back-redraw (() &body body)
  `(when *back-redraw-needed*
     ,@body
     (setf *back-redraw-needed* nil)))


(defun copy-back-surface ()
  (sdl:blit-surface *back-surface*))



(defun sdl-control-press ()
  (or (sdl:get-key-state :sdl-key-lctrl)
      (sdl:get-key-state :sdl-key-rctrl)))
