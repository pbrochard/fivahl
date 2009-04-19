(in-package :cl-newworld-sdl)

(defparameter *button-press* nil)


(defun create-main-window ()
  (setf *button-press* nil)
  (sdl:window *window-width* *window-height*
	      :title-caption "cl-newworld-sdl"
	      :icon-caption "cl-newworld-sdl"
	      :flags (if *fulscreen-mode* '(sdl:sdl-fullscreen sdl:sdl-hw-surface sdl:sdl-doublebuf) nil)))

(defun create-back-surface ()
  (setf *back-surface* (sdl:create-surface *window-width* *window-height*)))



(defun create-all-frames ()
  (setf *hostname-frame* (make-default-frame "Adresse du serveur : " 10 50 780 50 :input "localhost"
					     :content `("*Valider*  *Effacer*")
					     :valid-fun #'valid-hostname)
	*port-frame* (make-default-frame "Port du serveur : " 10 120 780 50 :input "30000"
					 :valid-fun #'valid-port
					 :content '("*Valider*  *Effacer*"))
	*error-frame* (make-default-frame "Erreur : " 50 60 700 400
					  :content '("Erreur")
					  :valid-fun #'close-error-frame)
	*help-frame* (make-default-frame "Aide : " 100 100 600 400
					 :content (help-content))
	*input-frame* (make-default-frame "Action : " 10 10 780 40
					  :input ""
					  :content '("*Effacer*  *Valider*")
					  :valid-fun #'valid-input)
	*command-frame* (make-default-frame "Commandes : " 10 60 150 530)
	*inventory-frame* (make-default-frame "Inventaire : " 640 60 150 530)
	*look-frame* (make-default-frame "Regard : " 170 60 460 250)
	*log-frame* (make-default-frame "Log : " 170 320 460 270)
	*visible-frames* (list *port-frame* *hostname-frame*)
	*current-frame* (current-frame)))




(defun set-default-frames-pos ()
  (set-frame-rect *hostname-frame* 10 50 780 50)
  (set-frame-rect *port-frame* 10 120 780 50)
  (set-frame-rect *error-frame* 50 60 700 400)
  (set-frame-rect *help-frame*  100 100 600 400)
  (set-frame-rect *input-frame* 10 10 780 40)
  (set-frame-rect *command-frame* 10 60 150 530)
  (set-frame-rect *inventory-frame* 640 60 150 530)
  (set-frame-rect *look-frame* 170 60 460 250)
  (set-frame-rect *log-frame*  170 320 460 270)
  (need-back-redraw))


(defun adjust-frames-pos ()
  (let ((x-size (round (/ *window-width* 5)))
	(y-size (round (/ *window-height* 2))))
    (set-frame-rect *hostname-frame* 10 50 (- *window-width* 20) 50)
    (set-frame-rect *port-frame* 10 120 (- *window-width* 20) 50)
    (set-frame-rect *error-frame* 50 60 (- *window-width* 100) (- *window-height* 120))
    (set-frame-rect *help-frame*  100 100 (- *window-width* 200) (- *window-height* 200))
    (set-frame-rect *input-frame* 10 10 (- *window-width* 20) 40)
    (set-frame-rect *command-frame* 10 60 x-size (- *window-height* 70))
    (set-frame-rect *inventory-frame* (- (* 4 x-size) 10) 60 x-size (- *window-height* 70))
    (set-frame-rect *look-frame* (+ x-size 20) 60 (- (* x-size 3) 40) (- y-size 70))
    (set-frame-rect *log-frame*  (+ x-size 20) y-size (- (* x-size 3) 40) (- y-size 10))
    (need-back-redraw)))







(defun swap-fullscreen ()
  (setf *fulscreen-mode* (not *fulscreen-mode*))
  (create-main-window)
  (need-back-redraw))

(defun change-resolution ()
  (setf *current-resolution* (cdr *current-resolution*))
  (unless *current-resolution*
    (setf *current-resolution* *all-resolution*))
  (set-size (first (first *current-resolution*))
	    (second (first *current-resolution*))
	    *fulscreen-mode*)
  (create-main-window)
  (need-back-redraw))


(defun toggle-display-help ()
  (setf *visible-frames*
	(if (member *help-frame* *visible-frames*)
	    (remove *help-frame* *visible-frames*)
	    (append *visible-frames* (list *help-frame*)))
	*current-frame* (current-frame))
  (need-back-redraw))


(defun treat-keys (key char)
  (cond ((and (or (sdl:key= key :sdl-key-q) (sdl:key= key :sdl-key-x))
	      (sdl-control-press))
	 (sdl:push-quit-event))
	((sdl:key= key :sdl-key-F1) (toggle-display-help))
	((sdl:key= key :sdl-key-F2) (set-default-frames-pos))
	((sdl:key= key :sdl-key-F3) (adjust-frames-pos))
	((sdl:key= key :sdl-key-f11) (swap-fullscreen))
	((sdl:key= key :sdl-key-f12) (change-resolution))
	((or (sdl:key= key :sdl-key-backspace)
	     (sdl:key= key :sdl-key-delete)
	     (sdl:key= key :sdl-key-left))
	 (del-last-char *current-frame*))
	((and char (char= char #\Return))
	 (nfuncall (frame-valid-fun *current-frame*)))
	((sdl:key= key :sdl-key-tab) (rotate-in-frames))
	((sdl:key= key :sdl-key-up) (up-one-line))
	((sdl:key= key :sdl-key-down) (down-one-line))
	((sdl:key= key :sdl-key-escape)
	 (clear-frame-input *current-frame*))
	(t (when char
	     (add-in-string *current-frame* char)))))



(defun action-on-item (x y)
  (let ((front-frame (find-front-frame x y)))
    (dolist (item *item-list*)
      (when (and (item-sel-p item)
		 (equal (item-frame item) front-frame)
		 (in-rect (item-rect item) x y))
	(or (treat-error-frame item)
	    (treat-hostname-frame item)
	    (treat-port-frame item)
	    (treat-input-frame item)
	    (treat-main-frames item))))))


(defun treat-buttons (button x y)
  (case button
    ((or 1 2 3) (select-frame x y)
     (unless (sdl-control-press)
       (action-on-item x y)))
    (4 (up-one-line))
    (5 (down-one-line))))


;;(defun treat-motion (x y x-rel y-rel)
;;  (if (sdl-control-press)
;;      (cond ((member 1 *button-press*) (move-frame-to-rel *current-frame* x y x-rel y-rel))
;;	    ((member 3 *button-press*) (resize-frame-to-rel *current-frame* x y x-rel y-rel)))
;;      (when (and (< x-rel 5) (< y-rel 5))
;;	(dolist (item *item-list*)
;;	  (multiple-value-bind (rx ry rw rh)
;;	      (sdl:rectangle-* (item-rect item))
;;	    (if (and (<= rx x (+ rx rw))
;;		     (<= ry y (+ ry rh)))
;;		(unless (item-sel-p item)
;;		  (if (equal (item-frame item) *current-frame*)
;;		      (draw-item item *back-surface* rx ry)
;;		      (need-redraw)))
;;		(when (item-sel-p item)
;;		  (if (equal (item-frame item) *current-frame*)
;;		      (draw-item item *back-surface* rx ry)
;;		      (need-redraw)))))))))



(defun treat-motion (x y x-rel y-rel)
  (if (sdl-control-press)
      (cond ((member 1 *button-press*) (move-frame-to-rel *current-frame* x y x-rel y-rel))
	    ((member 3 *button-press*) (resize-frame-to-rel *current-frame* x y x-rel y-rel)))
      (let ((front-frame (find-front-frame x y)))
	(dolist (item *item-list*)
	  (multiple-value-bind (rx ry rw rh)
	      (sdl:rectangle-* (item-rect item))
	    (if (and (equal front-frame (item-frame item))
		     (<= rx x (+ rx rw))
		     (<= ry y (+ ry rh)))
		(unless (or (equal item *current-item*) (item-sel-p item))
		  (setf *current-item* item
			(item-sel-p item) t)
		  (need-redraw))
		(when (item-sel-p item)
		  (when (equal item *current-item*)
		    (setf *current-item* nil))
		  (setf (item-sel-p item) nil)
		  (need-redraw))))))))





;;      (when (and (< x-rel 5) (< y-rel 5))
;;	(dolist (item *item-list*)
;;	  (multiple-value-bind (rx ry rw rh)
;;	      (sdl:rectangle-* (item-rect item))
;;	    (if (and (<= rx x (+ rx rw))
;;		     (<= ry y (+ ry rh)))
;;		(unless (item-sel-p item)
;;		  (if (equal (item-frame item) *current-frame*)
;;		      (draw-item item *back-surface* rx ry)
;;		      (need-redraw)))
;;		(when (item-sel-p item)
;;		  (if (equal (item-frame item) *current-frame*)
;;		      (draw-item item *back-surface* rx ry)
;;		      (need-redraw)))))))))




;;(defun draw-screen (mouse-x mouse-y)
;;  (when (member *log-frame* *visible-frames*)
;;    (update-log-frame))
;;  (with-redraw ()
;;    (sdl:clear-display (sdl:color))
;;    (setf *item-list* nil)
;;    (dolist (frame *visible-frames*)
;;      (draw-frame frame mouse-x mouse-y))))

(defun draw-back-screen ()
  (sdl:clear-display (sdl:color) :surface *back-surface*)
  (setf *item-list* nil
	*current-item* nil)
  (dolist (frame *visible-frames*)
    (draw-frame frame *back-surface*)))

(defun draw-screen ()
  (when (member *log-frame* *visible-frames*)
    (update-log-frame))
  (with-back-redraw ()
    (draw-back-screen))
  (with-redraw ()
    (copy-back-surface)
    (when *current-item*
      (sdl:with-surface (surface sdl:*default-display*)
	(draw-item *current-item* sdl:*default-surface*)))))






(defun main-loop (&optional width height fullscreen-p)
  (setf *current-resolution* (list (list width height)))
  (set-size width height fullscreen-p)
  (sdl:with-init ()
    (create-main-window)
    (create-back-surface)
    (create-all-frames)
    (setf (sdl:frame-rate) 60)
    (sdl:enable-key-repeat nil nil)
    (sdl:enable-unicode t)
    (setf *font* (sdl:initialise-default-font *font-name*)
	  *current-item* nil)
    (need-back-redraw)
    (need-redraw)
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (sdl:update-display))
      (:key-down-event (:key key :unicode unicode)
		       (treat-keys key (if (zerop unicode) nil (code-char unicode))))
      (:mouse-button-down-event (:button button :x x :y y)
				(treat-buttons button x y)
				(pushnew button *button-press*))
      (:mouse-button-up-event (:button button)
			      (setf *button-press* (remove button *button-press*)))
      (:mouse-motion-event (:x x :y y :x-rel x-rel :y-rel y-rel)
			   (treat-motion x y x-rel y-rel))
      (:idle ()
	     (draw-screen)
	     (sdl:update-display)))))


