(in-package :cl-newworld-sdl)


(defun make-default-frame (title x y w h &key input (from :top) (content nil) valid-fun)
  (make-frame :rect (sdl:rectangle :x x :y y :w w :h h)
	      :y-begin 0
	      :from from
	      :title title
	      :input input
	      :content content
	      :title-color (sdl-color #xF57900)
	      :input-color (sdl-color #x729FCF)
	      :content-color (sdl-color #x73D216)
	      :background (sdl-color #x2E3436)
	      :foreground (sdl-color #x555753)
	      :foreground-sel (sdl-color #xBABDB6)
	      :link-color (sdl-color #xFCAF3E)
	      :link-color-sel (sdl-color #xF57900)
	      :rect-link-color (sdl-color #xbabdb6)
	      :alpha 220
	      :valid-fun valid-fun))


(defun set-frame-rect (frame x y w h)
  (setf (frame-rect frame) (sdl:rectangle :x x :y y :w w :h h)))


(defun current-frame ()
  (car (last *visible-frames*)))

(defun add-in-string (frame char)
  (when (and char (frame-input frame))
    (setf (frame-input frame) (format nil "~A~C" (frame-input frame) char))
    (need-back-redraw)))

(defun del-last-char (frame)
  (when (frame-input frame)
    (setf (frame-input frame) (subseq (frame-input frame) 0 (1- (max (length (frame-input frame)) 1))))
    (need-back-redraw)))

(defun clear-frame-input (frame)
  (when (frame-input frame)
    (setf (frame-input frame) "")
    (need-back-redraw)))




(defun rotate-in-frames ()
  (setf *visible-frames* (append (last *visible-frames*) (butlast *visible-frames*))
	*current-frame* (current-frame))
  (need-back-redraw))

(defun raise-frame (frame)
  (setf *visible-frames* (append (remove frame *visible-frames*) (list frame))
	*current-frame* (current-frame))
  (need-back-redraw))



(defun up-one-line ()
  (setf (frame-y-begin *current-frame*) (max (1- (frame-y-begin *current-frame*)) 0))
  (need-back-redraw))

(defun down-one-line ()
  (setf (frame-y-begin *current-frame*) (min (1+ (frame-y-begin *current-frame*))
					     (length (frame-content *current-frame*))))
  (need-back-redraw))


(defun frame-init-pos (frame)
  (multiple-value-bind (x y w h)
      (sdl:rectangle-* (frame-rect frame))
    (declare (ignore x y w))
    (case (frame-from frame)
      (:top (round (/ *margin-y* 2)))
      (:bottom (round (- h (* *margin-y* 1.5))))
      (t *margin-y*))))



(defun add-cursor (frame)
  (if (equal frame *current-frame*) "|" ""))



(defun draw-item (item surface)
  (multiple-value-bind (rx ry)
      (sdl:rectangle-* (item-rect item))
    (sdl:draw-rectangle (item-rect item) :color (if (item-sel-p item)
						    (frame-rect-link-color (item-frame item))
						    (frame-background (item-frame item)))
			:surface surface)
    (sdl:draw-string-solid-* (item-link item) rx ry
			     :color (if (item-sel-p item)
					(frame-link-color-sel (item-frame item))
					(frame-link-color (item-frame item)))
			     :surface surface)))



(defun draw-content-line (surface x y line frame)
  (labels ((find-link (line)
	     (let* ((pos1 (position #\* line))
		    (pos2 (when pos1 (position #\* line :start (1+ pos1)))))
	       (if (and pos1 pos2)
		   (values (subseq line 0 pos1)
			   (subseq line (1+ pos1) pos2)
			   (subseq line (1+ pos2)))
		   line))))
    (multiple-value-bind (begin link end)
	(find-link line)
      (if link
	  (let* ((rx (+ x (* (length begin) *margin-x*)))
		 (ry y)
		 (rw (* (length link) *margin-x*))
		 (rh (1- *margin-y*))
		 (item (make-item :frame frame :link link
				  :rect (sdl:rectangle :x rx :y ry :w rw :h rh))))
	    (sdl:draw-string-solid-* begin x y :color (frame-content-color frame) :surface surface)
	    (push item *item-list*)
	    (draw-item item surface))
	  (sdl:draw-string-solid-* begin x y :color (frame-content-color frame) :surface surface))
      (when (and end (not (string= end "")))
	(draw-content-line surface (+ x (* (+ (length begin) (length link)) *margin-x*)) y end frame)))))



(defun draw-frame (frame surface)
  (sdl:draw-box (frame-rect frame)
		:color (frame-background frame)
		:stroke-color (if (eql *current-frame* frame)
				  (frame-foreground-sel frame)
				  (frame-foreground frame))
		:alpha (frame-alpha frame)
		:surface surface)
  (sdl:set-clip-rect (frame-rect frame) :surface surface)
  (multiple-value-bind (x y)
      (sdl:rectangle-* (frame-rect frame))
    (when (frame-title frame)
      (sdl:draw-string-solid-* (frame-title frame)
			       (+ x *margin-x*)
			       (+ y (frame-init-pos frame))
			       :color (frame-title-color frame)
			       :surface surface))
    (when (frame-input frame)
      (sdl:draw-string-solid-* (format nil "~A~A" (frame-input frame) (add-cursor frame))
			       (+ x (* *margin-x* (1+ (length (frame-title frame)))))
			       (+ y (frame-init-pos frame))
			       :color (frame-input-color frame)
			       :surface surface))
    (when (frame-content frame)
      (let ((inc (case (frame-from frame)
		   (:top *margin-y*)
		   (:bottom (- *margin-y*))
		   (t 0)))
	    (pos (frame-init-pos frame)))
	(dolist (c (subseq (frame-content frame) (frame-y-begin frame)))
	  (draw-content-line surface (+ x *margin-x*) (+ y (incf pos inc)) c frame)))))
  (sdl:clear-clip-rect surface))



(defun move-frame-to-rel (frame x y x-rel y-rel)
  (multiple-value-bind (rx ry rw rh)
      (sdl:rectangle-* (frame-rect frame))
    (when (and (<= rx x (+ rx rw))
	       (<= ry y (+ ry rh)))
      (setf (frame-rect frame)
	    (sdl:rectangle :x (+ rx x-rel) :y (+ ry y-rel) :w rw :h rh)))
    (need-back-redraw)))



(defun resize-frame-to-rel (frame x y x-rel y-rel)
  (multiple-value-bind (rx ry rw rh)
      (sdl:rectangle-* (frame-rect frame))
    (when (and (<= rx x (+ rx rw))
	       (<= ry y (+ ry rh)))
      (setf (frame-rect frame)
	    (sdl:rectangle :x rx :y ry :w (max (+ rw x-rel) 10) :h (max (+ rh y-rel) 10))))
    (need-back-redraw)))



(defun select-frame (x y)
  (let ((found-frame nil))
    (dolist (f *visible-frames*)
      (multiple-value-bind (rx ry rw rh)
	  (sdl:rectangle-* (frame-rect f))
	(when (and (<= rx x (+ rx rw))
		   (<= ry y (+ ry rh)))
	  (setf found-frame f))))
    (when found-frame
      (raise-frame found-frame))))


(defun link-equal (item link)
  (string-equal (item-link item) link))

