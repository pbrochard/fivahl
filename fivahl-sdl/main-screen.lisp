(in-package :cl-newworld-sdl)


(defun in-first-pos (find string)
  (let ((pos (search find string)))
    (and pos (zerop pos))))


(defun add-in-frame-content-prefix (prefix frame line erase)
  (when (in-first-pos prefix line)
    (when erase
      (setf (frame-content frame) nil))
    (setf (frame-content frame) (append (frame-content frame)
					(list (subseq line (length prefix)))))
    t))



(defun add-in-frame-content (frame line)
  (setf (frame-content frame) (append (frame-content frame) (list line)))
  :update-look)


(defun string-to-list-* (str &key (split-char #\space))
  (do* ((start 0 (1+ index))
	(index (position split-char str :start start)
	       (position split-char str :start start))
	(accum nil))
      ((null index)
       (unless (string= (subseq str start) "")
	 (push (concatenate 'string "*" (subseq str start) "*") accum))
       (nreverse accum))
    (when (/= start index)
      (push (concatenate 'string "*" (subseq str start index) "*") accum))))


(defun add-command-content (prefix frame line)
  (when (in-first-pos prefix line)
    (let ((cmds (subseq line (+ (length prefix) 21))))
      (setf (frame-content frame) (string-to-list-* cmds)))
    t))


(defun update-log-content (line)
  (or (add-command-content "cmd$ " *command-frame* line)
      (add-in-frame-content-prefix "inv$ " *inventory-frame* line t)
      (add-in-frame-content-prefix "inv| " *inventory-frame* line nil)
      (add-in-frame-content-prefix "re$ " *look-frame* line t)
      (add-in-frame-content-prefix "re| " *look-frame* line nil)
      (add-in-frame-content *log-frame* line)))


(defun update-log-length ()
  (setf (frame-y-begin *log-frame*) (max (- (length (frame-content *log-frame*))
					    (round (/ (nth-value 3 (sdl:rectangle-* (frame-rect *log-frame*))) *margin-y*))
					    -2) 0)))

(defun update-log-frame ()
  (let ((update-look nil))
    (when (listen *server-sock*)
      (loop for line = (read-from *server-sock*)
	 while (not (string= line ".")) do
	   (setf update-look (update-log-content line)))
      (update-log-length)
      (when (equal update-look :update-look)
	(send-to *server-sock* "regarder")
	(send-to *server-sock* "inventaire"))
      (need-back-redraw))))





(defun valid-input ()
  (send-to *server-sock* "~A" (frame-input *input-frame*))
  (setf (frame-input *input-frame*) "")
  (need-back-redraw))



(defun treat-input-frame (item)
  (when (equal (item-frame item) *input-frame*)
    (cond ((link-equal item "Valider") (valid-input))
	  ((link-equal item "Effacer") (setf (frame-input *input-frame*) "")
	   (need-back-redraw)))
    t))



(defun treat-main-frames (item)
  (when (or (equal (item-frame item) *command-frame*)
	    (equal (item-frame item) *look-frame*)
	    (equal (item-frame item) *inventory-frame*)
	    (equal (item-frame item) *log-frame*))
    (let ((pos (position #\: (item-link item))))
      (setf (frame-input *input-frame*)
	    (format nil "~A~A " (frame-input *input-frame*)
		    (if pos
			(format nil "*~A" (subseq (item-link item) 0 pos))
			(item-link item)))
	    *current-frame* *input-frame*))
    t))

