(in-package :fivahl)


(defun send-basic-page (sock host title string url-return string-return
			     &optional (only-head nil))
  (send-http sock "text/html"
	     (cstring "<!DOCTYPE html PUBLIC '-//W3C//DTD HTML 4.01//EN'
   'http://www.w3.org/TR/html4/strict.dtd'>
<html>
<head>
  <title>" title "</title>
</head>
<body>
  " string "<br>
  <a href='" url-return "'>" string-return "</a><br><br>
  <small><a href='http://" host "'>http://" host "</a></small>
</body>
</html>")
	     only-head))

(defun send-login-page (sock &optional (text "") (only-head nil))
  (send-http sock "text/html"
	     (cstring "<!DOCTYPE html PUBLIC '-//W3C//DTD HTML 4.01//EN'
   'http://www.w3.org/TR/html4/strict.dtd'>
<html>
<head>
  <title>FIVAHL: Login</title>
</head>
<body>
  <form action='' name='form' method='post' enctype='application/x-www-form-urlencoded'>
    <h1>Bienvenue dans fivahl</h1>
    <h2>" (or text "") "</h2>
    <p>Entrez votre pseudo :
      <input type='text' name='name' size='20'>
    </p>
    <p>Entez votre mot de passe :
      <input type='password' name='pass' size='20'>
    </p>
    <p>
      <input type='submit' value='Login' name='submit'>
    </p>
  </form>

  <script type='text/javascript'>
    document.form.name.focus();
  </script>
</body>
</html>")
	     only-head))

(defun do-login (sock name password only-head)
  (let ((ancien (trouve-objet (format nil "~A" name) *monde*))
	(perso nil))
    (if ancien
	(if (string-equal (password ancien) password)
	    (progn
	      (when (and (sock ancien)
			 (not (equal (sock ancien) *std-stream*)))
		(close (sock ancien)))
	      (setf (sock ancien) nil
		    (perso-style ancien) :http
		    perso ancien))
	    (send-login-page sock "Mauvais mot de passe!" only-head))
	(progn
	  (setf perso (make-instance 'perso :nom (format nil "~:(~A~)" name) :descr "..."
				     :sock nil :password password
				     :style :http))
	  (push perso (contenu (one-in (lieu-depart *monde*))))
	  (regarder perso)
	  (do-for-all-perso (perso obj)
	    (send-to obj "---> " perso " vient de rentrer.")
	    (show-prompt obj))))
    perso))



(defun encode-commands (commands)
  (with-output-to-string (str)
    (dolist (cmd commands)
      (let ((cmd (string-capitalize cmd)))
	(format str "<a href=\"javascript:ajouter('~A ')\">~A</a> | ~%" cmd cmd)))
    str))

(defun format-memory (perso)
  (with-output-to-string (str)
    (dolist (mem (memory perso))
      (format str "~A<br>~%" mem))
    str))

(defun send-result-page (sock name pass perso only-head)
  (send-http sock "text/html"
	     (cstring "<!DOCTYPE html PUBLIC '-//W3C//DTD HTML 4.01//EN'
   'http://www.w3.org/TR/html4/strict.dtd'>
<html>
<head>
  <title>FIVAHL: Login</title>

  <script type='text/javascript'>
    function ajouter(value) {
      document.input_form.action_text.value += value;
      document.input_form.action_text.focus();
    }

    function myclear() {
      document.input_form.action_text.value = '';
      document.input_form.action_text.focus();
    }

    function my_reload() {
      parent.hidden.document.input_form.action_text.value= document.input_form.action_text.value;
      document.location.reload();
    }

    function set_place() {
      window.self.location.href='#bottom';
      document.input_form.action_text.value = parent.hidden.document.input_form.action_text.value;
      document.input_form.action_text.focus();
      setTimeout ('my_reload();', 5000);
    }

    function my_logout() {
      parent.location = '/';
    }
  </script>
</head>
<body onload='javascript:set_place();'>

  <p style='text-align: right;'><a href='' onclick='javascript:my_logout();'>Deconnexion : <b>" name "</b></a></p>
  <hr>
  <p>" (format-memory perso) "</p>
  <hr>
  <p>Commandes : " (encode-commands *list-command*) "</p>
  <hr>

  <form action='' method='post' id='input_form' name='input_form' enctype='application/x-www-form-urlencoded'>
    <p>
      <input type='hidden' name='name' value='" name "'>
      <input type='hidden' name='pass' value='" pass "'>
    </p>
    <p>
      <input type='text' id='action_text' name='action_text' value='' size='80'>
      <input type='submit' value='Envoyer' name='Envoyer'>
      <input type='button'  onclick='javascript:myclear();' value='Effacer'>
    </p>
  </form>

  <hr>
  <p><a name='bottom'></a></p>
</body>
</html>")
	     only-head))

(defun send-main-page (sock name pass action-text &optional (only-head nil))
  (let ((perso (do-login sock name pass only-head)))
    (when perso
      (action-from-line perso action-text)
      (send-result-page sock name pass perso only-head))))


(defun send-frame-page (sock name password)
  (send-http sock "text/html"
	     (cstring "<!DOCTYPE html PUBLIC '-//W3C//DTD HTML 4.01//EN'
   'http://www.w3.org/TR/html4/strict.dtd'>
<html>
<head>
  <frameset rows='20%, 80%'>
    <frame src='hidden.html' name='hidden'>
    <frame src='login.html/" name "$" password "' name='login'>
  </frameset>
</head>
</html>")
	     nil))

(defun send-hidden-page (sock)
  (send-http sock "text/html"
	     (cstring "<!DOCTYPE html PUBLIC '-//W3C//DTD HTML 4.01//EN'
   'http://www.w3.org/TR/html4/strict.dtd'>
<html>
<head>
</head>
<body>
  <form action='' id='input_form' name='input_form'>
    <p>
      <input type='text' id='action_text' name='action_text' value='saved' size='80'>
    </p>
  </form>

</body>
</html>")
	     nil))

(let ((login-url-size (length "/login.html/")))
  (defun find-name-in-url (url)
    (let ((begin (search "/login.html/" url))
	  (end (position #\$ url)))
      (and begin end (string-trim " " (subseq url login-url-size end))))))

(defun find-password-in-url (url)
  (let ((begin (search "/login.html/" url))
	(end (position #\$ url)))
    (and begin end (string-trim " " (subseq url (1+ end))))))


(defun send-page (sock headers)
  (let ((host (first headers))
	(type-request (second headers))
	(url (third headers)))
    (case type-request
      ((or :get :head) (cond ((string-equal url "/") (send-login-page sock (only-head-type type-request)))
			     ((string= url "/hidden.html") (send-hidden-page sock))
			     ((search "/login.html" url) (send-main-page sock
									 (find-name-in-url url)
									 (find-password-in-url url)
									 ""))
			     (t (format t "Unknown address on GET~%")
				(send-basic-page sock host
						 "Unknown page"
						 "Sorry, this page is not on server.<br>" "" ""
						 (only-head-type type-request)))))
      (:post (cond ((string= url "/") (send-frame-page sock
						       (clean-http-content (find-http-headers headers "name"))
						       (clean-http-content (find-http-headers headers "pass"))))
		   ((search "/login.html" url)
		    (send-main-page sock
				    (clean-http-content (find-http-headers headers "name"))
				    (clean-http-content (find-http-headers headers "pass"))
				    (clean-http-content (find-http-headers headers "action_text"))))
		   (t (format t "Unknown address on POST~%")
		      (send-basic-page sock host
				       "Unknown page"
				       "Sorry, this page is not on server.<br>"
				       "" "")))))))

(defmethod check-new-http-connexion ((monde monde))
  (let ((sock (port:socket-accept (serveur-http-sock monde) :wait 0.01d0)))
    (when sock
      (init-transfer-stream)
      ;;(format t "~&******************** New Http Connexion ********************~%")
      (unwind-protect
	   (let ((headers (get-http sock)))
	     (format t "=> Reading headers : ~S~%" headers)
	     (send-page sock headers))
	(when (have-to-close-socket)
	  ;;(format t "~&Socket closed manually~%")
	  (close sock))))))

