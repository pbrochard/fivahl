(in-package :cl-newworld)

(defparameter *hide-url* t)

(unless *hide-url*
  (defun rot13 (str)
    str))


(defparameter *rot13-javascript*
  (if *hide-url*
      "   // This script hereby is dedicated in the Public Domain
    // as long as nobody else claims the copyright for it.
    // origin: 2000-01-08 nospam@geht.net http://tools.geht.net/rot13.html
    // Use at own risk.
    var last='';
    var rot13map;

    // The problem is that JavaScript 1.0
    // does not provide a Char to Numeric value conversion
    // Thus we define a map.
    // Because there are 64K UniCode characters, this map does not cover all characters.
    function rot13init()
    {
      var map = new Array();
      var s   = 'abcdefghijklmnopqrstuvwxyz';

      for (i=0; i<s.length; i++)
        map[s.charAt(i)]			= s.charAt((i+13)%26);
      for (i=0; i<s.length; i++)
        map[s.charAt(i).toUpperCase()]	= s.charAt((i+13)%26).toUpperCase();
      return map;
    }

    function rot13(a)
    {
      if (!rot13map)
        rot13map=rot13init();
      s = '';
      for (i=0; i<a.length; i++)
        {
          var b = a.charAt(i);

          s += (b>='A' && b<='Z' || b>='a' && b<='z' ? rot13map[b] : (b == ' ' ? '&': b));
        }
      return s;
    }"
      "function rot13 (a) { return a; }"))


(defparameter *submitenter*
  "    function submitenter(myfield,e)
    {
    var keycode;
    if (window.event) keycode = window.event.keyCode;
    else if (e) keycode = e.which;
    else return true;

    if (keycode == 13) { my_valid(); return false; }
      else return true;
    }")


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
  <p>" string "</p>
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
  <meta http-equiv='Content-Type' content='text/html; charset=iso-8859-1'>
  <title>CL-NEWWORLD: Login</title>
</head>
<body>
  <script type='text/javascript'><!--
" *rot13-javascript* "
    function my_valid()
    {
      name = document.my_form.name.value;
      password = document.my_form.pass.value;

      document.location = rot13(name + '-' + password + '^$');
    }

" *submitenter* "
  --></script>

  <form action='' name='my_form' method=''>
    <h1>Bienvenue dans cl-newworld</h1>
    <h2>" (if text text "") "</h2>
    <p>Entrez votre pseudo :
      <input type='text' name='name' size='20' onKeyPress='return submitenter(this,event)'>
    </p>
    <p>Entez votre mot de passe :
      <input type='password' name='pass' size='20' onKeyPress='return submitenter(this,event)'>
    </p>
    <p>
      <input type='button' value='Login' name='Login' onclick='javascript:my_valid();'>
    </p>
  </form>

  <script type='text/javascript'>
    document.my_form.name.focus();
  </script>
</body>
</html>")
	     only-head))

(defun do-login (sock name password only-head)
  (declare (ignore sock only-head))
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
		    perso ancien)))
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



(defun send-result-page (sock name password perso last-text only-head)
  (setf last-text (substitute #\Space #\+ last-text))
  ;;(dbg name password perso last-text)
  (send-http sock "text/html"
	     (cstring "<!DOCTYPE html PUBLIC '-//W3C//DTD HTML 4.01//EN'
   'http://www.w3.org/TR/html4/strict.dtd'>
<html>
<head>
  <meta http-equiv='Content-Type' content='text/html; charset=iso-8859-1'>
  <title>CL-NEWWORLD</title>
</head>
<body onload='javascript:set_place();'>
  <script type='text/javascript'><!--
" *rot13-javascript* "
    function my_valid()
    {
      action_text = document.my_form.action_text.value;
      document.my_form.action_text.value = '';
"
(cstring "document.location = rot13('" name "' + '-' + '" password "' + '^$' + action_text);")
"
    }

" *submitenter* "

    function ajouter(value)
    {
      document.my_form.action_text.value += value;
      document.my_form.action_text.focus();
    }

    function myclear()
    {
      document.my_form.action_text.value = '';
      document.my_form.action_text.focus();
    }

    function my_reload ()
    {
      text_value = document.my_form.action_text.value;"
(cstring "document.location = rot13('" name "' + '-' + '" password "' + '^' + text_value + '$');")
"    }

    function set_place()
    {
      window.self.location.href='#bottom';
      document.my_form.action_text.focus();
      setTimeout('my_reload()', 5000)
    }

  --></script>

  <p style='text-align: right;'><b>Login : " name "</b> | <a href='/'>logout</a></p>
  <hr>
  <p>" (format-memory perso) "</p>
  <hr>
  <p>Commandes : " (encode-commands *list-command*) "</p>
  <hr>

  <form action='' name='my_form' method=''>
    <p>
      <input type='text' name='action_text' value='"
last-text
"' size='80' onKeyPress='return submitenter(this,event)'>
      <input type='button' value='Action' name='Action' onclick='javascript:my_valid();'>
      <input type='button' onclick='javascript:myclear();' value='Effacer'>
    </p>
  </form>

  <script type='text/javascript'>
    document.my_form.action_text.focus();
  </script>
  <hr>
  <p><a name='bottom'></a></p>
</body>
</html>")
	     only-head))

(defun decode-url (url)
  "Return: name, password and action-text"
  (dbg url)
  (let* ((decoded-url (rot13 (substitute #\+ #\Space (clean-http-content url))))
	 (pos-minus (position #\- decoded-url))
	 (pos-circ (position #\^ decoded-url))
	 (pos-dollar (position #\$ decoded-url)))
    (values (and pos-minus
		 (subseq decoded-url 1 pos-minus))
	    (and pos-minus pos-circ
		 (subseq decoded-url (1+ pos-minus) pos-circ))
	    (and pos-circ pos-dollar
		 (subseq decoded-url (1+ pos-circ) pos-dollar))
	    (and pos-dollar
		 (subseq decoded-url (1+ pos-dollar))))))


(defun valid-url (sock url only-head)
  (multiple-value-bind
	(name password last-text action-text)
      (decode-url url)
    (let ((perso (do-login sock name password only-head)))
      (when perso
	;;(dbg perso name password last-text action-text)
	(action-from-line perso (substitute #\Space #\+ action-text))
	(send-result-page sock name password perso last-text only-head)
	t))))





(defun send-page (sock headers)
  (let ((host (first headers))
	(type-request (second headers))
	(url (third headers)))
    (case type-request
      ((or :get :head) (cond ((string-equal url "/") (send-login-page sock (only-head-type type-request)))
			     ((valid-url sock url (only-head-type type-request)))
			     (t (format t "Unknown address on GET~%")
				(send-basic-page sock host
						 "Page inconnue"
						 "<p>Cette page n'est pas sur le serveur.</p>
<p><b>Peut-être un mauvais mot de passe...</b></p>" "" ""
						 (only-head-type type-request)))))
      (:post (send-basic-page sock host
			      "Unknown page"
			      "Sorry, this page is not on the server.<br>"
			      "" "")))))



(defun fn-end-multipart (sock host)
  (send-basic-page sock host
		   "File uploaded"
		   "Your share file have well been uploaded:<br><br>"
		   (format nil "http://~A" host)
		   "Back to main page"))

(defun fn-end-extract-content (filename)
  (format t "~&File saved from client: ~A~&" filename))


(defmethod check-new-http-connexion ((monde monde))
  (let ((sock (port:socket-accept (serveur-http-sock monde) :wait 0.01d0)))
    (when sock
      (init-transfer-stream)
      ;;(format t "~&******************** New Http Connexion ********************~%")
      (unwind-protect
	   (let ((headers (get-http sock :tempfile "temp.tmp" :savedir ""
				    :fn-end-multipart #'fn-end-multipart
				    :fn-end-extract-content #'fn-end-extract-content)))
	     ;;(format t "=> Reading headers : ~S~%" headers)
	     (send-page sock headers))
	(when (have-to-close-socket)
	  ;;(format t "~&Socket closed manually~%")
	  (close sock))))))

