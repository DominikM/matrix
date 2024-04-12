(defpackage :matrix
  (:use :cl)
  (:local-nicknames (:jzon :com.inuoe.jzon)
		    (:alex :alexandria)))

(in-package :matrix)

(defvar *server* nil)

(defun start ()
  (unless *server*
    (setq *server*
	  (clack:clackup #'app
			 :server :woo
			 :port 5050))))
(defun stop ()
  (when *server*
    (clack:stop *server*)
    (setq *server* nil)))

(defun restart-server ()
  (stop)
  (start))

;;;

(defun request-method (env)
  (getf env :request-method))

(defun path-info (env)
  (getf env :path-info))

(defun raw-body (env)
  (getf env :raw-body))

(defun content-type (env)
  (getf env :content-type))

(defun content-length (env)
  (getf env :content-length))

(defun headers (env)
  (getf env :headers))

(defun query-params (env)
  (and (getf env :query-string)
       (quri:url-decode-params (getf env :query-string))))

(defun json-body (env)
  (getf env :json-body))

(defun remote-addr (env)
  (getf env :remote-addr))

;;; ROUTING

(defun app (env)
  (handle-request env))

(defun handle-request (env)
  (log-request env)
  (setf (getf env :json-body) (read-json-request env))
  (route-request env))

(defun log-request (env)
  (format t "~a at ~a from ~a ~%"
	  (request-method env)
	  (path-info env)
	  (remote-addr env)))

(defun read-json-request (env)
  (if (string= "application/json" (content-type env))
      (jzon:parse (raw-body env))
      (make-hash-table :test 'equal)))

(defun route-request (env)
  (let* ((routes (case (request-method env)
		   (:get *get-routes*)
		   (:post *post-routes*)
		   (:options *options-routes*)
		   (t '())))
	 (path (path-info env)))
    (loop for route in routes do
      (multiple-value-bind (match regs) (cl-ppcre:scan-to-strings (car route) path)
	(if match
	    (return (funcall (cdr route) regs env))))
	  finally (return (route-not-found path env)))))

;;; RESPONSES

(defvar +cors-headers+ '(:Access-Control-Allow-Origin  "*"
			 :Access-Control-Allow-Methods "GET, POST, PUT, DELETE, OPTIONS"
			 :Access-Control-Allow-Headers "X-Requested-With, Content-Type, Authorization"))

(defun text-response (content)
  `(200 (:content-type "text/plain") (,content)))

(defun json-response (alist &optional (http-code 200) (headers '()))
  (list http-code
	(append headers +cors-headers+ '(:content-type "application/json"))
	(list (jzon:stringify (alex:alist-hash-table alist)))))

(defun json-error (code message &optional (http-code 400) (headers '()))
  (json-response
   `(("errcode" . ,code)
     ("error"   . ,message))
   http-code headers))

(defun route-not-found (path env)
  (json-error :m_not_found "no path" 404))

(defun server-error (message)
  (json-error :m_unknown message 500))

;; Authorization

(defvar *auth-session* (make-hash-table :test 'equal))

(defun auth-json (env)
  (gethash "auth" (getf env :json-body)))

(defstruct auth-flow
  id
  completed
  completed-stages ;; list of completed stages
  possible-stages) ;; list of lists of possible stages

(defun registration-auth-flow (id)
  (make-auth-flow
   :id id
   :possible-stages '(("m.login.dummy"))))

(defun login-auth-flow (id)
  (make-auth-flow
   :id id
   :possible-stages '(("m.login.password"))))

(defun auth-flow-alist (flow &optional (params (make-hash-table)))
  `(("flows" .
	     ,(coerce
	       (mapcar (lambda (f) (alex:alist-hash-table `(("stages" . ,f)))) (auth-flow-possible-stages flow))
	       'vector))
    ("params" . ,params)
    ("session" . ,(auth-flow-id flow))))

(defmacro with-user-interactive-auth (env-var reg-var &rest body)
  `(or (intercept-unfinished-auth ,env-var ,reg-var)
       (progn ,@body)))

(defun intercept-unfinished-auth (env &optional reg)
  (let ((auth-json (auth-json env)))
    (if auth-json
	(let* ((session-id (gethash "session" auth-json))
	       (session    (gethash session-id *auth-session*)))
	  (continue-unfinished-auth auth-json session))
	(start-new-auth reg))))

(defun continue-unfinished-auth (auth-json session)
  (let* ((request-auth-stage (gethash "type" auth-json))
	 (expected-next      (and session (expected-next-stages session)))
	 (valid?             (member request-auth-stage expected-next :test 'equal)))
    (format t "~a ~a ~a ~%" request-auth-stage expected-next valid?)
    (if valid?
	(multiple-value-bind (success? repeat? result) (do-auth-stage request-auth-stage auth-json session)
	  (if success?
	      (push request-auth-stage (cdr (last (auth-flow-completed session)))))
	  (cond
	    ( ;; success and final stage
	     (and success? (completed-auth-flow? session))
	     (remhash (auth-flow-id session) *auth-session*)
	     nil)
	    ( ;; success and more to do
	     success?
	     (json-response (auth-flow-alist session)) 401)
	    ( ;; fail but let's try again
	     (and (not success?) repeat?)
	     (json-response (append result (auth-flow-alist session)) 401))
	    ( ;; fail and we're done here
	     (not (or success? repeat?))
	     (remhash (auth-flow-id session) *auth-session*)
	     (json-response result 400))))
	(json-error :m_unauthorized "invalid auth stage"))))

(defun do-auth-stage (auth-stage auth-json auth-flow)
  (case auth-stage
    ("m.login.dummy" (values t nil nil))))

(defun completed-auth-flow? (auth-flow)
  (member (auth-flow-completed-stages auth-flow) (auth-flow-possible-stages auth-flow)))
    
(defun expected-next-stages (auth-flow)
  (with-slots (completed-stages possible-stages) auth-flow
    (let ((remaining-stages possible-stages))
      (dolist (completed-stage completed-stages)
	(setq remaining-stages
	      (mapcar (lambda (stages)
			(if (equal (car stages) completed-stage)
			    (cdr stages)
			    nil))
		      remaining-stages)))
      (setq remaining-stages (mapcar #'car remaining-stages))
      (remove nil remaining-stages))))
      
  

(defun start-new-auth (reg)
  (let* ((session-id (write-to-string (uuid:make-v4-uuid)))
	 (this-flow (if reg
			(registration-auth-flow session-id)
			(login-auth-flow session-id))))
    (setf (gethash session-id *auth-session*) this-flow)
    (json-response (auth-flow-alist this-flow) 401)))

(defun header-access-token (env)
  (let* ((headers (headers env))
	 (header-value (gethash "Authorization" headers)))
    (cl-ppcre:register-groups-bind (access-token) ("^Bearer (.+)$" header-value)
				   access-token)))

(defun access-token (env)
  (let ((query-access-token (cdr (assoc "access_token" (query-params env) :test #'equalp)))
	(header-access-token (header-access-token (headers env))))
    (or header-access-token query-access-token)))

(defun valid-access-token? (env)
  t)

(defmacro with-authorization (env-var &rest body)
  `(let ((access-token (access-token ,env-var)))
     (if access-token
	 (if (valid-access-token? access-token)
	     (progn ,@body)
	     (json-response (error-alist :m_unknown_token "unknown token") 401))
	 (json-response (error-alist :m_missing_token "missing token") 401))))

;; API

(defparameter *get-routes*
  '(("^/.well-known/matrix/client$"            . well-known-client)
    ("^/_matrix/client/versions$"              . versions)
    ("^/_matrix/client/v3/needs_auth$"         . need-auth)
    ("^/_matrix/client/v3/login$"              . login-get)
    ("^/_matrix/client/v3/register/available$" . username-available)))

(defparameter *post-routes*
  '(("^/_matrix/client/v3/register$"   . register)
    ("^/_matrix/client/v3/login$"      . login)))

(defparameter *options-routes*
  (mapcar (lambda (route) (cons (car route) 'options)) (append *get-routes* *post-routes*)))

;;; GET

(defun well-known-client (path-arr env)
  (json-response `(("m.homeserver" . ,(alex:alist-hash-table '(("base_url" . "https://localhost.test"))))) 200))

(defun versions (path-arr env)
  (json-response '((:versions . ("v1.10" "v1.9")))))

(defun need-auth (path-arr env)
  (with-authorization env
    (json-response '((:message . "hello")) 200)))

(defun login-get (path-arr env)
  (json-response (login-flow-password)))

(defun login-flow-password ()
  `(("flows" . ,(vector (alex:alist-hash-table '(("type" . "m.login.password")))))))

(defun username-available (path-arr env)
  (let ((username (cdr (assoc "username" (query-params env) :test 'equal))))
    (cond
      ((not username)            (json-error :m_invalid_username "enter a username!!" 400))
      ((> (length username) 100) (json-error :m_invalid_username "username too long" 400))
      (t	                 (let ((user (matrix-database:get-user username)))
				   (if user
				       (json-error :m_user_in_use "pick another username!!" 400)
				       (json-response '(("available" . t)))))))))
	
    
;;; OPTIONS
(defun options (path-arr env)
  (json-response '() 200))

;;; POST
(defun register (path-arr env)
  (with-user-interactive-auth env t
    (json-response '((:message . "register")) 200)))

(defun login (path-arr env)
  (with-user-interactive-auth env nil
    (json-response '((:message . "login")) 200)))

