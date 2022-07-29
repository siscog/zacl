;;;; package-socket.lisp

(in-package #:zacl)

(defparameter *print-hostname-in-stream* nil)

(defun ip-address-integer (ip-address)
  (check-type ip-address (simple-array * (4)) "octet vector")
  (logand #xFFFFFFFF
          (logior (ash (aref ip-address 0) 24)
                  (ash (aref ip-address 1) 16)
                  (ash (aref ip-address 2)  8)
                  (ash (aref ip-address 3)  0))))

(defun integer-ip-address (integer)
  (check-type integer (unsigned-byte 32))
  (let ((ip-address (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (aref ip-address 0) (ldb (byte 8 24) integer)
          (aref ip-address 1) (ldb (byte 8 16) integer)
          (aref ip-address 2) (ldb (byte 8  8) integer)
          (aref ip-address 3) (ldb (byte 8  0) integer))
    ip-address))


(defclass zacl-socket (fundamental-binary-output-stream
                       fundamental-character-output-stream
                       fundamental-binary-input-stream
                       fundamental-character-input-stream)
  ((socket
    :initarg :socket
    :initform nil
    :reader socket)
   (real-stream
    :initarg :real-stream
    :initform nil
    :accessor real-stream
    :reader underlying-input-stream
    :reader underlying-output-stream)
   (bytes-written
    :initarg :bytes-written
    :initform 0
    :accessor bytes-written)
   (external-format
    :initarg :external-format
    :initform :latin-1
    :reader socket-ef
    :accessor zacl-cl:stream-external-format)
   #+sbcl
   (read-timeout
    :accessor read-timeout
    :initform nil)
   #+sbcl
   (write-timeout
    :accessor write-timeout
    :initform nil)))

#+sbcl
(defun call-with-deadline-maybe (identifier seconds body-fn)
  (handler-bind
      ((sb-sys:deadline-timeout
	 (lambda (e)
	   (declare (ignore e))
	   (error 'excl:socket-error :identifier identifier))))
    (sb-sys:with-deadline (:seconds seconds)
      (funcall body-fn))))

(defmacro with-deadline-maybe (identifier seconds &body body)
  #-sbcl (declare (ignore identifier seconds))
  #-sbcl `(progn ,@body)
  #+sbcl `(call-with-deadline-maybe ,identifier ,seconds (lambda () ,@body)))

(defmethod print-object ((object zacl-socket) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((socket (socket object)))
      (if (real-stream object)
	  (format stream
		  "connected from ~a:~a to ~a:~a"
		 (get-local-address socket)
		 (get-local-port socket)
		 (get-peer-address socket)
		 (get-peer-port socket))
	  (format stream
		  "waiting for connection at ~a:~a"
		 (get-local-address socket)
		 (get-local-port socket))))))

(defmethod stream-write-byte :after ((stream zacl-socket) byte)
  (declare (ignore byte))
  (incf (bytes-written stream)))

(defmethod stream-write-byte ((stream zacl-socket) byte)
  (with-deadline-maybe :write-timeout (write-timeout stream)
    (write-byte byte (real-stream stream))))

(defmethod stream-write-char ((stream zacl-socket) char)
  (map nil (lambda (octet)
             (stream-write-byte stream octet))
       (string-to-octets (string char) :encoding (socket-ef stream))))

(defmethod stream-write-sequence :after ((stream zacl-socket) sequence
                                         start end &key &allow-other-keys)
  ;; FIXME: Could be affected by string encoding?
  (declare (ignore sequence))
  (incf (bytes-written stream) (- end start)))

(defmethod excl::socket-bytes-written ((socket zacl-socket) &optional set)
  (if set
      (setf (bytes-written socket) set)
      (bytes-written socket)))

(defmethod stream-write-sequence ((stream zacl-socket) sequence start end
                                  &key &allow-other-keys)
  (when (typep sequence 'string)
    (setf sequence (string-to-octets sequence :start start :end end
                                     :encoding (socket-ef stream)))
    (setf start 0)
    (setf end (length sequence)))
  (with-deadline-maybe :write-timeout (write-timeout stream)
    (write-sequence sequence (real-stream stream) :start start :end end)))

(defmethod stream-read-byte ((stream zacl-socket))
  (with-deadline-maybe :read-timeout (read-timeout stream)
    (read-byte (real-stream stream) nil :eof)))

(defmethod stream-read-char ((stream zacl-socket))
  (with-deadline-maybe :read-timeout (read-timeout stream)
    (let ((byte (read-byte (real-stream stream) nil :eof)))
      (if (eql byte :eof)
	  :eof
	  (code-char byte)))))

(defmethod stream-read-char-no-hang ((stream zacl-socket))
  (let ((ready (wait-for-input (socket stream) :timeout 0 :ready-only t)))
    (when ready
      (stream-read-char stream))))

(defmethod stream-read-sequence ((stream zacl-socket) sequence start end
                                 &key &allow-other-keys)
  (unless start (setf start 0))
  (unless end (setf end (length sequence)))
  (with-deadline-maybe :read-timeout (read-timeout stream)
    (if (stringp sequence)
	(let ((offset start)
	      (buffer (make-array (- end start) :element-type '(unsigned-byte 8))))
	  (let* ((after-index (read-sequence buffer (real-stream stream)))
		 (string (octets-to-string buffer :start 0 :end after-index
						  :encoding :latin-1)))
	    (replace sequence string :start1 start :end1 end
				     :start2 0 :end2 after-index)
	    (+ offset after-index)))
	(read-sequence sequence (real-stream stream) :start start :end end))))

#+ccl
(defmethod ccl:stream-read-vector ((stream zacl-socket) sequence start end)
  (unless start (setf start 0))
  (unless end (setf end (length sequence)))
  (if (stringp sequence)
      (let ((offset start)
            (buffer (make-array (- end start) :element-type '(unsigned-byte 8))))
        (let* ((after-index (read-sequence buffer (real-stream stream)))
               (string (octets-to-string buffer :start 0 :end after-index
                                         :encoding :latin-1)))
          (replace sequence string :start1 start :end1 end
                   :start2 0 :end2 after-index)
          (+ offset after-index)))
      (read-sequence sequence (real-stream stream) :start start :end end)))

(defmethod stream-force-output ((stream zacl-socket))
  (with-deadline-maybe :write-timeout (write-timeout stream)
    (force-output (real-stream stream))))

(defmethod close ((stream zacl-socket) &key abort)
  (declare (ignore abort))
  (close (real-stream stream))
  (socket-close (socket stream)))

(defun socket:make-socket (&key connect local-port local-host reuse-address
                             remote-port remote-host
                             format (backlog 5) type nodelay)
  (declare (ignore format type))
  (unless backlog
    (setf backlog 5))
  (ecase connect
    (:passive
     (let ((socket
            (socket-listen local-host (or local-port 0)
                           :reuseaddress reuse-address
                           :element-type '(unsigned-byte 8)
                           :backlog backlog)))
       (make-instance 'zacl-socket
                      :socket socket)))

    ((nil :active)
     (let ((socket
            (socket-connect remote-host remote-port
                            :nodelay nodelay
                            :element-type '(unsigned-byte 8))))
       (make-instance 'zacl-socket
                      :socket socket
                      :real-stream (socket-stream socket))))))

(defun socket:accept-connection (socket)
  (let ((incoming (socket-accept (socket socket))))
    (make-instance 'zacl-socket
                   :socket incoming
                   :real-stream (socket-stream incoming))))

(defun socket:local-host (socket)
  (ip-address-integer (get-local-address (socket socket))))

(defun socket:local-port (socket)
  (get-local-port (socket socket)))

(defun socket:set-socket-options (socket &key nodelay)
  (setf (socket-option (socket socket) :tcp-no-delay) nodelay))

(defun socket:remote-host (socket)
  (ip-address-integer (get-peer-address (socket socket))))

(defun socket:ipaddr-to-dotted (ip-integer)
  (format nil "~A.~A.~A.~A"
          (ldb (byte 8 24) ip-integer)
          (ldb (byte 8 16) ip-integer)
          (ldb (byte 8  8) ip-integer)
          (ldb (byte 8  0) ip-integer)))

(defun socket:dotted-to-ipaddr (dotted &key errorp)
  (let ((result 0)
        (length (length dotted))
        (i 0)
        (start 0))
    (loop
      (when (= i 4)
        (return result))
      (when (<= length start)
        (if errorp
            (error "Malformed IP string - overrun: ~A" dotted)
            (return nil)))
      (let* ((end (or (position #\. dotted :start start) length))
             (integer (ignore-errors
                        (parse-integer dotted :start start :end end))))
        (unless integer
          (if errorp
              (error "Cannot parse integer at ~A-~A of ~A"
                     start end dotted)
              (return nil)))
        (setf result (logior integer
                             (ash result 8)))
        (incf i)
        (setf start (1+ end))))))

(defmacro socket:with-pending-connect (&body body)
  `(progn ,@body))

(defun socket:ipaddr-to-hostname (ipaddr)
  #+sbcl (sb-bsd-sockets:host-ent-name
	  (sb-bsd-sockets:get-host-by-address
	   (if (typep ipaddr 'string)
	       (sb-bsd-sockets:make-inet-address ipaddr)
	       ipaddr)))
  #+ccl (ipaddr-to-hostname ipaddr))

(defun socket:lookup-hostname (name)
  #+ccl (lookup-hostname name)
  ;;
  ;; FLAG -- this assumes IPv4 - fix to work with IPv6 also. 
  ;;
  #+sbcl (format nil "~{~a~^.~}" (coerce (get-host-by-name name) 'list)))

(defun make-ssl-stream (socket stream-type &key context
						method
						certificate key certificate-password
						max-depth
						ca-file ca-directory
						ciphers
						crl-check crl-file
						verify
						server-name)
  (declare (ignore crl-check crl-file))
  (ecase stream-type
    ((:client :server)))

  (when (and ca-file
	     (pathnamep ca-file))
    (setf ca-file (namestring ca-file)))
  (when (and ca-directory
	     (pathnamep ca-directory))
    (setf ca-directory (namestring ca-directory)))

  (let ((stream (if context
		    (with-global-context ((ssl-context-open-ssl-context context))
		      (let ((arguments nil))
			(setf (getf arguments :certificate) (or certificate (ssl-context-certificate context)))
			(setf (getf arguments :key) (or key
							(ssl-context-key context)
							;; CL+SSL fails to automatically extract the private key
							;; from PEM certificates, therefore we must point the
							;; key to the certificate if no key is specified
							(ssl-context-certificate context)))
			(alexandria:when-let (key-password (ssl-context-key-password context))
			  (setf (getf arguments :password) key-password))
			(when (eq stream-type :client)
			  (when verify
			    (setf (getf arguments :verify) verify))
			  (when server-name
			    (setf (getf arguments :hostname) server-name)))
			(apply (if (eq stream-type :client)
				   #'make-ssl-client-stream
				   #'make-ssl-server-stream)
			       (real-stream socket)
			       arguments)))
		    (with-global-context ((ssl-context-open-ssl-context (if (eq stream-type :client)
									    (make-ssl-context
									     :method method
									     :max-depth max-depth
									     :ca-file ca-file
									     :certificate certificate
									     :key key
									     :certificate-password certificate-password
									     :verify verify
									     :max-depth max-depth
									     :ca-file ca-file
									     :ca-directory ca-directory
									     :ciphers ciphers)
									    (make-ssl-context
									     :method method
									     :max-depth max-depth
									     :ca-file ca-file
									     :certificate certificate
									     :key key
									     :certificate-password certificate-password
									     :verify verify
									     :max-depth max-depth
									     :ca-file ca-file
									     :ca-directory ca-directory)))
					  :auto-free-p t)
		      (if (eq stream-type :client)
			  (make-ssl-client-stream (real-stream socket)
						  :certificate certificate
						  :key (or key certificate)
						  :password certificate-password
						  :verify verify
						  :hostname server-name)
			  (make-ssl-server-stream (real-stream socket)
					      :certificate certificate
					      :key (or key certificate)
					      :password certificate-password))))))
    (setf (real-stream socket) stream)
    socket))

(defun socket::make-ssl-client-stream (socket
				       &rest args
				       &key context
					    method
					    ciphers
					    certificate key certificate-password
					    verify max-depth server-name
					    ca-file ca-directory
					    crl-check crl-file)
  (declare (ignorable method ciphers verify max-depth server-name))
  (when (and context
	     (or certificate key ca-file ca-directory certificate-password crl-file crl-check))
    (error "Cannot supply both a CONTEXT and one of CERTIFICATE, KEY, CA-FILE, CA-DIRECTORY, CERTIFICATE-PASSWORD, CRL-FILE, or CRL-CHECK."))
  (apply #'make-ssl-stream socket :client args))

(defun socket::make-ssl-server-stream (socket
                                       &rest args
				       &key context
					    method
                                            certificate key certificate-password
                                            verify max-depth
                                            ca-file ca-directory
					    crl-file crl-check)
  (declare (ignorable method verify max-depth))
  (when (and context
	     (or certificate key ca-file ca-directory certificate-password crl-file crl-check))
    (error "Cannot supply both a CONTEXT and one of CERTIFICATE, KEY, CA-FILE, CA-DIRECTORY, CERTIFICATE-PASSWORD, CRL-FILE, or CRL-CHECK."))
  (apply #'make-ssl-stream socket :server args))

(defun socket:socket-control (socket &key (read-timeout nil read-timeout-p)
					  (write-timeout nil write-timeout-p))
  #-sbcl (declare (ignore socket read-timeout write-timeout read-timeout-p write-timeout-p))
  #+sbcl
  (when read-timeout-p
    (setf (read-timeout socket) read-timeout))
  #+sbcl
  (when write-timeout-p
    (setf (write-timeout socket) write-timeout)))

(defun make-ssl-context (&key method
			      certificate
			      key
			      certificate-password
			      verify
			      max-depth
			      ca-file
			      ca-directory
			      ciphers)
  (let ((arguments (list)))
    (when method
      (if (listp method)
	  (error "Specifying a list of enabled SSL Methods is unsupported at the time.")
	  (ecase method
	    (:tlsv1+) ;; the default for CL+SSL: TLS-method
	    (:sslv3+
	     (warn "SSL v3 is deprecated and should not be used. Defaulting to TLS v1+ methods."))
	    (:sslv23
	     (error "SSL v2 is an unsecure protocol."))
	    (:sslv2
	     (error "SSL v2 is an unsecure protocol."))
	    (:sslv3
	     (warn "SSL v3 is deprecated and should not be used.")
	     (setf (getf arguments :method) (cl+ssl::ssl-v3-method)))
	    (:tlsv1
	     (setf (getf arguments :method) (cl+ssl::ssl-TLSv1-method)))
	    (:tlsv1.1
	     (setf (getf arguments :method) (cl+ssl::ssl-TLSv1-1-method)))
	    (:tlsv1.1+
	     (setf (getf arguments :disabled-protocols)
	           (list cl+ssl:+SSL-OP-NO-SSLv2+ cl+ssl:+SSL-OP-NO-SSLv3+
	                 cl+ssl:+SSL-OP-NO-TLSv1+)))
	    (:tlsv1.2
	     (setf (getf arguments :method) (cl+ssl::ssl-TLSv1-2-method)))
	    (:tlsv1.2+
	     (setf (getf arguments :disabled-protocols)
	           (list cl+ssl:+SSL-OP-NO-SSLv2+ cl+ssl:+SSL-OP-NO-SSLv3+
	                 cl+ssl:+SSL-OP-NO-TLSv1+ cl+ssl:+SSL-OP-NO-TLSv1-1+)))
	    (:tlsv1.3+
	     (setf (getf arguments :disabled-protocols) (list CL+SSL::+SSL-OP-NO-SSLv2+ CL+SSL::+SSL-OP-NO-SSLv3+
	                                                      CL+SSL::+SSL-OP-NO-TLSv1+ CL+SSL::+SSL-OP-NO-TLSv1-1+
	                                                      CL+SSL::+SSL-OP-NO-TLSv1-2+)))
	    )))
    (when ciphers
      (unless (stringp ciphers) (error "Argument CIPHERS must be a string"))
      (setf (getf arguments :cipher-list) ciphers))
    ;; Verification documentation: https://www.openssl.org/docs/man1.1.1/man3/SSL_CTX_set_verify.html
    (if verify
	(ecase verify
	  (:optional
	   (setf (getf arguments :verify-mode)
		 cl+ssl::+SSL-VERIFY-PEER+))
	  (:required
	   (setf (getf arguments :verify-mode)
		 (logior cl+ssl::+SSL-VERIFY-PEER+
			 cl+ssl::+SSL-VERIFY-FAIL-IF-NO-PEER-CERT+))))
	(setf (getf arguments :verify-mode)
	      cl+ssl::+SSL-VERIFY-NONE+))
    (when (and ca-file
	       (pathnamep ca-file))
      (setf ca-file (namestring ca-file)))
    (when (and ca-directory
	       (pathnamep ca-directory))
      (setf ca-directory (namestring ca-directory)))
    (cond ((and ca-file ca-directory)
	   (setf (getf arguments :verify-location) (list ca-file ca-directory)))
	  (ca-file (setf (getf arguments :verify-location) ca-file))
	  (ca-directory (setf (getf arguments :verify-location) ca-directory)))
    (when max-depth
      (setf (getf arguments :verify-depth) max-depth))
    (when (and certificate
	       (pathnamep certificate))
      (setf certificate (namestring certificate)))
    (when (and key
	       (pathnamep key))
      (setf key (namestring key)))
    (let ((context (apply #'make-context arguments)))
      (when certificate
	(with-global-context (context)
	  (use-certificate-chain-file certificate)))
      (excl::make-ssl-context :open-ssl-context context
			      :certificate certificate
			      :key key
			      :key-password certificate-password))))

(defun socket:make-ssl-server-context (&rest args
				       &key (method :tlsv1+)
					    certificate
					    key
					    certificate-password
					    (verify nil)
					    (max-depth 10)
					    ca-file
					    ca-directory
					    ciphers
					    crl-check
					    crl-file
					    prefer-server-cipher-order
					    server-name)
  (declare (ignore certificate key certificate-password
		   ca-file ca-directory ciphers))
  (when (or crl-check
	    crl-file
	    prefer-server-cipher-order
	    server-name)
    (error "The arguments CRL-CHECK, CRL-FILE, PREFER-SERVER-CIPHER-ORDER, and SERVER-NAME are unsupported at the moment."))
  ;; Don't loose the defaults
  (setf (getf args :method) method)
  (setf (getf args :verify) verify)
  (setf (getf args :max-depth) max-depth)
  (apply #'make-ssl-context args))

(defun socket:make-ssl-client-context (&rest args
				       &key (method :tlsv1+)
					    certificate
					    key
					    certificate-password
					    (verify :optional)
					    (max-depth 10)
					    ca-file
					    ca-directory
					    ciphers
					    crl-check
					    crl-file
					    prefer-server-cipher-order)
  (declare (ignore certificate key certificate-password
		   ca-file ca-directory ciphers))
  (when (or crl-check
	    crl-file
	    prefer-server-cipher-order)
    (error "The arguments CRL-CHECK, CRL-FILE, and PREFER-SERVER-CIPHER-ORDER are unsupported at the moment."))
  ;; Don't loose the defaults
  (setf (getf args :method) method)
  (setf (getf args :verify) verify)
  (setf (getf args :max-depth) max-depth)
  (apply #'make-ssl-context args))

(defun socket:get-ssl-verify-result (ssl-stream)
  (cl+ssl::ssl-get-verify-result
   (cl+ssl::ssl-stream-handle
    (real-stream ssl-stream))))

(defun socket:get-ssl-peer-certificate (ssl-stream)
  (cl+ssl::ssl-get-peer-certificate
   (cl+ssl::ssl-stream-handle
    (real-stream ssl-stream))))

(defun socket:x509-certificate-subject (x509-certificate)
  (let ((subject-one-liner
	  (unless (cffi:null-pointer-p x509-certificate)
	    (cffi:with-foreign-pointer (buf 1024)
	      (let ((subject-name (cl+ssl::x509-get-subject-name x509-certificate)))
		(unless (cffi:null-pointer-p subject-name)
		  (cl+ssl::x509-name-oneline subject-name buf 1024)
		  (cffi:foreign-string-to-lisp buf)))))))
    (mapcar (lambda (element)
	      (let ((splitted (uiop:split-string element
						 :separator '(#\=))))
		(cons (first splitted)
		      (second splitted))))
	    (remove "" (uiop:split-string subject-one-liner
					  :separator '(#\/))
		    :test #'string=))))

(defun socket:x509-certificate-subject-alt-name (x509-certificate)
  ;; Remarks:
  ;; - in ACL socket:x509-certificate-subject-alt-name returns:
  ;;   '(("DNS" . "<name1>") ("DNS" . "<nameN>") ("IP Address" . "n.n.n.n") ("IP Address" . "n:n:n:n:n:n:n:n")))
  
  (cl+ssl::certificate-all-alt-names x509-certificate))
