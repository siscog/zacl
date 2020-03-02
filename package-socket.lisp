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
    :reader socket)
   (real-stream
    :initarg :real-stream
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
    :accessor zacl-cl:stream-external-format)))

(defmethod stream-write-byte :after ((stream zacl-socket) byte)
  (declare (ignore byte))
  (incf (bytes-written stream)))

(defmethod stream-write-byte ((stream zacl-socket) byte)
  (write-byte byte (real-stream stream)))

(defmethod stream-write-char ((stream zacl-socket) char)
  (map nil (lambda (octet)
             (stream-write-byte stream octet))
       (string-to-octets (string char) :external-format (socket-ef stream))))

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
                                     :external-format (socket-ef stream)))
    (setf start 0)
    (setf end (length sequence)))
  (write-sequence sequence (real-stream stream) :start start :end end))

(defmethod stream-read-byte ((stream zacl-socket))
  (read-byte (real-stream stream) nil :eof))

(defmethod stream-read-char ((stream zacl-socket))
  (let ((byte (read-byte (real-stream stream) nil :eof)))
    (if (eql byte :eof)
        :eof
        (code-char byte))))

(defmethod stream-read-char-no-hang ((stream zacl-socket))
  (let ((ready (wait-for-input (socket stream) :timeout 0 :ready-only t)))
    (when ready
      (stream-read-char stream))))

(defmethod stream-read-sequence ((stream zacl-socket) sequence start end
                                 &key &allow-other-keys)
  (unless start (setf start 0))
  (unless end (setf end (length sequence)))
  (if (stringp sequence)
      (let ((offset start)
            (buffer (make-array (- end start) :element-type '(unsigned-byte 8))))
        (let* ((after-index (read-sequence buffer (real-stream stream)))
               (string (octets-to-string buffer :start 0 :end after-index
                                         :external-format :latin-1)))
          (replace sequence string :start1 start :end1 end
                   :start2 0 :end2 after-index)
          (+ offset after-index)))
      (read-sequence sequence (real-stream stream) :start start :end end)))

#+ccl
(defmethod ccl:stream-read-vector ((stream zacl-socket) sequence start end)
  (unless start (setf start 0))
  (unless end (setf end (length sequence)))
  (if (stringp sequence)
      (let ((offset start)
            (buffer (make-array (- end start) :element-type '(unsigned-byte 8))))
        (let* ((after-index (read-sequence buffer (real-stream stream)))
               (string (octets-to-string buffer :start 0 :end after-index
                                         :external-format :latin-1)))
          (replace sequence string :start1 start :end1 end
                   :start2 0 :end2 after-index)
          (+ offset after-index)))
      (read-sequence sequence (real-stream stream) :start start :end end)))

(defmethod stream-force-output ((stream zacl-socket))
  (force-output (real-stream stream)))

(defmethod close ((stream zacl-socket) &key abort)
  (declare (ignore abort))
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
            (socket-listen local-host local-port
                           :reuseaddress reuse-address
                           :element-type '(unsigned-byte 8)
                           :backlog backlog)))
       (make-instance 'zacl-socket
                      :socket socket)))
    ((nil)
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


(defun socket::make-ssl-client-stream (socket &key &allow-other-keys)
  (let ((stream (make-ssl-client-stream (real-stream socket))))
    (setf (real-stream socket) stream)
    socket))

(defun socket::make-ssl-server-stream (socket
                                       &key
					 context
                                         certificate key certificate-password
                                         verify
                                         ca-file ca-directory crl-file crl-check method max-depth)
  (declare (ignore max-depth method crl-check crl-file ca-directory ca-file verify))
  (let ((stream (make-ssl-server-stream (real-stream socket)
					:certificate certificate
					:key (or key certificate)
					:password certificate-password)))
    (setf (real-stream socket) stream)
    socket))

(defun socket:socket-control (socket &key read-timeout write-timeout)
  (declare (ignore socket read-timeout write-timeout))
  nil)

(defun socket:make-ssl-server-context (&key (method :tlsv1+)
					    certificate
					    key
					    certificate-password
					    verify
					    (max-depth 10)
					    ca-file
					    ca-directory
					    ciphers
					    crl-check
					    crl-file
					    (prefer-server-cipher-order t)
					    server-name)
  (declare (ignore server-name))
  (error "unimplemented")
  (apply #'cl+ssl:make-context
	 (loop for (k v)
		 on (list :method nil
			  (:options (list +SSL-OP-ALL+))
			  (:session-cache-mode +ssl-sess-cache-server+)
			  (:verify-location :default)
			  :verify-depth max-depth
			  (:verify-mode +ssl-verify-peer+)
			  (:verify-callback nil verify-callback-supplied-p)
			  (:cipher-list +default-cipher-list+)
			  (:pem-password-callback 'pem-password-callback))
	       by #'cddr
	       when v
		 collect k and collect v)))

(defun socket:make-ssl-client-context (&rest args)
  (declare (ignore args))
  (error "unimplemented"))

(defun socket:get-ssl-verify-result (&rest args)
  (declare (ignore args))
  (error "unimplemented"))

(defun socket:x509-certificate-subject (&rest args)
  (declare (ignore args))
  (error "unimplemented"))

(defun get-ssl-peer-certificate (&rest args)
  (declare (ignore args))
  (error "unimplemented"))
