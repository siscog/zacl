;;;; package-excl.lisp
;;;
;;; This file is a grab-bag of miscellaneous functions from EXCL.
;;;

(in-package #:zacl)

(defparameter *external-format-translations*
  '((:octets . :latin1)
    (:latin1-base . :latin1)
    (:latin1 . :latin1)
    (:ascii . :latin1)
    (:iso8859-1 . :latin1)
    (:utf-8 . :utf-8)
    (nil . :latin1)))

(defun translate-external-format (external-format)
  (let ((entry (assoc external-format *external-format-translations*)))
    (if entry
        (cdr entry)
        :utf-8)))


(defvar excl:*initial-terminal-io* *terminal-io*)

(defvar excl:*cl-default-special-bindings* nil)

(defvar excl:*required-top-level-bindings* nil)

(defvar excl:*current-case-mode* :case-insensitive-upper)


(defmacro excl:named-function (name lambda-form)
  "Return the function produced by LAMBDA-FORM wrapped in a named
function object. Useful for debugging, as the function object is no
longer anonymous, but has a meaningful name name."
  (destructuring-bind (lambda-name lambda-list &body body)
      lambda-form
    (unless (eq lambda-name 'cl:lambda)
      (error "Unexpected named-function form"))
    `(flet ((,name ,lambda-list ,@body))
       #',name)))


(defun excl:featurep (feature)
  (find feature *features*))

(defun excl:gc (&optional full)
  (declare (ignorable full))
  #+ccl
  (ccl:gc)
  #+sbcl
  (sb-ext:gc :full full))

(defun excl:match-re (pattern string &key (return :string) case-fold)
  (multiple-value-bind (start end regs-starts regs-ends)
      (scan (create-scanner pattern :case-insensitive-mode case-fold) string )
    (when (and start end)
      (ecase return
        (:index
         (values-list (list* (cons start end) (map 'list 'cons regs-starts regs-ends))))
        (:string
         (values-list (list* (subseq string start end)
                             (map 'list (lambda (start end)
                                          (subseq string start end))
                                  regs-starts
                                  regs-ends))))
        ((nil)
         t)))))

(defun excl:match-regexp (pattern string &key (return :string))
  (excl:match-re pattern string :return return))

(defun excl:compile-regexp (pattern)
  (create-scanner pattern))


(defmacro excl:with-output-to-buffer ((stream) &body body)
  `(with-output-to-sequence (,stream)
     ,@body))

(defun excl:get-output-stream-buffer (stream)
  (get-output-stream-sequence stream))


;;; Streams

(defmacro excl:sm (slot-name object)
  `(slot-value ,object ',slot-name))

(defmacro excl:def-stream-class (name (&rest parents) &body slot-defs)
  `(defclass ,name (,@parents)
     ,@slot-defs))

(defmacro excl:with-stream-class ((class var) &body body)
  (declare (ignore class var))
  `(progn ,@body))

(define-condition excl:stream-closed-error (error) ())
(define-condition excl::socket-chunking-end-of-file (error) ())

(def-fake-slot excl::stream-property-list stream :default-value nil)
(def-fake-slot excl:stream-error-identifier stream :default-value nil)
(def-fake-slot excl:stream-error-code stream :default-value 0)
(defmethod excl:stream-error-identifier (condition)
  (socket-error-identifier condition))

(defmethod excl:stream-error-code (condition)
  (socket-error-code condition))

;;; Concurrency

;; FIXME: apparently using EXCL::FAST and EXCL::ATOMICALLY produces the same result
;; as EXCL:FAST-AND-CLEAN which in turn is a performant version of
;; EXCL:WITH-DELAYD-INTERRUPTS
;; https://franz.com/support/documentation/current/doc/operators/excl/fast-and-clean.htm
;; Additionally, not only these symbols not exported but they are also undocumented and
;; used unrestrainedly across ZACL and ZASERVE in all kinds of combinations...
(defmacro excl::fast (&body body)
  `(progn ,@body))

(defmacro excl::.atomically (&body body)
  `(progn ,@body))

(defmacro excl::atomically (&body body)
  `(progn ,@body))

(defmacro excl:atomic-conditional-setf (place new-value old-value)
  #-sbcl (declare (ignore old-value))
  ;; ATTENTION: the arguments in SB-EXT:COMPARE-AND-SWAP have a different order
  #+sbcl `(sb-ext:compare-and-swap ,place ,old-value ,new-value)
  #-sbcl `(setf ,place ,new-value))

(defmacro excl:incf-atomic (place &optional (delta 1))
  #+sbcl `(sb-ext:atomic-incf ,place ,delta)
  #-sbcl `(incf ,place ,delta))

(defmacro excl:decf-atomic (place &optional (delta 1))
  #+sbcl `(sb-ext:atomic-decf ,place ,delta)
  #-sbcl `(decf ,place ,delta))

(defmacro excl:pop-atomic (place)
  #+sbcl `(sb-ext:atomic-pop ,place)
  #-sbcl `(pop ,place))

(defmacro excl:push-atomic (value place)
  #+sbcl `(sb-ext:atomic-push ,value ,place)
  #-sbcl `(push ,value ,place))

;;; Misc

(defstruct (excl::ssl-context
	    (:constructor excl::make-ssl-context))
  (open-ssl-context nil)
  (key nil)
  (key-password nil)
  (certificate nil))

(defmacro excl:errorset (form &optional announce catch-breaks)
  "Return NIL if FORM signals an error, T and values as multiple
values otherwise."
  (declare (ignore announce catch-breaks))
  (let ((result (gensym "RESULT")))
    `(let ((,result (multiple-value-list (ignore-errors ,form))))
       (if (not (first ,result))
           nil
           (values-list (list* t ,result))))))

(defun excl::tilde-expand-unix-namestring (namestring)
  (merge-pathnames namestring))

(defun excl:run-shell-command (program
                               &key input output error-output separate-streams
                                 wait environment show-window)
  (declare (ignore program input output error-output separate-streams wait environment show-window))
  (error "Not implemented -- RUN-SHELL-COMMAND"))

(defun excl::merge-to-physical (pathname)
  (merge-pathnames (translate-logical-pathname pathname)))

(defun excl:rename-file-raw (file new-name)
  (let ((new-name (merge-pathnames new-name
                                   (make-pathname :name :unspecific
                                                  :type :unspecific
                                                  :defaults *default-pathname-defaults*))))
    (rename-file file new-name)))

(defun excl:delimited-string-to-list (string delimiter)
  (unless (characterp delimiter)
    ;; aserve only uses a character
    (error "Unsupported delimiter -- ~A" delimiter))
  (split-sequence delimiter string))

(defun excl:stream-input-fn (stream)
  "Return the underlying Unix input FD of STREAM."
  (stream-unix-fd stream))

(defun excl::filesys-size (fd)
  (fstat-size fd))

(defvar *unix-epoch-universal-time*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun excl::filesys-write-date (fd)
  (+ *unix-epoch-universal-time* (fstat-mtime fd)))

(defun excl::filesys-type (native-namestring)
  (file-kind native-namestring))

(defun excl.osi:stat (file)
  (if (probe-file file)
      (list :mtime (stat-mtime file))
      (error "~A does not exist" file)))

(defun excl.osi:stat-mtime (stat)
  (getf stat :mtime))

(defun excl:find-external-format (name &key errorp)
  (declare (ignore errorp))
  (translate-external-format name))

(defun excl:crlf-base-ef (external-format)
  external-format)

(defun excl:ef-name (external-format)
  external-format)

(defun excl:fixnump (integer)
  (typep integer 'fixnum))

(defmacro excl::with-dynamic-extent-usb8-array ((var len) &body body)
  `(let ((,var (make-array ,len :element-type '(unsigned-byte 8))))
     ,@body))

(defun excl:native-string-sizeof (string &key (external-format :latin1))
  (length (string-to-octets string :encoding (translate-external-format external-format))))

(defun excl:string-to-mb (string &key external-format mb-vector null-terminate)
  (when null-terminate
    (error "Cannot null-terminate"))
  (let ((result
         (string-to-octets string
                           :encoding (translate-external-format external-format))))
    (if mb-vector
        (replace mb-vector result)
        result)))

(defun excl:string-to-octets (string &key external-format null-terminate)
  (when null-terminate
    (error "No null terminating!"))
  (string-to-octets string
                    :encoding (translate-external-format external-format)))

(defun excl:mb-to-string (string &key external-format (start 0)
                                   (end (length string)))
  (octets-to-string string
                    :encoding (translate-external-format external-format)
                    :start start
                    :end end))

(defun excl:octets-to-string (octets &key external-format
                                          string (string-start 0) string-end
                                          truncate
                                          (start 0) (end (length octets)))
  (declare (ignore truncate))
  (octets-to-string octets
                    :string string
                    :start start
                    :end end
                    :string-start string-start
                    :string-end string-end
                    :encoding (translate-external-format external-format)
                    :return-written-size? t))

(defun excl:schedule-finalization (object fun)
  ;; Doesn't work; semantics differ.
  ;; (finalize object fun))
  (declare (ignore object fun)))

(defun excl::unix-signal (signal-number action)
  (declare (ignore signal-number action)))

(defmacro excl:without-package-locks (&body body)
  #-sbcl
  `(progn ,@body)
  #+sbcl
  `(without-package-locks ,@body))

(defmacro excl:without-interrupts (&body body)
  #+(or ccl sbcl)
  `(without-interrupts ,@body)
  #-(or ccl sbcl)
  `(progn ,@body))

(defmacro excl:defvar-nonbindable (name value &optional doc)
  #+sbcl
  `(defglobal ,name ,value ,@(if doc (list doc)))
  #+ccl
  `(defstaticvar ,name ,value ,@(if doc (list doc)) )
  #-(or ccl sbcl)
  `(defvar ,name ,value ,@(if doc  (list doc))))

(defclass excl:lockable-object ()
  ((lock
    :initarg :lock
    :reader lock
    :initform (make-lock))))

(defun call-with-locked-object (object fun)
  (let ((lock (lock object)))
    (with-lock-held (lock)
      (funcall fun))))

(defmacro excl:with-locked-object ((object &key type block non-smp) &body body)
  (declare (ignore type block non-smp))
  `(call-with-locked-object ,object (lambda () ,@body)))

(defstruct excl:synchronizing-structure
  (lock (make-lock)))

(defun call-with-locked-structure (struct fun)
  (with-lock-held ((synchronizing-structure-lock struct))
    (funcall fun)))

(defmacro excl:with-locked-structure ((struct &key block non-smp) &body body)
  (declare (ignore block non-smp))
  `(call-with-locked-structure ,struct (lambda () ,@body)))

(defstruct (basic-lock (:include excl:synchronizing-structure))
  name)

(defun excl::make-basic-lock (&key name)
  (make-basic-lock :name name))

(define-condition excl:socket-error (error)
  ((identifier
    :initarg :identifier
    :initform nil
    :reader excl:stream-error-identifier))
  (:report (lambda (condition stream)
	     (write-string (case (excl:stream-error-identifier condition)
			     (:read-timeout "Timeout while reading from socket")
			     (:write-timeout "Timeout while writing to socket")
			     (t "Socket error"))
			   stream))))

;;; Streams

(defgeneric excl:device-open (stream slot-names initargs))
(defgeneric excl:device-close (stream abort))
(defgeneric excl:device-read (stream buffer start end blocking))
(defgeneric excl:device-write (stream buffer start end blocking))
(defgeneric excl:install-single-channel-character-strategy (stream format &optional default)
  (:method (stream format &optional default)
    (declare (ignore stream format default))))

(defvar excl::*std-control-out-table* nil)

(defclass zacl-simple-stream (fundamental-binary-output-stream
                              fundamental-binary-input-stream)
  (
   ;; The original SIMPLE-STREAMs in ACL contain only one buffer that is used
   ;; interchangeably between output and input in SINGLE-CHANNEL-SIMPLE-STREAMs
   ;; according to its MODE
   ;; For simplicity's sake, we use an additional buffer for output and keep
   ;; the original one for input.
   (output-buffer
    :initform (make-array 4096 :element-type '(unsigned-byte 8))
    :accessor zacl-simple-stream-output-buffer)
   (output-pointer
    :initform 0
    :accessor zacl-simple-stream-output-pointer)

   (excl::buffer
    :initform (make-array 1024 :element-type '(unsigned-byte 8))
    :reader buffer)
   (excl::output-handle
    :initarg :output-handle)
   (excl::input-handle
    :initarg :input-handle
    :reader excl::stream-input-handle)
   (excl::buffer-ptr)
   (excl::control-out)
   (excl::co-state)
   (excl::oc-state)
   (external-format
    :initarg :external-format
    :initform :latin1
    :accessor zacl-cl:stream-external-format)))

(defmethod shared-initialize :after ((stream zacl-simple-stream)
                                     slot-names
                                     &rest initargs
				     &key &allow-other-keys)
  (excl:device-open stream slot-names initargs))

(defmethod close ((stream zacl-simple-stream) &key abort)
  (excl:device-close stream abort))

;; Needed for SBCL
(defmethod stream-element-type ((stream zacl-simple-stream))
  '(unsigned-byte 8))

(defmethod stream-read-sequence ((stream zacl-simple-stream) sequence start end
                                 &key &allow-other-keys)
  (unless start (setf start 0))
  (unless end (setf end (length sequence)))
  (if (stringp sequence)
      (let ((result (excl:device-read stream nil 0 nil nil))
            (buffer (buffer stream )))
        (when (minusp result)
          (return-from stream-read-sequence 0))
        (when (<= (- end start) result)
          (error "Can't handle buffering yet"))
        (let ((string (octets-to-string buffer
                                        :end result
                                        :encoding (zacl-cl:stream-external-format stream))))
          (replace sequence string :start1 start :end1 end)
          (min end (+ start (length string)))))
      (let ((result (excl:device-read stream sequence start end nil)))
        (if (minusp result)
            0
            result))))

(defmethod stream-write-char ((stream zacl-simple-stream) char)
  (stream-write-string stream (string char) 0 1)
  char)

(defmethod stream-write-byte ((stream zacl-simple-stream) byte)
  (let ((buffer (zacl-simple-stream-output-buffer stream)))
    (when (= (length buffer) (zacl-simple-stream-output-pointer stream))
      (force-output stream))
    (setf (elt buffer (zacl-simple-stream-output-pointer stream)) byte)
    (incf (zacl-simple-stream-output-pointer stream)))
  byte)

(defmethod stream-write-string ((stream zacl-simple-stream) string &optional start end)
  (let ((sequence (babel:string-to-octets string
					  :start start
					  :end end
					  :encoding (zacl-cl:stream-external-format stream))))
    (stream-write-sequence stream sequence 0 (length sequence)))
  string)

(defmethod stream-write-sequence ((stream zacl-simple-stream) (sequence string) start end &key)
  (stream-write-string stream sequence start end))

(defmethod stream-write-sequence ((stream zacl-simple-stream) sequence start end &key)
  (let ((buffer (zacl-simple-stream-output-buffer stream)))
    ;; no space in buffer?
    (when (> (+ (- end start) (zacl-simple-stream-output-pointer stream))
	     (length buffer))
      ;; flush it
      (stream-force-output stream)
      ;; the sequence is still bigger than our buffer
      ;; let's write it directly to the output
      (when (>= (- end start) (length buffer))
	(excl:device-write stream sequence start end nil)
	(return-from stream-write-sequence sequence)))
    ;; buffer it
    (replace buffer sequence
	     :start1 (zacl-simple-stream-output-pointer stream)
	     :start2 start
	     :end2 end)
    (incf (zacl-simple-stream-output-pointer stream) (- end start)))
  sequence)

(defmethod stream-force-output ((stream zacl-simple-stream))
  (let ((buffer (zacl-simple-stream-output-buffer stream)))
    (excl:device-write stream buffer 0 (zacl-simple-stream-output-pointer stream) nil)
    (setf (zacl-simple-stream-output-pointer stream) 0)))

(defmethod trivial-gray-streams:stream-fresh-line ((stream zacl-simple-stream))
  ;; This always prints a newline has we have no idea how to determine if we
  ;; are in the a new line or not
  (trivial-gray-streams:stream-terpri stream)
  t)

(defgeneric underlying-output-stream (stream)
  (:method ((stream zacl-simple-stream))
    (underlying-output-stream (slot-value stream 'excl::output-handle)))
  (:method ((stream stream-usocket))
    (socket-stream stream))
  (:method ((stream usocket))
    (socket-stream stream))
  (:method ((stream stream))
    stream))

(defgeneric underlying-input-stream (stream)
  (:method ((stream zacl-simple-stream))
    (underlying-input-stream (slot-value stream 'excl::input-handle)))
  (:method ((stream usocket))
    (socket-stream stream))
  (:method ((stream stream))
    stream))

(defclass excl:single-channel-simple-stream (zacl-simple-stream)
  ())

#+ccl
(defmethod ccl:stream-write-vector ((stream zacl-simple-stream) sequence start end)
  (unless start (setf start 0))
  (unless end (setf end (length sequence)))
  (excl:device-write stream sequence start end nil))

#+ccl
(defmethod ccl:stream-read-vector ((stream zacl-simple-stream) sequence start end)
  (unless start (setf start 0))
  (unless end (setf end (length sequence)))
  (if (stringp sequence)
      (let ((result (excl:device-read stream nil 0 nil nil))
            (buffer (buffer stream )))
        (when (minusp result)
          (return-from ccl:stream-read-vector 0))
        (when (<= (- end start) result)
          (error "Can't handle buffering yet"))
        (let ((string (octets-to-string buffer
                                        :end result
                                        :encoding (zacl-cl:stream-external-format stream))))
          (replace sequence string :start1 start :end1 end)
          (min end (+ start (length string)))))
      (let ((result (excl:device-read stream sequence start end nil)))
        (if (minusp result)
            0
            result))))

(defmethod stream-force-output ((stream usocket))
  (force-output (underlying-output-stream stream)))

(defmethod stream-read-char ((stream usocket))
  (stream-read-char (socket-stream stream)))

(defmethod stream-read-byte ((stream usocket))
  (stream-read-byte (socket-stream stream)))

(defmethod cl:close ((stream usocket) &key abort)
  (close (socket-stream stream) :abort abort))

(defmacro excl:add-stream-instance-flags (stream &rest flags)
  (declare (ignore stream flags))
  nil)

(defun excl:write-vector (vector stream &key (start 0) (end (length vector)))
  ;; The real write-vector has more complicated blocking
  ;; behavior. Save that for later.
  (let ((result end))
    (when (stringp vector)
      (setf vector (string-to-octets vector :start start :end end))
      (setf start 0)
      (setf end (length vector)))
    (write-sequence vector stream :start start :end end)
    result))

(defun excl:read-vector (vector stream &key (start 0) end endian-swap)
  (declare (ignore endian-swap))
  (read-sequence vector stream :start start :end end))

(defmethod excl::socket-bytes-written (socket &optional set)
  (declare (ignore socket))
  (or set
      42))

;;; MD5

(defun excl:md5-init ()
  (make-md5-state))

(defun excl:md5-update (state data &key (start 0) end external-format)
  (when (stringp data)
    (setf data (string-to-octets data
                                 :encoding (or external-format :latin-1)
                                 :start start
                                 :end (or end (length data))))
    (setf start 0)
    (setf end (length data)))
  (update-md5-state state data :start start :end end)
  (values))

(defun excl:md5-final (state &key (return :integer))
  (let ((result (finalize-md5-state state)))
    (ecase return
      (:usb8
       result)
      (:hex
       (with-output-to-string (s)
         (map nil (lambda (octet)
                    (format s "~2,'0X" octet))
              result)))
      (:integer
       (let ((i 0))
         (map nil (lambda (octet)
                    (setf i (logior (ash i 8) octet)))
              result)
         i)))))

;;; FASL

(defun excl:fasl-write (object stream &optional fasl-circle compile-verbose)
  (declare (ignore compile-verbose))
  (let ((*check-for-circs* fasl-circle))
    ;; This conversion is used because calls to EXCL:FASL-WRITE assume
    ;; a bivalent stream while passing a character stream. May not
    ;; matter, because it's not in a hot spot - just called at proxy
    ;; shutdown.
    (let ((octets
           (with-output-to-sequence (s)
             (store object stream))))
      (write-string (octets-to-string octets :encoding :latin-1) stream))))

(defun excl:fasl-read (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (restore stream)))

;;; BASE64

(defun excl:base64-string-to-string (string)
  (cl-base64:base64-string-to-string string))

(defun excl:string-to-base64-string (str)
  (cl-base64:string-to-base64-string str :columns 52))


;;; Microtime

(defconstant excl::base-for-internal-real-time -455615030)

(defun excl::acl-internal-real-time ()
  (values (-  (get-universal-time) 4165516800)
	  (/ (local-time:nsec-of (local-time:now)) 1000)))
