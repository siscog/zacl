;;;; defpackage-hack.lisp

(in-package #:zacl)

(defmacro user:defpackage (name &rest clauses)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defpackage ,name
       (:shadowing-import-from #:zacl-cl
                               #:macroexpand
                               #:read-sequence
                               #:streamp
                               #:read-char)
       ,@clauses)))

(defun zacl-cl:read-sequence (sequence stream &key start end partial-fill)
  (declare (ignore partial-fill))
  (read-sequence sequence stream :start start :end end))

(defun zacl-cl:macroexpand (form &optional env stop-on-special-forms-p)
  (declare (ignore stop-on-special-forms-p))
  (macroexpand form env))

(defun zacl-cl:stream-external-format (stream)
  (stream-external-format stream))

(defun (setf zacl-cl:stream-external-format) (new-value stream)
  (declare (ignore stream))
  ;; FIXME
  new-value)

(defun zacl-cl:streamp (object)
  (or (streamp object)
      (usocket-p object)))

(defgeneric zacl-cl:read-char (stream &optional eof-error-p eof-value)
  (:method ((stream usocket) &optional (eof-error-p t) eof-value)
    (read-char (socket-stream stream) eof-error-p eof-value))
  (:method ((stream stream) &optional (eof-error-p t) eof-value)
    (read-char stream eof-error-p eof-value)))

