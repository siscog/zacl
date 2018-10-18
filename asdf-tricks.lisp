;;;; asdf-tricks.lisp

(in-package #:zacl)

(defclass zacl-reader:cl-file (asdf:cl-source-file)
  ((type :initform "cl")))

(defmethod asdf:perform :around ((operation asdf:compile-op)
                                 (component zacl-reader:cl-file))
  (with-zacl-build-environment (call-next-method)))


