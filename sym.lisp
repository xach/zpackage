;;;; sym.lisp

(in-package #:zpackage)

(defclass sym ()
  ((name
    :initarg :name
    :reader zsymbol-name)
   (pack
    :initarg :pack
    :reader zsymbol-package
    :writer (setf %zsymbol-package)))
  (:default-initargs
   :pack nil))

(defmethod zmake-symbol (sym-name)
  (make-instance 'sym :name sym-name))

(defmethod print-object ((sym sym) stream)
  (print-unreadable-object (sym stream :type t :identity t)
    (let ((pack (zsymbol-package sym)))
      (cond ((null pack)
             (format stream "#:~A" (zsymbol-name sym)))
            (t
             (format stream "~A~:[::~;:~]~A"
                     (zpackage-name pack)
                     (externalp sym pack)
                     (zsymbol-name sym)))))))
