;;;; sym-table.lisp

(in-package #:zpackage)

(defclass sym-table ()
  ((name-table
    :initarg :name-table
    :reader name-table))
  (:default-initargs
   :name-table (make-hash-table :test 'equal)))

(defmethod make-sym-table ()
  (make-instance 'sym-table))

(defmethod tget (sym-name table)
  (values (gethash sym-name (name-table table))))

(defmethod tput (sym table)
  (setf (gethash (zsymbol-name sym) (name-table table)) sym))

(defmethod tremove (sym table)
  (remhash (zsymbol-name sym) (name-table table)))

(defmethod tmember (sym table)
  (let ((entry (tget (zsymbol-name sym) table)))
    (eq entry sym)))

(defmethod tmap-syms (fun table)
  (maphash (lambda (sym-name sym)
             (declare (ignore sym-name))
             (funcall fun sym))
           (name-table table)))

(defmethod tmembers (table)
  (let ((members '()))
    (tmap-syms (lambda (sym)
                 (push sym members))
               table)
    members))
