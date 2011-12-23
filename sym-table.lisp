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

(defmethod tmember (sym table)
  (let ((entry (tget (zsymbol-name sym) table)))
    (eq entry sym)))

(defmethod tput (sym table)
  (let* ((name (zsymbol-name sym))
         (existing (tget name table)))
    (when existing
      (error "~A already names a symbol in ~A (~A)"
             name table existing))
    (setf (gethash name (name-table table)) sym)))

(defmethod tremove (sym table)
  (unless (tmember sym table)
    (error "~A is not a member of ~A"
           sym table))
  (remhash (zsymbol-name sym) (name-table table)))

(defmethod tensure (sym table)
  (unless (tmember sym table)
    (tput sym table)))

(defmethod tremove-if-member (sym table)
  (when (tmember sym table)
    (tremove sym table)))

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
