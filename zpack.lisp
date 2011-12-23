(defpackage #:zpack
  (:use #:cl))

(in-package #:zpack)

;;; Symbol internal management

(defgeneric sym-pack (sym))
(defgeneric (setf sym-pack) (pack sym))

;;; Sym tables

(defgeneric make-sym-table ())
(defgeneric tget (sym-name table))
(defgeneric tput (sym table))
(defgeneric tremove (sym table))
(defgeneric tmember (sym table))
(defgeneric tmap-syms (fun table))
(defgeneric tmembers (table))

;;; Pack management

(defgeneric present-table (pack))
(defgeneric shadowing-table (pack))
(defgeneric external-table (pack))

(defgeneric accessiblep (sym pack))
(defgeneric externalp (sym pack))
(defgeneric shadowingp (sym pack))
(defgeneric presentp (sym pack))

(defgeneric check-import-conflict (sym pack))
(defgeneric check-inherit-conflict (used-pack using-pack))
(defgeneric check-export-conflict (sym pack))
(defgeneric check-unintern-conflict (sym-name pack))

(defgeneric zimport-without-checks (sym pack))
(defgeneric zunintern-without-checks (sym pack))

(defgeneric (setf used-packs) (used-packs pack))
(defgeneric (setf used-by-packs) (used-by-packs pack))

;;; Clone of the CL symbol/package interface

(defgeneric zmake-symbol (sym-name))
(defgeneric zsymbol-name (sym))
(defgeneric zsymbol-package (sym))

(defgeneric zmake-package (pack-name))
(defgeneric zfind-package (pack-name))
(defgeneric zdelete-package (pack-name))

(defgeneric zfind-symbol (sym-name pack))
(defgeneric zimport (sym pack))
(defgeneric zintern (sym-name pack))
(defgeneric zshadow (sym-name pack))
(defgeneric zshadowing-import (sym pack))
(defgeneric zexport (sym pack))

(defgeneric zunexport (sym pack))
(defgeneric zunintern (sym pack))

(defgeneric zuse-package (pack using-pack))
(defgeneric zunuse-package (pack using-pack))

(defgeneric zpackage-name (pack))
(defgeneric zpackage-use-list (pack))
(defgeneric zpackage-used-by-list (pack))
(defgeneric zpackage-shadowing-symbols (pack))


;;; Implementation of syms

(defclass sym ()
  ((name
    :initarg :name
    :reader zsymbol-name)
   (pack
    :initarg :pack
    :reader zsymbol-package
    :accessor sym-pack))
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


;;; Implementation of sym-tables

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
  (setf (gethash (zsymbol-name sym) (name-table table)) sym))

(defmethod tremove (sym table)
  (remhash (zsymbol-name sym) (name-table table)))

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


;;; Implementation of packs & CL clone interface

(defvar *packs* (make-hash-table :test 'equal))

(defclass pack ()
  ((name
    :initarg :name
    :reader zpackage-name)
   (external-table
    :initarg :external-table
    :reader external-table)
   (present-table
    :initarg :present-table
    :reader present-table)
   (shadowing-table
    :initarg :shadowing-table
    :reader shadowing-table)
   (used-packs
    :initarg :used-packs
    :reader zpackage-use-list
    :writer (setf used-packs))
   (used-by-packs
    :initarg :used-by-packs
    :reader zpackage-used-by-list
    :writer (setf used-by-packs)))
  (:default-initargs
   :name (error "A package name is required")
   :external-table (make-sym-table)
   :present-table (make-sym-table)
   :shadowing-table (make-sym-table)
   :used-packs nil
   :used-by-packs nil))

(defmethod print-object ((pack pack) stream)
  (print-unreadable-object (pack stream :type t)
    (prin1 (zpackage-name pack) stream)))

(defmethod zpackage-shadowing-symbols (pack)
  (tmembers (shadowing-table pack)))


(defmethod accessiblep (sym pack)
  (let ((existing-sym (zfind-symbol (zsymbol-name sym) pack)))
    (eq existing-sym sym)))

(defmethod externalp (sym pack)
  (tmember sym (external-table pack)))

(defmethod shadowingp (sym pack)
  (tmember sym (shadowing-table pack)))

(defmethod presentp (sym pack)
  (tmember sym (present-table pack)))


(defmethod check-import-conflict (sym pack)
  (let ((existing-sym (zfind-symbol (zsymbol-name sym) pack)))
    (when (and existing-sym (not (eq existing-sym sym)))
      (error "Conflict: importing ~A into ~A conflicts with ~A"
             sym pack existing-sym))))

(defmacro zdo-external-symbols ((var pack) &body body)
  `(tmap-syms (lambda (,var)
                ,@body)
              (external-table ,pack)))

(defmethod check-inherit-conflict (used-pack using-pack)
  (zdo-external-symbols (inherited-sym used-pack)
    (let ((existing-sym (zfind-symbol (zsymbol-name inherited-sym)
                                      using-pack)))
       (when (and existing-sym
                  (not (eq inherited-sym existing-sym))
                  (not (shadowingp existing-sym using-pack)))
         (error "Conflict: Inheriting ~A from ~A conflicts with ~A in ~A"
                inherited-sym
                used-pack
                existing-sym
                using-pack)))))

(defmethod check-export-conflict (sym pack)
  (let ((sym-name (zsymbol-name sym)))
    (dolist (using-pack (zpackage-used-by-list pack))
      (let ((existing-sym (zfind-symbol sym-name using-pack)))
        (when existing-sym
          (unless (eq existing-sym sym)
            (error "Conflict: exporting ~A conflicts with ~A in ~A"
                   sym existing-sym using-pack)))))))

(defmethod check-unintern-conflict (sym pack)
  (let ((sym-name (zsymbol-name sym))
        (first-existing-sym nil))
    (dolist (used-pack (zpackage-use-list pack))
      (let ((existing-sym (zfind-symbol sym-name used-pack)))
        (if first-existing-sym
            (unless (eq existing-sym first-existing-sym)
              (error "Conflict: uninterning ~A would lead to conflict ~
                      between ~A and ~A"
                     sym first-existing-sym existing-sym))
            (setf first-existing-sym existing-sym))))))


(defmethod zimport-without-checks (sym pack)
  (tput sym (present-table pack))
  (unless (zsymbol-package sym)
    (setf (sym-pack sym) pack)))

(defmethod zunintern-without-checks (sym pack)
  (tremove sym (external-table pack))
  (tremove sym (shadowing-table pack))
  (tremove sym (present-table pack))
  (when (eq (zsymbol-package sym) pack)
    (setf (sym-pack sym) nil)))


(defmethod zmake-package (pack-name)
  (when (zfind-package pack-name)
    (error "A package named ~S already exists"
           pack-name))
  (setf (gethash pack-name *packs*)
        (make-instance 'pack :name pack-name)))

(defmethod zfind-package (pack-name)
  (values (gethash pack-name *packs*)))

(defmethod zdelete-package (pack)
  (remhash (zpackage-name pack) *packs*))


(defmethod zfind-symbol (sym-name pack)
  (let (sym)
    (cond ((setf sym (tget sym-name (external-table pack)))
           (values sym :external))
          ((setf sym (tget sym-name (shadowing-table pack)))
           (values sym :internal))
          ((setf sym (some (lambda (used-pack)
                             (tget sym-name (external-table used-pack)))
                           (zpackage-use-list pack)))
           (values sym :inherited))
          ((setf sym (tget sym-name (present-table pack)))
           (values sym :internal))
          (t
           (values nil nil)))))

(defmethod zimport (sym pack)
  (check-import-conflict sym pack)
  (unless (presentp sym pack)
    (zimport-without-checks sym pack))
  t)

(defmethod zintern (sym-name pack)
  (or (zfind-symbol sym-name pack)
      (let ((sym (zmake-symbol sym-name)))
        (zimport sym pack)
        sym)))

(defmethod zshadow (sym-name pack)
  (let ((sym (tget sym-name (present-table pack))))
    (unless sym
      (setf sym (zmake-symbol sym-name))
      (zimport-without-checks sym pack))
    (tput sym (shadowing-table pack))
    t))

(defmethod zshadowing-import (sym pack)
  (let ((sym-name (zsymbol-name sym)))
    (multiple-value-bind (existing-sym type)
        (zfind-symbol sym-name pack)
      (ecase type
        ((nil :inherited)
         (zshadow sym-name pack))
        ((:external :internal)
         (unless (eq existing-sym sym)
           (zunintern-without-checks existing-sym pack)
           (zimport sym pack))
         (tput sym (shadowing-table pack)))))))

(defmethod zexport (sym pack)
  (unless (accessiblep sym pack)
    (error "~A is not accessible in ~A"
           sym pack))
  (check-export-conflict sym pack)
  (unless (presentp sym pack)
    (zimport sym pack))
  (tput sym (external-table pack))
  t)


(defmethod zunexport (sym pack)
  (unless (accessiblep sym pack)
    (error "~A is not accessible in ~A"
           sym pack))
  (tremove sym (external-table pack))
  t)

(defmethod zunintern (sym pack)
  (when (accessiblep sym pack)
    (check-unintern-conflict sym pack)
    (zunintern-without-checks sym pack)
    t))


(defmethod zuse-package (pack using-pack)
  (let ((use-list (zpackage-use-list using-pack)))
    (unless (member pack use-list)
      (check-inherit-conflict pack using-pack)
      (setf (used-packs using-pack) (cons pack use-list))
      (setf (used-by-packs pack)
            (cons using-pack (zpackage-used-by-list pack)))))
  t)

(defmethod zunuse-package (pack using-pack)
  (setf (used-packs using-pack)
        (remove pack (zpackage-use-list using-pack)))
  (setf (used-by-packs pack)
        (remove using-pack (zpackage-used-by-list pack)))
  t)
