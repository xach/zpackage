;;;; pack.lisp

(in-package #:zpackage)

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
    :writer (setf %zpackage-use-list))
   (used-by-packs
    :initarg :used-by-packs
    :reader zpackage-used-by-list
    :writer (setf %zpackage-used-by-list)))
  (:default-initargs
   :external-table (make-sym-table)
   :present-table (make-sym-table)
   :shadowing-table (make-sym-table)
   :used-packs nil
   :used-by-packs nil))

(defmethod print-object ((pack pack) stream)
  (print-unreadable-object (pack stream :type t)
    (prin1 (zpackage-name pack) stream)))

(defmethod zfind-package (pack-name)
  (values (gethash pack-name *packs*)))

(defmethod zmake-package (pack-name)
  (when (zfind-package pack-name)
    (error "A package named ~S already exists"
           pack-name))
  (setf (gethash pack-name *packs*)
        (make-instance 'pack :name pack-name)))

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

(defmethod check-import-conflict (sym pack)
  (let ((existing-sym (zfind-symbol (zsymbol-name sym) pack)))
    (when (and existing-sym (not (eq existing-sym sym)))
      (error "Conflict: importing ~A into ~A conflicts with ~A"
             sym pack existing-sym))))

(defun zimport-without-checks (sym pack)
  (tput sym (present-table pack))
  (unless (zsymbol-package sym)
    (setf (sym-pack sym) pack)))

(defmethod zimport (sym pack)
  (check-import-conflict sym pack)
  (unless (presentp sym pack)
    (zimport-without-checks sym pack))
  t)

(defmethod check-export-conflict (sym pack)
  (let ((sym-name (zsymbol-name sym)))
    (dolist (using-pack (zpackage-used-by-list pack))
      (let ((existing-sym (zfind-symbol sym-name using-pack)))
        (when existing-sym
          (unless (eq existing-sym sym)
            (error "Conflict: exporting ~A conflicts with ~A in ~A"
                   sym existing-sym using-pack)))))))

(defmethod zexport (sym pack)
  (unless (accessiblep sym pack)
    (error "~A is not accessible in ~A"
           sym pack))
  (check-export-conflict sym pack)
  (unless (presentp sym pack)
    (zimport sym pack))
  (tensure sym (external-table pack))
  t)

(defmethod zunexport (sym pack)
  (unless (accessiblep sym pack)
    (error "~A is not accessible in ~A"
           sym pack))
  (tremove-if-member sym (external-table pack))
  t)

(defmethod zintern (sym-name pack)
  (or (zfind-symbol sym-name pack)
      (let ((sym (zmake-symbol sym-name)))
        (zimport sym pack)
        sym)))

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

(defun zunintern-without-checks (sym pack)
  (tremove-if-member sym (external-table pack))
  (tremove-if-member sym (shadowing-table pack))
  (tremove-if-member sym (present-table pack))
  (when (eq (zsymbol-package sym) pack)
    (setf (%zsymbol-package sym) nil)))

(defmethod zunintern (sym pack)
  (when (accessiblep sym pack)
    (check-unintern-conflict sym pack)
    (zunintern-without-checks sym pack)
    t))

(defmethod zshadow (sym-name pack)
  (let ((sym (tget sym-name (present-table pack))))
    (unless sym
      (setf sym (zmake-symbol sym-name))
      (zimport-without-checks sym pack))
    (tensure sym (shadowing-table pack))
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
         (tensure sym (shadowing-table pack)))))))

(defmethod zpackage-shadowing-symbols (pack)
  (tmembers (shadowing-table pack)))

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

(defmethod zuse-package (pack using-pack)
  (let ((use-list (zpackage-use-list using-pack)))
    (unless (member pack use-list)
      (check-inherit-conflict pack using-pack)
      (setf (%zpackage-use-list using-pack) (cons pack use-list))
      (setf (%zpackage-used-by-list pack)
            (cons using-pack (zpackage-used-by-list pack)))))
  t)

(defmethod zunuse-package (pack using-pack)
  (setf (%zpackage-use-list using-pack)
        (remove pack (zpackage-use-list using-pack)))
  (setf (%zpackage-used-by-list pack)
        (remove using-pack (zpackage-used-by-list pack)))
  t)

(defmethod accessiblep (sym pack)
  (let ((existing-sym (zfind-symbol (zsymbol-name sym) pack)))
    (eq existing-sym sym)))

(defmethod externalp (sym pack)
  (tmember sym (external-table pack)))

(defmethod shadowingp (sym pack)
  (tmember sym (shadowing-table pack)))

(defmethod presentp (sym pack)
  (tmember sym (present-table pack)))

