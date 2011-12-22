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
   (shadowed-table
    :initarg :shadowed-table
    :reader shadowed-table)
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
   :shadowed-table (make-sym-table)
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
          ((setf sym (tget sym-name (shadowed-table pack)))
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
  (let ((existing-sym (zfind-symbol (zsymbol-name sym) pack)))
    (when (and existing-sym (not (eq existing-sym sym)))
      (error "Conflict: importing ~A into ~A conflicts with ~A"
             sym pack existing-sym))
    ;; Why not use TENSURE here? Because of the spec of CL:IMPORT: "If
    ;; the symbol is already present in the importing package, import
    ;; has no effect." I interpret that to mean that it should not
    ;; change the symbol-package of symbol, even if it's nil, when the
    ;; symbol is already present.
    (unless (tmember sym (present-table pack))
      (tput sym (present-table pack))
      (unless (zsymbol-package sym)
        (setf (%zsymbol-package sym) pack))))
  t)

(defmethod zexport (sym pack)
  (unless (accessiblep sym pack)
    (error "~A is not accessible in ~A"
           sym pack))
  ;; Check for possible conflicts
  (let ((name (zsymbol-name sym)))
    (dolist (using-pack (zpackage-used-by-list pack))
      (let ((existing-sym (zfind-symbol name using-pack)))
        (unless (eq existing-sym sym)
          (error "Conflict: exporting ~A conflicts with ~A in ~A"
                 sym existing-sym using-pack)))))
  ;; No conflicts
  (unless (presentp sym pack)
    (zimport sym pack))
  (setf (externalp sym pack) t))

(defmethod zunexport (sym pack)
  (unless (accessiblep sym pack)
    (error "~A is not accessible in ~A"
           sym pack))
  (setf (externalp sym pack) nil)
  t)

(defmethod zintern (sym-name pack)
  (or (zfind-symbol sym-name pack)
      (let ((sym (zmake-symbol sym-name)))
        (zimport (zmake-symbol sym-name) pack)
        sym)))

(defmethod zunintern (sym pack)
  (when (accessiblep sym pack)
    ;; Check for conflicts; are there any two used packages with EQUAL
    ;; names that are not EQ?
    (let ((name (zsymbol-name sym))
          (first-existing-sym nil))
      (dolist (used-pack (zpackage-use-list pack))
        (let ((existing-sym (zfind-symbol name used-pack)))
          (if first-existing-sym
              (unless (eq existing-sym first-existing-sym)
                (error "Conflict: uninterning ~A would lead to conflict ~
                      between ~A and ~A"
                       sym first-existing-sym existing-sym))
              (setf first-existing-sym existing-sym)))))
    ;; No conflict
    (setf (externalp sym pack) nil)
    (setf (shadowedp sym pack) nil)
    (setf (presentp sym pack) nil)
    (setf (%zsymbol-package pack) nil)
    t))

(defmethod zshadow (sym-name pack)
  (multiple-value-bind (existing-sym type)
      (zfind-symbol sym-name pack)
    (ecase type
      ((nil)
       (let ((sym (zmake-symbol sym-name)))
         (zimport sym pack)
         (setf (shadowedp sym pack) t)))
      ((:inherited)
       (zimport existing-sym pack)
       (setf (shadowedp existing-sym pack) t))
      ((:internal :external)
       (setf (shadowedp existing-sym pack) t)))
    t))

(defmethod zshadowing-import (sym pack)
  (let ((name (zsymbol-name sym)))
    (multiple-value-bind (existing-sym type)
        (zfind-symbol name pack)
      (ecase type
        ((nil :inherited)
         (zshadow name pack))
        ((:external :internal)
         (setf (shadowedp existing-sym pack) nil)
         (setf (externalp existing-sym pack) nil)
         (setf (presentp existing-sym pack) nil)
         (zimport sym pack)
         (zshadow sym pack))))))

(defmethod accessiblep (sym pack)
  (let ((existing-sym (zfind-symbol (zsymbol-name sym) pack)))
    (eq existing-sym sym)))

(defmethod inheritedp (sym pack)
  (eql (nth-value 1 (zfind-symbol (zsymbol-name sym) pack))
       :inherited))

(defmethod shadowedp (sym pack)
  (tmember sym (shadowed-table pack)))

(defmethod (setf shadowedp) (new-value sym pack)
  (let ((table (shadowed-table pack)))
    (if new-value
        (tensure sym table)
        (tremove-if-member sym table))))

(defmethod (setf externalp) (new-value sym pack)
  (let ((table (external-table pack)))
    (if new-value
        (tensure sym table)
        (tremove-if-member sym table))))

(defmethod (setf presentp) (new-value sym pack)
  (let ((table (present-table pack)))
    (if new-value
        (tensure sym table)
        (tremove-if-member sym table))))
