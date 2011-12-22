;;;; zpackage.lisp

(in-package #:zpackage)

;;; "zpackage" goes here. Hacks and glory await!

;;; Clone of the CL symbol/package interface
(defgeneric zmake-symbol (sym-name))
(defgeneric zsymbol-name (sym))
(defgeneric zsymbol-package (sym))
(defgeneric (setf %zsymbol-package) (new-pack sym))

(defgeneric zfind-symbol (sym-name pack))
(defgeneric zimport (sym pack))
(defgeneric zexport (sym pack))
(defgeneric zunexport (sym pack))
(defgeneric zintern (sym-name pack))
(defgeneric zunintern (sym pack))
(defgeneric zshadow (sym-name pack))
(defgeneric zshadowing-import (sym pack))

(defgeneric zmake-package (pack-name))
(defgeneric zpackage-name (pack))
(defgeneric zfind-package (pack-name))
(defgeneric zuse-package (source-pack target-pack))
(defgeneric zunuse-package (source-pack target-pack))
(defgeneric zpackage-use-list (pack))
(defgeneric (setf %zpackage-use-list) (new-list pack))
(defgeneric zpackage-used-by-list (pack))
(defgeneric (setf %zpackage-used-by-list) (new-list pack))
(defgeneric zdelete-package (pack-name))


;;; Sym tables
(defgeneric make-sym-table ())
(defgeneric tget (sym-name table))
(defgeneric tput (sym table))
(defgeneric tremove (sym table))
(defgeneric tmember (sym table))
(defgeneric tensure (sym table))
(defgeneric tremove-if-member (sym table))
(defgeneric tmap-syms (fun table))


;;; Pack management
(defvar *packs* (make-hash-table :test 'equal))

(defgeneric present-table (pack))
(defgeneric shadowed-table (pack))
(defgeneric external-table (pack))

(defgeneric accessiblep (sym pack))
(defgeneric inheritedp (sym pack))

(defgeneric shadowedp (sym pack))
(defgeneric (setf shadowedp) (new-value sym pack))

(defgeneric externalp (sym pack))
(defgeneric (setf externalp) (new-value sym pack))

(defgeneric presentp (sym pack))
(defgeneric (setf presentp) (new-value sym pack))






(defmethod make-sym (name)
  (make-instance 'sym :name name))



(defgeneric presentp (sym table)
  (:documentation
   "If SYM is present in TABLE, return SYM, otherwise return NIL."))

(defgeneric lookup (name table)
  (:documentation
   "If NAME names any symbol in TABLE, return the symbol so named,
   otherwise return NIL. "))

(defgeneric add-symbol (sym table)
  (:documentation
   "Add SYM to TABLE. It is an error to add a symbol to the table if a
   distinct symbol by the same name is already in the table."))

(defgeneric ensure-symbol (sym table)
  (:documentation
   "Ensure that SYM is present in TABLE."))

(defgeneric remove-symbol (sym table)
  (:documentation
   "Remove SYM from TABLE."))

(defgeneric remove-symbol-if-present (sym table)
  (:documentation
   "Remove SYM from TABLE, but don't signal an error if it isn't
   present."))

(defgeneric map-symbols (fun table)
  (:documentation
   "Call FUN once for each sym in TABLE with that sym as its only
   argument."))

(defgeneric table-union (table1 table2)
  (:documentation
   "Return a fresh table representing the union of TABLE1 and
   TABLE2."))

(defgeneric table-difference (table1 table2)
  (:documentation
   "Return a fresh table representing the difference between TABLE1 and
   TABLE2, that is, with all the symbols in TABLE2 removed from
   TABLE1."))



(defclass symbol-table ()
  ((name-table
    :initarg :name-table
    :initform (make-hash-table :test 'equal)
    :accessor name-table)))

(defmethod lookup (name table)
  (gethash name (name-table table)))

(defmethod presentp (sym table)
  (let ((existing (lookup (sym-name sym) table)))
    (when (and existing (eq sym existing))
      sym)))

(defmethod add-symbol (sym table)
  (let* ((name (sym-name sym))
         (existing (lookup name table)))
    (when (and existing
               (not (eq existing sym)) table)
      (error "Adding ~A to ~A would conflict with ~A"
             sym table existing))
    (setf (gethash name (name-table table)) sym)))

(defmethod ensure-symbol (sym table)
  (or (presentp sym table)
      (add-symbol sym table)))

(defmethod remove-symbol (sym table)
  (unless (presentp sym table)
    (error "~A is not in ~A" sym table))
  (remhash (sym-name sym) (name-table table))
  sym)

(defmethod remove-symbol-if-present (sym table)
  (when (presentp sym table)
    (remove-symbol sym table)))

(defmethod map-syms (fun table)
  (maphash (lambda (name sym)
             (declare (ignore name))
             (funcall fun sym))
           (name-table table)))

(defmethod table-union (table1 table2)
  (let ((result (make-instance (class-of table1))))
    (flet ((add-to-result (sym)
             (add-symbol sym result)))
      (map-syms #'add-to-result table1)
      (map-syms #'add-to-result table2))
    result))

(defmethod table-difference (table1 table2)
  (let ((result (make-instance (class-of table1))))
    (map-syms (lambda (sym)
                (unless (presentp sym table2)
                  (add-symbol sym result)))
              table1)
    result))

(defvar *packs* (make-hash-table :test 'equal))

(defgeneric zfind-symbol (name pack))
(defgeneric zimport (sym pack))
(defgeneric zexport (sym pack))
(defgeneric zunexport (sym pack))
(defgeneric zintern (string pack))
(defgeneric zunintern (sym pack))
(defgeneric zshadow (string pack))
(defgeneric zshadowing-import (sym pack))

(defgeneric zuse-package (source-pack target-pack))
(defgeneric zunuse-package (source-pack target-pack))
(defgeneric zfind-package (name))
(defgeneric zmake-package (name))
(defgeneric zdelete-package (name))

(defgeneric accessiblep (sym pack))



(defmethod print-object ((pack pack) stream)
  (print-unreadable-object (pack stream :type t)
    (prin1 (name pack) stream)))

(defun find-external-sym (name pack)
  (lookup name (external pack)))

(defun find-shadowed-sym (name pack)
  (lookup name (shadowed pack)))

(defun find-inherited-sym (name pack)
  (some (lambda (used-pack)
          (find-external-sym name used-pack))
        (used-packs pack)))

(defun find-present-sym (name pack)
  (lookup name (present pack)))

(defun find-accessible-sym (name pack)
  (or (find-shadowed-sym name pack)
      (find-external-sym name pack)
      (find-inherited-sym name pack)
      (find-present-sym name pack)))

(defmethod zfind-package (name)
  (values (gethash name *packs*)))

(defmethod zmake-package (name)
  (when (zfind-package name)
    (error "~S already names a package" name))
  (setf (gethash name *packs*)
        (make-instance 'pack :name name)))

(defmethod zfind-symbol (name pack)
  (let (sym)
    (cond ((setf sym (find-external-sym name pack))
           (values sym :external))
          ((setf sym (find-shadowed-sym name pack))
           (values sym :internal))
          ((setf sym (find-inherited-sym name pack))
           (values sym :inherited))
          ((setf sym (find-present-sym name pack))
           (values sym :internal))
          (t
           (values nil nil)))))

(defmethod find-eq-symbol (sym pack)
  (multiple-value-bind (existing type)
      (zfind-symbol (sym-name sym) pack)
    (if (eq existing sym)
        (values sym type)
        (values nil nil))))

(defmethod zimport (sym pack)
  (let ((existing (zfind-symbol (sym-name sym) pack)))
    (when existing
      (unless (eq existing sym)
        (error "Importing ~A into ~A would cause a conflict with ~A"
               sym pack existing)))
    (unless (presentp sym (present pack))
      (add-symbol sym (present pack))
      (unless (sym-pack sym)
        (setf (%sym-pack sym) pack)))
    t))

(defmethod zexport (sym pack)
  (multiple-value-bind (existing type)
      (find-eq-symbol sym pack)
    (unless existing
      (error "~A is not accessible in ~A" sym pack))
    (let ((name (sym-name sym)))
      (dolist (used-pack (used-by-packs pack))
        (let ((existing (find-accessible-sym name used-pack)))
          (unless (eq existing sym)
            (error "Exporting ~A would cause a conflict with ~A in ~A"
                   sym existing used-pack)))))
    (ecase type
      ((:external))
      ((:internal)
       (add-symbol sym (external pack)))
      ((:inherited)
       (zimport sym pack)
       (add-symbol sym (external pack)))))
  t)

(defmethod zunexport (sym pack)
  (unless (find-eq-symbol sym pack)
    (error "~A is not accessible in ~A" sym pack))
  (when (presentp sym (external pack))
    (remove-symbol sym (external pack)))
  t)

(defmethod zintern (string pack)
  (or (zfind-symbol string pack)
      (let ((sym (make-sym string)))
        (zimport sym pack)
        sym)))

(defun find-name-conflict-in-packs (name packs)
  "Collect a list of (sym . pack) conses for all symbols named by NAME
  in PACKS."
  (let ((syms
         (loop for pack in packs
               for sym = (zfind-symbol name pack)
               when sym
               collect sym)))
    (let ((uniques (remove-duplicates syms)))
      (when (cdr uniques)
        uniques))))

(defun check-unintern-conflicts (sym pack)
  (let ((conflicts (find-name-conflict-in-packs (sym-name sym)
                                                (used-packs pack))))
    (when conflicts
      (error "Uninterning ~A would create conflicts: ~A"
             sym conflicts))))

(defun unintern-without-checks (sym pack)
  (when (eq (sym-pack sym) pack)
    (setf (%sym-pack sym) nil))
  (remove-symbol sym (present pack))
  (remove-symbol-if-present sym (external pack))
  (remove-symbol-if-present sym (shadowed pack)))

(defmethod zunintern (sym pack)
  (multiple-value-bind (existing type)
      (find-eq-symbol sym pack)
    (when (and existing
               (not (eq type :inherited)))
      (check-unintern-conflicts sym pack)
      (unintern-without-checks sym pack)
      t)))

(defmethod zshadow (string pack)
  (multiple-value-bind (existing type)
      (zfind-symbol string pack)
    (ecase type
      ((nil)
       (let ((sym (make-sym string)))
         (zimport sym pack)
         (add-symbol sym (shadowed pack))))
      ((:inherited)
       (zimport existing pack)
       (add-symbol existing (shadowed pack)))
      ((:internal :external)
       (ensure-symbol existing (shadowed pack))))
    t))

(defmethod zshadowing-import (sym pack)
  (let ((name (sym-name sym)))
    (multiple-value-bind (existing type)
        (zfind-symbol (sym-name sym) pack)
      (ecase type
        ((nil :inherited)
         (zshadow name pack))
        ((:external :internal)
         (unintern-without-checks existing pack)
         (zimport sym pack)
         (zshadow sym pack)))))
  t)

(defun external-conflicting-symbols (source-pack target-pack)
  (let ((conflicts '()))
    (map-syms (lambda (source-sym)
                (let ((target-sym (find-accessible-sym (sym-name source-sym)
                                                       target-pack)))
                  (when (and target-sym (not (eq source-sym target-sym)))
                    (push source-sym conflicts))))
              (external source-pack))
    conflicts))

(defmethod zuse-package (source-pack target-pack)
  (unless (member source-pack (used-packs target-pack))
    (let ((conflicts (external-conflicting-symbols source-pack target-pack)))
      (when conflicts
        (error "Using ~A would cause conflicts from the following symbols: ~A"
               source-pack conflicts)))
    (push source-pack (used-packs target-pack))
    (push target-pack (used-by-packs source-pack)))
  t)

(defmethod zunuse-package (source-pack target-pack)
  (setf (used-packs target-pack)
        (remove source-pack (used-packs target-pack)))
  (setf (used-by-packs source-pack)
        (remove target-pack (used-by-packs source-pack)))
  t)


;;;
;;; Debug
;;;

(defmethod describe-object :after ((pack pack) stream)
  (labels ((dump-table (description table)
             (write-line description stream)
             (map-syms (lambda (sym)
                         (format t "  ~A~%" sym))
                       table)))
    (dump-table "Present:" (present pack))
    (dump-table "Shadowed:" (shadowed pack))
    (dump-table "External:" (external pack))
    (format t "Used packs: ~A~%" (used-packs pack))
    (format t "Used-by packs: ~A~%" (used-by-packs pack))))
;;;
;;; Printing syms
;;;

(defgeneric pack-prefix (sym)
  (:method (sym)
    (let ((pack (sym-pack sym)))
      (cond ((null pack)
             "#:")
            ((presentp sym (external pack))
             (format nil "~A:" (name pack)))
            (t
             (format nil "~A::" (name pack)))))))

(defmethod print-object ((sym sym) stream)
  (print-unreadable-object (sym stream :type t :identity t)
    (format stream "~A~A" (pack-prefix sym) (sym-name sym))))
