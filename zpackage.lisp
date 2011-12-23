;;;; zpackage.lisp

(in-package #:zpackage)

;;; "zpackage" goes here. Hacks and glory await!

;;; Clone of the CL symbol/package interface
(defgeneric zmake-symbol (sym-name))
(defgeneric zsymbol-name (sym))
(defgeneric zsymbol-package (sym))

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
(defgeneric zdelete-package (pack-name))

(defgeneric zuse-package (pack using-pack))
(defgeneric zunuse-package (pack using-pack))
(defgeneric zpackage-use-list (pack))
(defgeneric zpackage-used-by-list (pack))

;;; Needed for implementation
(defgeneric (setf %zsymbol-package) (new-pack sym))
(defgeneric (setf %zpackage-use-list) (new-list pack))
(defgeneric (setf %zpackage-used-by-list) (new-list pack))

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
(defgeneric externalp (sym pack))
(defgeneric shadowedp (sym pack))
(defgeneric presentp (sym pack))

(defgeneric check-import-conflict (sym pack))
(defgeneric check-inherit-conflict (used-pack using-pack))
(defgeneric check-unintern-conflict (sym-name pack))
(defgeneric check-export-conflict (sym pack))
