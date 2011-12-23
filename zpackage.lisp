;;;; zpackage.lisp

(in-package #:zpackage)

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

