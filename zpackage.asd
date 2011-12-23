;;;; zpackage.asd

(asdf:defsystem #:zpackage
  :serial t
  :components ((:file "package")
               (:file "zpackage")
               (:file "sym-table")
               (:file "sym")
               (:file "pack")))

