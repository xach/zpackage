;;;; syntax.lisp

(in-package #:zpackage)

(defmethod square-bracket-reader (stream char)
  (declare (ignore char))
  (let ((string (with-output-to-string (string-stream)
                  (loop
                    (let ((char (read-char stream)))
                      (when (char= char #\])
                        (return))
                      (write-char (char-upcase char) string-stream))))))
    (let ((pack-name-end (position #\: string))
          (sym-name-start (position #\: string :from-end t)))
      (unless sym-name-start
        (error "Expected : in [] syntax"))
      (let ((pack-name (subseq string 0 pack-name-end))
            (sym-name (subseq string (1+ sym-name-start)))
            (externalp (= pack-name-end sym-name-start)))
        (let ((pack (zfind-package pack-name)))
          (unless pack
            (error "No pack named ~S" pack-name))
          (multiple-value-bind (sym type)
              (zfind-symbol sym-name pack)
            (if externalp
                (cond ((eql type :external)
                       sym)
                      ((null type)
                       (error "No symbol named ~S in ~A"
                              sym-name pack))
                      (t
                       (error "Symbol ~A is not external in ~A"
                              sym pack)))
                (cond ((null type)
                       (zintern sym-name pack))
                      (t
                       sym)))))))))
