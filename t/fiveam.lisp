(defpackage :green.t
  (:use :cl)
  (:export :add-tests))
(in-package :green.t)

(fiveam:def-suite* :green.t)

(defmacro add-tests (name &rest syms)
  (let ((full-name (intern (concatenate 'string
                            "GREEN.T." (string-upcase name))
                           :keyword)))
    `(progn
       (fiveam:def-suite ,full-name :in :green.t)
       ,@(mapcar (lambda (sym)
                   `(fiveam:def-test ,sym (:suite ,full-name)
                      (,sym :test fiveam:is)))
                 syms))))
