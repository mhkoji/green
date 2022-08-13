(defpackage :green.t
  (:use :cl))
(in-package :green.t)

(defmacro ldr8r8 (&key test)
  `(let ((set (green.cpu::make-register-set
               :pc 0
               :bc #x0000
               :de #x0012)))
     (green.cpu::run (green.cpu::make-ldr8r8 :x :b :y :e) nil set)
     (,test (= (green.cpu::register-set-bc set) #x1200))
     (,test (= (green.cpu::register-set-pc set) 1))))

;;;

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

(add-tests :command
 ldr8r8)
