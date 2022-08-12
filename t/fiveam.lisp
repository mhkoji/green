(defpackage :green.t
  (:use :cl))
(in-package :green.t)

(fiveam:def-suite* :green)

(fiveam:test ldr8r8
  (let ((set (green.cpu::make-register-set
              :pc 0
              :bc #x0000
              :de #x0012)))
    (green.cpu::run (green.cpu::make-ldr8r8 :x :b :y :e) nil set)
    (fiveam:is (= (green.cpu::register-set-bc set) #x1200))
    (fiveam:is (= (green.cpu::register-set-pc set) 1))))
