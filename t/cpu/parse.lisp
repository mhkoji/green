(defpackage :green.t.cpu.parse
  (:use :cl))
(in-package :green.t.cpu.parse)

(defmacro ldr8r8 (&key test)
  `(let ((cmd (green.cpu::parse (list #b01001011) 0)))
     (,test (eq (type-of cmd) 'green.cpu::ldr8r8))
     (,test (eql (green.cpu::ldr8r8-x cmd) green.cpu::+c+))
     (,test (eql (green.cpu::ldr8r8-y cmd) green.cpu::+e+))))

(defmacro ldr8d8 (&key test)
  `(let ((cmd (green.cpu::parse (list #b00001110 #x12) 0)))
     (,test (eq (type-of cmd) 'green.cpu::ldr8d8))
     (,test (eql (green.cpu::ldr8d8-r cmd) green.cpu::+c+))
     (,test (= (green.cpu::ldr8d8-d cmd) #x12))))

(defmacro ldr8hl (&key test)
  `(let ((cmd (green.cpu::parse (list #b01001110) 0)))
     (,test (eq (type-of cmd) 'green.cpu::ldr8hl))
     (,test (eql (green.cpu::ldr8hl-r cmd) green.cpu::+c+))))

(defmacro ldhlr8 (&key test)
  `(let ((cmd (green.cpu::parse (list #b01110001) 0)))
     (,test (eq (type-of cmd) 'green.cpu::ldhlr8))
     (,test (eql (green.cpu::ldhlr8-r cmd) green.cpu::+c+))))

(defmacro ldhld8 (&key test)
  `(let ((cmd (green.cpu::parse (list #b00110110 #x12) 0)))
     (,test (eq (type-of cmd) 'green.cpu::ldhld8))
     (,test (eql (green.cpu::ldhld8-d cmd) #x12))))

(defmacro ldabc (&key test)
  `(let ((cmd (green.cpu::parse (list #b00001010) 0)))
     (,test (eq (type-of cmd) 'green.cpu::ldabc))))

(defmacro ldade (&key test)
  `(let ((cmd (green.cpu::parse (list #b00011010) 0)))
     (,test (eq (type-of cmd) 'green.cpu::ldade))))

(defmacro lda16 (&key test)
  `(let ((cmd (green.cpu::parse (list #b11111010 #x12 #x34) 0)))
     (,test (eq (type-of cmd) 'green.cpu::ldad16))
     (,test (eql (green.cpu::ldad16-d cmd) #x3412))))

(green.t:add-tests
 :cpu.parse
 ldr8r8
 ldr8d8
 ldr8hl
 ldhlr8
 ldhld8
 ldabc
 ldade
 lda16)
