(defpackage :green.t
  (:use :cl))
(in-package :green.t)

(defmethod green.cpu::mem8-get ((mem list) (addr integer))
  (nth addr mem))

(defmethod green.cpu::mem8-set ((mem list) (addr integer) (int8 integer))
  (setf (nth addr mem) int8))

(defmethod green.cpu::mem8-get ((mem hash-table) (addr integer))
  (gethash addr mem))

(defmethod green.cpu::mem8-set ((mem hash-table)
                                (addr integer) (int8 integer))
  (setf (gethash addr mem) int8))

(defmacro ldr8r8 (&key test)
  `(let ((set (green.cpu::make-register-set
               :pc 0
               :bc #x0000
               :de #x0012)))
     (green.cpu::run (green.cpu::make-ldr8r8 :x :b :y :e) nil set)
     (,test (= (green.cpu::b-get set) #x12))
     (,test (= (green.cpu::pc-get set) 1))))

(defmacro ldr8d8 (&key test)
  `(let ((set (green.cpu::make-register-set
               :pc 0
               :bc #x0000)))
     (green.cpu::run (green.cpu::make-ldr8d8 :r :b :d #x12) nil set)
     (,test (= (green.cpu::b-get set) #x12))
     (,test (= (green.cpu::pc-get set) 2))))

(defmacro ldr8hl (&key test)
  `(let ((set (green.cpu::make-register-set
               :pc 0
               :bc #x0000
               :hl #x0000)))
     (green.cpu::run (green.cpu::make-ldr8hl :r :b) (list #x12) set)
     (,test (= (green.cpu::b-get set) #x12))
     (,test (= (green.cpu::pc-get set) 1))))

(defmacro ldhlr8 (&key test)
  `(let ((mem (list #x00))
         (set (green.cpu::make-register-set
               :pc 0
               :bc #x1200
               :hl #x0000)))
     (green.cpu::run (green.cpu::make-ldhlr8 :r :b) mem set)
     (,test (= (nth 0 mem) #x12))
     (,test (= (green.cpu::pc-get set) 1))))

(defmacro ldhld8 (&key test)
  `(let ((mem (list #x00))
         (set (green.cpu::make-register-set
               :pc 0
               :hl #x0000)))
     (green.cpu::run (green.cpu::make-ldhld8 :d #x12) mem set)
     (,test (= (nth 0 mem) #x12))
     (,test (= (green.cpu::pc-get set) 2))))

(defmacro ldabc (&key test)
  `(let ((mem (list #x12))
         (set (green.cpu::make-register-set
               :pc 0
               :af #x0000
               :bc #x0000)))
     (green.cpu::run (green.cpu::make-ldabc) mem set)
     (,test (= (green.cpu::a-get set) #x12))
     (,test (= (green.cpu::pc-get set) 1))))

(defmacro ldade (&key test)
  `(let ((mem (list #x12))
         (set (green.cpu::make-register-set
               :pc 0
               :af #x0000
               :de #x0000)))
     (green.cpu::run (green.cpu::make-ldade) mem set)
     (,test (= (green.cpu::a-get set) #x12))
     (,test (= (green.cpu::pc-get set) 1))))

(defmacro ldad16 (&key test)
  `(let ((mem (list #x12))
         (set (green.cpu::make-register-set
               :pc 0
               :af #x0000)))
     (green.cpu::run (green.cpu::make-ldad16 :d #x0000) mem set)
     (,test (= (green.cpu::a-get set) #x12))
     (,test (= (green.cpu::pc-get set) 3))))

(defmacro ldbca (&key test)
  `(let ((mem (list #x00))
         (set (green.cpu::make-register-set
               :pc 0
               :bc #x0000
               :af #x1200)))
     (green.cpu::run (green.cpu::make-ldbca) mem set)
     (,test (= (nth 0 mem) #x12))
     (,test (= (green.cpu::pc-get set) 1))))

(defmacro lddea (&key test)
  `(let ((mem (list #x00))
         (set (green.cpu::make-register-set
               :pc 0
               :de #x0000
               :af #x1200)))
     (green.cpu::run (green.cpu::make-lddea) mem set)
     (,test (= (nth 0 mem) #x12))
     (,test (= (green.cpu::pc-get set) 1))))

(defmacro ldd16a (&key test)
  `(let ((mem (list #x00))
         (set (green.cpu::make-register-set
               :pc 0
               :af #x1200)))
     (green.cpu::run (green.cpu::make-ldd16a :d #x0000) mem set)
     (,test (= (nth 0 mem) #x12))
     (,test (= (green.cpu::pc-get set) 3))))

(defmacro ldaff00+d8 (&key test)
  `(let ((mem (make-hash-table :test #'equal))
         (set (green.cpu::make-register-set
               :pc 0
               :af #x0000)))
     (green.cpu::mem8-set mem #xFF01 #x12)
     (green.cpu::run (green.cpu::make-ldaff00+d8 :d #x01) mem set)
     (,test (= (green.cpu::a-get set) #x12))
     (,test (= (green.cpu::pc-get set) 2))))

(defmacro ldff00+d8a (&key test)
  `(let ((mem (make-hash-table :test #'equal))
         (set (green.cpu::make-register-set
               :pc 0
               :af #x1200)))
     (green.cpu::run (green.cpu::make-ldff00+d8a :d #x01) mem set)
     (,test (= (green.cpu::mem8-get mem #xFF01) #x12))
     (,test (= (green.cpu::pc-get set) 2))))

(defmacro ldaff00+c (&key test)
  `(let ((mem (make-hash-table :test #'equal))
         (set (green.cpu::make-register-set
               :pc 0
               :bc #x0001
               :af #x0000)))
     (green.cpu::mem8-set mem #xFF01 #x12)
     (green.cpu::run (green.cpu::make-ldaff00+c) mem set)
     (,test (= (green.cpu::a-get set) #x12))
     (,test (= (green.cpu::pc-get set) 1))))

(defmacro ldff00+ca (&key test)
  `(let ((mem (make-hash-table :test #'equal))
         (set (green.cpu::make-register-set
               :pc 0
               :bc #x0001
               :af #x1200)))
     (green.cpu::run (green.cpu::make-ldff00+ca) mem set)
     (,test (= (green.cpu::mem8-get mem #xFF01) #x12))
     (,test (= (green.cpu::pc-get set) 1))))

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

(add-tests
 :command
 ldr8r8
 ldr8d8
 ldr8hl
 ldhlr8
 ldhld8
 ldabc
 ldade
 ldad16
 ldbca
 lddea
 ldd16a
 ldaff00+d8
 ldff00+d8a
 ldaff00+c
 ldff00+ca)
