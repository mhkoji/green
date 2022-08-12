(defpackage :green.cpu
  (:use :cl))
(in-package :green.cpu)

(defun int16-hi (int16)
  (assert (<= 0 int16 #xFFFF))
  (ash (logand int16 #xFF00) -8))

(defun int16-lo (int16)
  (assert (<= 0 int16 #xFFFF))
  (logand int16 #xFF))

(defun make-int16 (hi lo)
  (+ (ash hi 8) lo))

;;;

(defgeneric af-get (register-set))
(defgeneric af-set (register-set int16))
(defgeneric bc-get (register-set))
(defgeneric bc-set (register-set int16))
(defgeneric de-get (register-set))
(defgeneric de-set (register-set int16))
(defgeneric hl-get (register-set))
(defgeneric hl-set (register-set int16))
(defgeneric sp-get (register-set))
(defgeneric sp-set (register-set int16))
(defgeneric pc-get (register-set))
(defgeneric pc-inc (register-set &optional delta))

(defun a-get (register-set)
  (int16-hi (af-get register-set)))

(defun b-get (register-set)
  (int16-hi (bc-get register-set)))

(defun c-get (register-set)
  (int16-lo (bc-get register-set)))

(defun d-get (register-set)
  (int16-hi (de-get register-set)))

(defun e-get (register-set)
  (int16-lo (de-get register-set)))

(defun h-get (register-set)
  (int16-hi (hl-get register-set)))

(defun l-get (register-set)
  (int16-lo (hl-get register-set)))


(defun a-set (register-set a)
  (let ((f (int16-lo (af-get register-set))))
    (af-set register-set (make-int16 a f))))

(defun b-set (register-set b)
  (let ((c (c-get register-set)))
    (bc-set register-set (make-int16 b c))))

(defun c-set (register-set c)
  (let ((b (b-get register-set)))
    (bc-set register-set (make-int16 b c))))

(defun d-set (register-set d)
  (let ((e (e-get register-set)))
    (de-set register-set (make-int16 d e))))

(defun e-set (register-set e)
  (let ((d (d-get register-set)))
    (de-set register-set (make-int16 d e))))

(defun h-set (register-set h)
  (let ((l (l-get register-set)))
    (hl-set register-set (make-int16 h l))))

(defun l-set (register-set l)
  (let ((h (h-get register-set)))
    (hl-set register-set (make-int16 h l))))

;;;

(defgeneric mem8-get (memory addr))
(defgeneric mem8-set (memory addr int8))

;;;

(defgeneric run (cmd memory register-set))

(defvar +af+ :af)
(defvar +bc+ :bc)
(defvar +de+ :de)
(defvar +hl+ :hl)
(defvar +sp+ :sp)
(defvar +a+ :a)
(defvar +f+ :f)
(defvar +b+ :b)
(defvar +c+ :c)
(defvar +d+ :d)
(defvar +e+ :e)
(defvar +h+ :h)
(defvar +l+ :l)

(defun reg16-get (register-set reg16)
  (let ((getter (ecase reg16
                  (:bc #'bc-get)
                  (:de #'de-get)
                  (:hl #'hl-get))))
    (funcall getter register-set)))

(defun reg16-set (register-set reg16 int16)
  (let ((setter (ecase reg16
                  (:bc #'bc-set)
                  (:de #'de-set)
                  (:hl #'hl-set))))
    (funcall setter register-set int16)))

(defun reg8-get (register-set reg8)
  (let ((getter (ecase reg8
                  (:a #'a-get)
                  (:b #'b-get)
                  (:c #'c-get)
                  (:d #'d-get)
                  (:e #'e-get)
                  (:h #'h-get)
                  (:l #'l-get))))
    (funcall getter register-set)))

(defun reg8-set (register-set reg8 int8)
  (let ((setter (ecase reg8
                  (:a #'a-set)
                  (:b #'b-set)
                  (:c #'c-set)
                  (:d #'d-set)
                  (:e #'e-set)
                  (:h #'h-set)
                  (:l #'l-set))))
    (funcall setter register-set int8)))

(defstruct ldr8r8 x y)
(defstruct ldr8d8 r d)
(defstruct ldr8hl r)
(defstruct ldhlr8 r)
(defstruct ldabc)
(defstruct ldade)
(defstruct ldad16 d)

(defmethod run ((cmd ldr8r8) memory register-set)
  (with-slots (x y) cmd
    (reg8-set register-set x (reg8-get register-set y)))
  (pc-inc register-set))

(defmethod run ((cmd ldr8d8) memory register-set)
  (with-slots (x d) cmd
    (reg8-set register-set x d))
  (pc-inc register-set 2))

(defmethod run ((cmd ldr8hl) memory register-set)
  (with-slots (r) cmd
    (let ((addr (hl-get register-set)))
      (let ((int8 (mem8-get memory addr)))
        (reg8-set register-set r int8))))
  (pc-inc register-set))

(defmethod run ((cmd ldhlr8) memory register-set)
  (with-slots (r) cmd
    (let ((addr (hl-get register-set))
          (int8 (reg8-get register-set r)))
      (mem8-set memory addr int8)))
  (pc-inc register-set))

(defmethod run ((cmd ldabc) memory register-set)
  (let ((addr (bc-get register-set)))
    (let ((int8 (mem8-get memory addr)))
      (a-set register-set int8)))
  (pc-inc register-set))

(defmethod run ((cmd ldade) memory register-set)
  (let ((addr (de-get register-set))
        (int8 (a-get register-set)))
    (mem8-set memory addr int8))
  (pc-inc register-set))

(defmethod run ((cmd ldad16) memory register-set)
  (with-slots (d) cmd
    (a-set register-set (mem8-get memory d)))
  (pc-inc register-set 3))

;;;

(defstruct register-set
  af bc de hl sp pc)

(defmethod af-get ((set register-set))
  (register-set-af set))

(defmethod af-set ((set register-set) (int16 integer))
  (setf (register-set-af set) int16))

(defmethod bc-get ((set register-set))
  (register-set-bc set))

(defmethod bc-set ((set register-set) (int16 integer))
  (setf (register-set-bc set) int16))

(defmethod de-get ((set register-set))
  (register-set-de set))

(defmethod de-set ((set register-set) (int16 integer))
  (setf (register-set-de set) int16))

(defmethod hl-get ((set register-set))
  (register-set-hl set))

(defmethod hl-set ((set register-set) (int16 integer))
  (setf (register-set-hl set) int16))

(defmethod sp-get ((set register-set))
  (register-set-sp set))

(defmethod sp-set ((set register-set) (int16 integer))
  (setf (register-set-sp set) int16))

(defmethod pc-get ((set register-set))
  (register-set-pc set))
                   
(defmethod pc-inc ((set register-set) &optional (delta 1))
  (incf (register-set-pc set) delta))
