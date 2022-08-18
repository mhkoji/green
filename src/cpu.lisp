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
  (logior (ash hi 8) lo))

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


(defun f-get (register-set)
  (int16-lo (af-get register-set)))

(defun f-set (register-set f)
  (let ((a (a-get register-set)))
    (af-set register-set (make-int16 a f))))

(defun f-update (register-set fn)
  (let ((new-f (funcall fn (f-get register-set))))
    (f-set register-set new-f)))

(defun zero-get (register-set)
  (let ((f (f-get register-set)))
    (= (logand f #b10000000) #b10000000)))

(defun zero-set (register-set zero-p)
  (f-update register-set
   (lambda (f)
     (if zero-p (logior f #b10000000) (logand f #b01111111)))))

(defun subtraction-get (register-set)
  (let ((f (f-get register-set)))
    (= (logand f #b01000000) #b01000000)))

(defun subtraction-set (register-set sub-p)
  (f-update register-set
   (lambda (f)
     (if sub-p (logior f #b01000000) (logand f #b10111111)))))

(defun half-carry-get (register-set)
  (let ((f (f-get register-set)))
    (= (logand f #b00100000) #b00100000)))

(defun half-carry-set (register-set half-carry-p)
  (f-update register-set
   (lambda (f)
     (if half-carry-p (logior f #b00100000) (logand f #b11011111)))))

(defun carry-get (register-set)
  (let ((f (f-get register-set)))
    (= (logand f #b00010000) #b00010000)))

(defun carry-set (register-set carry-p)
  (f-update register-set
   (lambda (f)
     (if carry-p (logior f #b00010000) (logand f #b11101111)))))

;;;

(defgeneric mem8-get (memory addr))
(defgeneric mem8-set (memory addr int8))
(defgeneric mem16-get (memory addr))
(defgeneric mem16-set (memory addr int16))

;;;

(defgeneric run (op memory register-set))

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

(let ((accessors
        `((,+af+ ,#'af-get ,#'af-set)
          (,+bc+ ,#'bc-get ,#'bc-set)
          (,+de+ ,#'de-get ,#'de-set)
          (,+hl+ ,#'hl-get ,#'hl-set)
          (,+sp+ ,#'sp-get ,#'sp-set))))
  (defun reg16-getter (reg16)
    (or (second (assoc reg16 accessors))
        (error "No such register: ~A" reg16)))
  (defun reg16-setter (reg16)
    (or (third (assoc reg16 accessors))
        (error "No such register: ~A" reg16))))

(let ((accessors
        `((,+a+ ,#'a-get ,#'a-set)
          (,+b+ ,#'b-get ,#'b-set)
          (,+c+ ,#'c-get ,#'c-set)
          (,+d+ ,#'d-get ,#'d-set)
          (,+e+ ,#'e-get ,#'e-set)
          (,+h+ ,#'h-get ,#'h-set)
          (,+l+ ,#'l-get ,#'l-set))))
  (defun reg8-getter (reg8)
    (or (second (assoc reg8 accessors))
        (error "No such register: ~A" reg8)))
  (defun reg8-setter (reg8)
    (or (third (assoc reg8 accessors))
        (error "No such register: ~A" reg8))))

(defun reg16-get (register-set reg16)
  (funcall (reg16-getter reg16) register-set))

(defun reg16-set (register-set reg16 int16)
  (funcall (reg16-setter reg16) register-set int16))

(defun reg8-get (register-set reg8)
  (funcall (reg8-getter reg8) register-set))

(defun reg8-set (register-set reg8 int8)
  (funcall (reg8-setter reg8) register-set int8))

(defgeneric place8-get (place))
(defgeneric place8-set (place int8))

(defun place8-add (place register-set int8)
  (let* ((old-val (place8-get place))
         (new-val (+ old-val int8)))
    (assert (<= 0 int8 #xFF))
    (assert (<= 0 old-val #xFF))
    (cond ((< #xFF new-val)
           (place8-set place (- new-val #x100))
           (zero-set register-set (= new-val #x100))
           (carry-set register-set t))
          (t
           (place8-set place new-val)
           (zero-set register-set nil)
           (carry-set register-set nil)))
    (half-carry-set register-set (and (<= old-val #x0F) (< #x0F new-val))))
  (subtraction-set register-set nil))

(defun place8-sub (place register-set int8)
  (let* ((old-val (place8-get place))
         (new-val (- old-val int8)))
    (assert (<= 0 int8 #xFF))
    (assert (<= 0 old-val #xFF))
    (cond ((= new-val 0)
           (place8-set place 0)
           (zero-set register-set t)
           (carry-set register-set nil))
          ((< new-val 0)
           (place8-set place (+ new-val #x100))
           (zero-set register-set nil)
           (carry-set register-set t))
          (t
           (place8-set place new-val)
           (zero-set register-set nil)
           (carry-set register-set nil)))
    (half-carry-set register-set (and (< #x0F old-val) (<= new-val #xF))))
  (subtraction-set register-set t))

(defstruct place8-reg register-set reg8)

(defmethod place8-get ((place place8-reg))
  (with-slots (register-set reg8) place
    (reg8-get register-set reg8)))

(defmethod place8-set ((place place8-reg) int8)
  (with-slots (register-set reg8) place
    (reg8-set register-set reg8 int8)))

(defstruct place8-mem memory addr)

(defmethod place8-get ((place place8-mem))
  (with-slots (memory addr) place
    (mem8-get memory addr)))

(defmethod place8-set ((place place8-mem) int8)
  (with-slots (memory addr) place
    (mem8-set memory addr int8)))

(defun reg8-add (register-set reg8 int8)
  (place8-add (make-place8-reg :register-set register-set :reg8 reg8)
              register-set
              int8))

(defun reg8-sub (register-set reg8 int8)
  (place8-sub (make-place8-reg :register-set register-set :reg8 reg8)
              register-set
              int8))

(defun reg8-and (register-set reg8 int8)
  (let ((val (logand (reg8-get register-set reg8) int8)))
    (reg8-set register-set reg8 val)
    (zero-set register-set (= val 0)))
  (subtraction-set register-set nil)
  (half-carry-set register-set t)
  (carry-set register-set nil))

(defun reg8-xor (register-set reg8 int8)
  (let ((val (logxor (reg8-get register-set reg8) int8)))
    (reg8-set register-set reg8 val)
    (zero-set register-set (= val 0)))
  (subtraction-set register-set nil)
  (half-carry-set register-set nil)
  (carry-set register-set nil))

(defun reg8-or (register-set reg8 int8)
  (let ((val (logior (reg8-get register-set reg8) int8)))
    (reg8-set register-set reg8 val)
    (zero-set register-set (= val 0)))
  (subtraction-set register-set nil)
  (half-carry-set register-set nil)
  (carry-set register-set nil))

(defun reg8-cp (register-set reg8 int8)
  (let* ((old-val (reg8-get register-set reg8))
         (new-val (- old-val int8)))
    (zero-set register-set (= new-val 0))
    (half-carry-set register-set (and (< #x0F old-val) (<= new-val #xF)))
    (carry-set register-set (< new-val 0)))
  (subtraction-set register-set t))

(defun reg8-dec (register-set reg8)
  (reg8-sub register-set reg8 1))

(defun cy-get (register-set)
  (if (carry-get register-set) 1 0))

(defstruct ldr8r8 x y)
(defstruct ldr8d8 r d)
(defstruct ldr8hl r)
(defstruct ldhlr8 r)
(defstruct ldhld8 d)
(defstruct ldabc)
(defstruct ldade)
(defstruct ldad16 d)
(defstruct ldbca)
(defstruct lddea)
(defstruct ldd16a d)
(defstruct ldaff00+d8 d)
(defstruct ldff00+d8a d)
(defstruct ldaff00+c)
(defstruct ldff00+ca)
(defstruct ldihla)
(defstruct ldiahl)
(defstruct lddhla)
(defstruct lddahl)

(defstruct ldr16d16 r d)
(defstruct ldd16sp d)
(defstruct ldsphl)
(defstruct pushr16 r)
(defstruct popr16 r)

(defstruct addr8r8 x y)
(defstruct addr8d8 r d)
(defstruct addr8hl r)
(defstruct adcr8r8 x y)
(defstruct adcr8d8 r d)
(defstruct adcr8hl r)
(defstruct subr8r8 x y)
(defstruct subr8d8 r d)
(defstruct subr8hl r)
(defstruct sbcr8r8 x y)
(defstruct sbcr8d8 r d)
(defstruct sbcr8hl r)
(defstruct andr8r8 x y)
(defstruct andr8d8 r d)
(defstruct andr8hl r)
(defstruct xorr8r8 x y)
(defstruct xorr8d8 r d)
(defstruct xorr8hl r)
(defstruct orr8r8 x y)
(defstruct orr8d8 r d)
(defstruct orr8hl r)
(defstruct cpr8r8 x y)
(defstruct cpr8d8 r d)
(defstruct cpr8hl r)
(defstruct incr8 r)
(defstruct inchl)
(defstruct decr8 r)
(defstruct dechl)
(defstruct daa)
(defstruct cpl)

(defstruct incr16 r)

(defun num->reg8 (num)
  (case num
    (0 +b+)
    (1 +c+)
    (2 +d+)
    (3 +e+)
    (4 +h+)
    (5 +l+)
    (7 +a+)))

(defun num->reg16 (num)
  (case num
    (0 +bc+)
    (1 +de+)
    (2 +hl+)
    (3 +sp+)))

(defun parse (memory addr)
  (let ((opcode (mem8-get memory addr)))
    (block nil
      (when (= (logand opcode #b11000000) #b01000000)
        (let ((x-reg (num->reg8 (ash (logand opcode #b00111000) -3)))
              (y-num (logand opcode #b00000111)))
          (when (and x-reg (= y-num #b110))
            (return (make-ldr8hl :r x-reg)))
          (let ((y-reg (num->reg8 (logand opcode #b00000111))))
            (when (and x-reg y-reg)
              (return (make-ldr8r8 :x x-reg :y y-reg))))))
      (when (= (logand opcode #b11000111) #b00000110)
        (let ((r (num->reg8 (ash (logand opcode #b00111000) -3))))
          (when r
            (return (make-ldr8d8 :r r :d (mem8-get memory (1+ addr)))))))
      (when (= (logand opcode #b11111000) #b01110000)
        (let ((r (num->reg8 (logand opcode #b00000111))))
          (when r
            (return (make-ldhlr8 :r r)))))
      (when (= opcode #b00110110)
        (return (make-ldhld8 :d (mem8-get memory (1+ addr)))))
      (when (= opcode #b00001010)
        (return (make-ldabc)))
      (when (= opcode #b00011010)
        (return (make-ldade)))
      (when (= opcode #b11111010)
        (let ((lo (mem8-get memory (+ addr 1)))
              (hi (mem8-get memory (+ addr 2))))
          (return (make-ldad16 :d (make-int16 hi lo)))))
      (when (= (logand opcode #b11001111) #b00000011)
        (let ((r (num->reg16 (ash (logand opcode #b00110000) -4))))
          (when r
            (return (make-incr16 :r r)))))
      (error "Invalid opcode: ~A" opcode))))

;;;

(defmethod run ((op ldr8r8) memory register-set)
  (with-slots (x y) op
    (reg8-set register-set x (reg8-get register-set y)))
  (pc-inc register-set))

(defmethod run ((op ldr8d8) memory register-set)
  (with-slots (r d) op
    (reg8-set register-set r d))
  (pc-inc register-set 2))

(defmethod run ((op ldr8hl) memory register-set)
  (with-slots (r) op
    (let ((addr (hl-get register-set)))
      (let ((int8 (mem8-get memory addr)))
        (reg8-set register-set r int8))))
  (pc-inc register-set))

(defmethod run ((op ldhlr8) memory register-set)
  (with-slots (r) op
    (let ((addr (hl-get register-set))
          (int8 (reg8-get register-set r)))
      (mem8-set memory addr int8)))
  (pc-inc register-set))

(defmethod run ((op ldhld8) memory register-set)
  (with-slots (d) op
    (let ((addr (hl-get register-set)))
      (mem8-set memory addr d)))
  (pc-inc register-set 2))

(defmethod run ((op ldabc) memory register-set)
  (let ((addr (bc-get register-set)))
    (let ((int8 (mem8-get memory addr)))
      (a-set register-set int8)))
  (pc-inc register-set))

(defmethod run ((op ldade) memory register-set)
  (let ((addr (de-get register-set)))
    (let ((int8 (mem8-get memory addr)))
      (a-set register-set int8)))
  (pc-inc register-set))

(defmethod run ((op ldad16) memory register-set)
  (with-slots (d) op
    (a-set register-set (mem8-get memory d)))
  (pc-inc register-set 3))

(defmethod run ((op ldbca) memory register-set)
  (let ((addr (bc-get register-set))
        (int8 (a-get register-set)))
    (mem8-set memory addr int8))
  (pc-inc register-set))

(defmethod run ((op lddea) memory register-set)
  (let ((addr (de-get register-set))
        (int8 (a-get register-set)))
    (mem8-set memory addr int8))
  (pc-inc register-set))

(defmethod run ((op ldd16a) memory register-set)
  (with-slots (d) op
    (mem8-set memory d (a-get register-set)))
  (pc-inc register-set 3))

(defmethod run ((op ldaff00+d8) memory register-set)
  (with-slots (d) op
    (let ((addr (make-int16 #xFF d)))
      (let ((int8 (mem8-get memory addr)))
        (a-set register-set int8))))
  (pc-inc register-set 2))

(defmethod run ((op ldff00+d8a) memory register-set)
  (with-slots (d) op
    (let ((addr (make-int16 #xFF d))
          (int8 (a-get register-set)))
      (mem8-set memory addr int8)))
  (pc-inc register-set 2))

(defmethod run ((op ldaff00+c) memory register-set)
  (let ((addr (make-int16 #xFF (c-get register-set))))
    (let ((int8 (mem8-get memory addr)))
      (a-set register-set int8)))
  (pc-inc register-set))

(defmethod run ((op ldff00+ca) memory register-set)
  (let ((addr (make-int16 #xFF (c-get register-set)))
        (int8 (a-get register-set)))
    (mem8-set memory addr int8))
  (pc-inc register-set))

(labels ((hl-update (register-set fn)
           (let ((int16 (funcall fn (hl-get register-set))))
             (hl-set register-set int16)))
         (hl-inc (register-set)
           (hl-update register-set
            (lambda (int16)
              (if (= int16 #xFFFF) #x0000 (1+ int16)))))
         (hl-dec (register-set)
           (hl-update register-set
            (lambda (int16)
              (if (= int16 #x0000) #xFFFF (1- int16))))))
  (defmethod run ((op ldihla) memory register-set)
    (let ((addr (hl-get register-set))
          (int8 (a-get register-set)))
      (mem8-set memory addr int8))
    (hl-inc register-set)
    (pc-inc register-set))

  (defmethod run ((op ldiahl) memory register-set)
    (let ((addr (hl-get register-set)))
      (let ((int8 (mem8-get memory addr)))
        (a-set register-set int8)))
    (hl-inc register-set)
    (pc-inc register-set))

  (defmethod run ((op lddhla) memory register-set)
    (let ((addr (hl-get register-set))
          (int8 (a-get register-set)))
      (mem8-set memory addr int8))
    (hl-dec register-set)
    (pc-inc register-set))

  (defmethod run ((op lddahl) memory register-set)
    (let ((addr (hl-get register-set)))
      (let ((int8 (mem8-get memory addr)))
        (a-set register-set int8)))
    (hl-dec register-set)
    (pc-inc register-set)))

(defmethod run ((op ldr16d16) memory register-set)
  (with-slots (r d) op
    (assert (member r (list +bc+ +de+ +hl+ +sp+) :test #'eql))
    (reg16-set register-set r d))
  (pc-inc register-set))

(defmethod run ((op ldd16sp) memory register-set)
  (with-slots (d) op
    (mem16-set memory d (sp-get register-set)))
  (pc-inc register-set))

(defmethod run ((op ldsphl) memory register-set)
  (sp-set register-set (hl-get register-set))
  (pc-inc register-set))

(labels ((sp-update (register-set fn)
           (let ((int16 (funcall fn (sp-get register-set))))
             (sp-set register-set int16)))
         (sp-inc (register-set)
           (sp-update register-set (lambda (int16) (+ int16 2))))
         (sp-dec (register-set)
           (sp-update register-set (lambda (int16) (- int16 2)))))
  (defmethod run ((op pushr16) memory register-set)
    (with-slots (r) op
      (assert (member r (list +af+ +bc+ +de+ +hl+) :test #'eql))
      (sp-dec register-set)
      (mem16-set memory (sp-get register-set) (reg16-get register-set r)))
    (pc-inc register-set))

  (defmethod run ((op pushr16) memory register-set)
    (with-slots (r) op
      (assert (member r (list +af+ +bc+ +de+ +hl+) :test #'eql))
      (reg16-set register-set r (mem16-get memory (sp-get register-set)))
      (sp-inc register-set))
    (pc-inc register-set)))

(defmethod run ((op addr8r8) memory register-set)
  (with-slots (x y) op
    (reg8-add register-set x (reg8-get register-set y)))
  (pc-inc register-set))

(defmethod run ((op addr8d8) memory register-set)
  (with-slots (r d) op
    (reg8-add register-set r d))
  (pc-inc register-set))

(defmethod run ((op addr8hl) memory register-set)
  (with-slots (r) op
    (let ((int8 (mem8-get memory (hl-get register-set))))
      (reg8-add register-set r int8)))
  (pc-inc register-set))

(defmethod run ((op adcr8r8) memory register-set)
  (with-slots (x y) op
    (let ((int8 (+ (reg8-get register-set y) (cy-get register-set))))
      (reg8-add register-set x int8)))
  (pc-inc register-set))

(defmethod run ((op adcr8d8) memory register-set)
  (with-slots (r d) op
    (reg8-add register-set r (+ d (cy-get register-set))))
  (pc-inc register-set))

(defmethod run ((op adcr8hl) memory register-set)
  (with-slots (r) op
    (let ((int8 (+  (mem8-get memory (hl-get register-set))
                    (cy-get register-set))))
      (reg8-add register-set r int8)))
  (pc-inc register-set))

(defmethod run ((op subr8r8) memory register-set)
  (with-slots (x y) op
    (reg8-sub register-set x (reg8-get register-set y)))
  (pc-inc register-set))

(defmethod run ((op subr8d8) memory register-set)
  (with-slots (r d) op
    (reg8-sub register-set r d))
  (pc-inc register-set))

(defmethod run ((op subr8hl) memory register-set)
  (with-slots (r) op
    (let ((int8 (mem8-get memory (hl-get register-set))))
      (reg8-sub register-set r int8)))
  (pc-inc register-set))

(defmethod run ((op sbcr8r8) memory register-set)
  (with-slots (x y) op
    (let ((int8 (+ (reg8-get register-set y) (cy-get register-set))))
      (reg8-sub register-set x int8)))
  (pc-inc register-set))

(defmethod run ((op sbcr8d8) memory register-set)
  (with-slots (r d) op
    (reg8-sub register-set r (+ d (cy-get register-set))))
  (pc-inc register-set))

(defmethod run ((op sbcr8hl) memory register-set)
  (with-slots (r) op
    (let ((int8 (+  (mem8-get memory (hl-get register-set))
                    (cy-get register-set))))
      (reg8-sub register-set r int8)))
  (pc-inc register-set))

(defmethod run ((op andr8r8) memory register-set)
  (with-slots (x y) op
    (reg8-and register-set x (reg8-get register-set y)))
  (pc-inc register-set))

(defmethod run ((op andr8d8) memory register-set)
  (with-slots (r d) op
    (reg8-and register-set r d))
  (pc-inc register-set))

(defmethod run ((op andr8hl) memory register-set)
  (with-slots (r) op
    (let ((int8 (mem8-get memory (hl-get register-set))))
      (reg8-and register-set r int8)))
  (pc-inc register-set))

(defmethod run ((op xorr8r8) memory register-set)
  (with-slots (x y) op
    (reg8-xor register-set x (reg8-get register-set y)))
  (pc-inc register-set))

(defmethod run ((op xorr8d8) memory register-set)
  (with-slots (r d) op
    (reg8-xor register-set r d))
  (pc-inc register-set))

(defmethod run ((op xorr8hl) memory register-set)
  (with-slots (r) op
    (let ((int8 (mem8-get memory (hl-get register-set))))
      (reg8-xor register-set r int8)))
  (pc-inc register-set))

(defmethod run ((op orr8r8) memory register-set)
  (with-slots (x y) op
    (reg8-or register-set x (reg8-get register-set y)))
  (pc-inc register-set))

(defmethod run ((op orr8d8) memory register-set)
  (with-slots (r d) op
    (reg8-or register-set r d))
  (pc-inc register-set))

(defmethod run ((op orr8hl) memory register-set)
  (with-slots (r) op
    (let ((int8 (mem8-get memory (hl-get register-set))))
      (reg8-or register-set r int8)))
  (pc-inc register-set))

(defmethod run ((op cpr8r8) memory register-set)
  (with-slots (x y) op
    (reg8-cp register-set x (reg8-get register-set y)))
  (pc-inc register-set))

(defmethod run ((op cpr8d8) memory register-set)
  (with-slots (r d) op
    (reg8-cp register-set r d))
  (pc-inc register-set))

(defmethod run ((op cpr8hl) memory register-set)
  (with-slots (r) op
    (let ((int8 (mem8-get memory (hl-get register-set))))
      (reg8-cp register-set r int8)))
  (pc-inc register-set))

(defmethod run ((op incr8) memory register-set)
  (with-slots (r) op
    (reg8-add register-set r 1))
  (pc-inc register-set))

(defmethod run ((op inchl) memory register-set)
  (with-slots (r) op
    (place8-add (make-place8-mem :memory memory :addr (hl-get register-set))
                register-set
                1))
  (pc-inc register-set))

(defmethod run ((op decr8) memory register-set)
  (with-slots (r) op
    (reg8-sub register-set r 1))
  (pc-inc register-set))

(defmethod run ((op dechl) memory register-set)
  (with-slots (r) op
    (place8-sub (make-place8-mem :memory memory :addr (hl-get register-set))
                register-set
                1))
  (pc-inc register-set))

(defmethod run ((op daa) memory register-set)
  (when (or (< 9 (f-get register-set))
            (half-carry-get register-set))
    (let* ((old-f (f-get register-set))
           (new-f (+ old-f 6)))
      (cond ((< new-f #xFF)
             (f-set register-set (- new-f #x100))
             (carry-set register-set t))
            (t
             (f-set register-set new-f)
             (carry-set register-set nil)))
      (zero-set register-set (= new-f #x100))))
  (when (or (< 9 (a-get register-set))
            (carry-get register-set))
    (let* ((old-a (a-get register-set))
           (new-a (+ old-a 6)))
      (cond ((< new-a #xFF)
             (a-set register-set (- new-a #x100))
             (carry-set register-set t))
            (t
             (f-set register-set new-a)
             (carry-set register-set nil)))
      (zero-set register-set (= new-a #x100))))
  (pc-inc register-set))

(defmethod run ((op cpl) memory register-set)
  (let ((a (a-get register-set)))
    (a-set register-set (logxor a #xFF)))
  (subtraction-set register-set t)
  (half-carry-set register-set t)
  (pc-inc register-set))

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
