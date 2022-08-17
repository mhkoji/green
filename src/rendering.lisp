(defpackage :green.rendering
  (:use :cl))
(in-package :green.rendering)

(defgeneric mem8-get (memory addr))

(defvar *tile-pixel-count* 8)

(defun bit-get (byte index)
  (assert (<= 0 byte #xFF))
  (assert (<= 0 index 7))
  (logand (ash byte (- index)) 1))

(defstruct tile memory addr)

(defmacro do-tile-pixels (((pixel i j) tile) &body body)
  `(with-accessors ((memory tile-memory)
                    (addr tile-addr)) ,tile
     (dotimes (,j *tile-pixel-count*)
       (let ((lo-byte (mem8-get memory (+ addr (* ,j 2))))
             (hi-byte (mem8-get memory (+ addr (* ,j 2) 1))))
         (dotimes (,i *tile-pixel-count*)
           (let* ((hi-bit (bit-get hi-byte (- 7 ,i)))
                  (lo-bit (bit-get lo-byte (- 7 ,i)))
                  (,pixel (logior (ash hi-bit 1) lo-bit)))
             (progn ,@body)))))))

(defun tile-pixels (tile)
  (let ((pixels (make-array '(8 8))))
    (do-tile-pixels ((pixel i j) tile)
      (setf (aref pixels j i) pixel))
    pixels))

(labels ((lcdc (memory)
           (mem8-get memory #xFF40)))

  (defun lcdc-tile-view-origin-win (memory)
    (if (= (bit-get (lcdc memory) 6) 1) #x9C00 #x9800))

  (defun lcdc-window-enabled-p (memory)
    (= (bit-get (lcdc memory) 5) 1))

  (defun lcdc-8000-p (memory)
    (= (bit-get (lcdc memory) 4) 1))

  (defun lcdc-tile-view-origin-bg (memory)
    (if (= (bit-get (lcdc memory) 3) 1) #x9C00 #x9800)))

;;;

(defun compute-tile-addr (tile-index $8000-addressing-p)
  (if (<= #x80 tile-index #xFF)
      (+ #x8800 (* 16 (- tile-index #x80)))
      (+ (if $8000-addressing-p #x8000 #x9000)
         (* 16 tile-index))))

(defun tile-addr-bg/win (tile-index memory)
  (compute-tile-addr tile-index (lcdc-8000-p memory)))

(defun tile-addr-sprite (tile-index)
  (compute-tile-addr tile-index t))

;;;

(defgeneric sync-tile (display tile x y))

(defun sync-bg/win (display memory view-origin)
  (dotimes (y 32)
    (dotimes (x 32)
      (let* ((view-index (+ (* y 32) x))
             (tile-index (mem8-get memory (+ view-origin view-index)))
             (tile-addr (tile-addr-bg/win tile-index memory))
             (tile (make-tile :memory memory :addr tile-addr)))
        (sync-tile display tile x y)))))

(defun sync (display memory)
  ;; TODO: impl
  (let ((tile (make-tile :memory memory :addr 0)))
    (green.rendering::sync-tile display tile 1 0)))
