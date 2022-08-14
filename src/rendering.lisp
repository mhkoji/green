(defpackage :green.rendering
  (:use :cl))
(in-package :green.rendering)

(defstruct tile
  bytes) ;; data of 16 bytes to represent the pixels in the tile

(defun bit-get (byte index)
  (assert (<= 0 byte #xFF))
  (logand (ash byte (- index)) 1))

(defun tile->pixels (tile)
  (let ((bytes (tile-bytes tile))
        (pixels (make-array '(8 8))))
    (loop for i from 0 to 14 by 2
          for lo-byte = (nth i bytes)
          for hi-byte = (nth (1+ i) bytes) do
      (loop for j from 7 downto 0
            for hi-bit = (bit-get hi-byte j)
            for lo-bit = (bit-get lo-byte j)
            for pixel = (logior (ash hi-bit 1) lo-bit) do
        (setf (aref pixels (/ i 2) (- 7 j)) pixel)))
    pixels))
