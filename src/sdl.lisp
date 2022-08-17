(defpackage :green.sdl
  (:use :cl)
  (:export :main))
(in-package :green.sdl)

(defun pixel->color (p)
  (cond ((= p 0)
         (sdl:color :r #xff :g #xff :b #xff))
        ((= p 1)
         #+nil (sdl:color :r #xd3 :g #xd3 :b #xd3)
         (sdl:color :r #xaa :g #xaa :b #xaa))
        ((= p 2)
         #+nil (sdl:color :r #xa9 :g #xa9 :b #xa9)
         (sdl:color :r #x55 :g #x55 :b #x55))
        ((= p 3)
         (sdl:color :r #x00 :g #x00 :b #x00))))

;;;

(defstruct display w h surface)

(defmethod green.rendering::sync-tile ((display display) tile x y)
  (with-accessors ((w display-w)
                   (h display-h)
                   (surface display-surface)) display
    (green.rendering::do-tile-pixels ((pixel i j) tile)
      (let ((pos-x (+ (* x green.rendering::*tile-pixel-count*) i))
            (pos-y (+ (* y green.rendering::*tile-pixel-count*) j))
            (color (pixel->color pixel)))
        (sdl:draw-box-* (* pos-x w) (* pos-y h) w h
                        :color color :surface surface)))))

;;;

(defmethod green.rendering::mem8-get ((memory list) (addr integer))
  (nth addr memory))


(defun main ()
  (sdl:with-init ()
    (sdl:window 640 480)
    (setf (sdl:frame-rate) 0)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
        (sdl:clear-display (sdl:color :r #x22 :g #x22 :b #x44))
        (let ((display (make-display
                        :w 20 :h 20 :surface sdl:*default-display*))
              (memory '(#xFF #x00 #x7E #xFF
                        #x85 #x81 #x89 #x83
                        #x93 #x85 #xA5 #x8B
                        #xC9 #x97 #x7E #xFF)))
          (green.rendering::sync display memory))
        (sdl:update-display)))))
