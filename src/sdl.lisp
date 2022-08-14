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

(defun main ()
  (sdl:with-init ()
    (sdl:window 640 480)
    (setf (sdl:frame-rate) 0)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
        (sdl:clear-display (sdl:color :r #x22 :g #x22 :b #x44))
        (let ((w 20) (h 20))
          (let* ((tile (green.rendering::make-tile
                        :bytes
                        '(#xFF #x00 #x7E #xFF
                          #x85 #x81 #x89 #x83
                          #x93 #x85 #xA5 #x8B
                          #xC9 #x97 #x7E #xFF)))
                 (pixels (green.rendering::tile->pixels tile)))
            (dotimes (i 8)
              (dotimes (j 8)
                (let ((x (+ (* j h) 100))
                      (y (+ (* i w) 100))
                      (color (pixel->color (aref pixels i j))))
                  (sdl:draw-box-* x y w h
                                  :color color
                                  :surface sdl:*default-display*))))))
        (sdl:update-display)))))
