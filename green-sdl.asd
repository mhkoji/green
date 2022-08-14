(asdf:defsystem :green-sdl
  :serial t
  :pathname "src/"
  :components
  ((:file "sdl"))
  :depends-on (:green
               :lispbuilder-sdl))
