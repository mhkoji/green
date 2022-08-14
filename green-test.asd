(asdf:defsystem :green-test
  :serial t
  :pathname "t/"
  :components
  ((:file "fiveam")
   (:file "cpu")
   (:file "rendering"))
  :depends-on (:fiveam
               :green))
