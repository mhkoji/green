(asdf:defsystem :green-test
  :serial t
  :pathname "t/"
  :components
  ((:file "fiveam")
   (:file "cpu"))
  :depends-on (:fiveam
               :green))
