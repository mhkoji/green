(asdf:defsystem :green-test
  :serial t
  :pathname "t/"
  :components
  ((:file "fiveam")
   (:file "cpu/run")
   (:file "cpu/parse")
   (:file "rendering"))
  :depends-on (:fiveam
               :green))
