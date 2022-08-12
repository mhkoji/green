(asdf:defsystem :green-test
  :serial t
  :pathname "t/"
  :components
  ((:file "fiveam"))
  :depends-on (:fiveam
               :green))
