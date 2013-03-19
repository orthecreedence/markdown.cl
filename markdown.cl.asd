(asdf:defsystem markdown.cl
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.0.7"
  :description "A markdown parser for Common Lisp"
  :depends-on (#:cl-ppcre)
  :components
  ((:file "package")
   (:file "util" :depends-on ("package"))
   (:file "parser" :depends-on ("util"))))

