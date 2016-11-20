(asdf:defsystem markdown.cl
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.7"
  :description "A markdown parser for Common Lisp"
  :depends-on (#:cl-ppcre #:xmls #:split-sequence)
  :components
  ((:file "package")
   (:file "util" :depends-on ("package"))
   (:file "html" :depends-on ("util"))
   (:file "parser" :depends-on ("html"))))

