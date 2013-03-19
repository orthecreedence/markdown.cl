(defpackage :markdown.cl-test
  (:use :cl :eos :markdown.cl)
  (:nicknames :markdown-test)
  (:export #:run-tests))
(in-package :markdown.cl-test)

;; TODO: test all functions in util package

(defun concat (&rest args)
  "Shortens string concatenation because I'm lazy and really who the hell wants
   to type out (concatenate 'string ...) seriously, I mispell concatentate like
   90% of the time I type it out."
  (apply #'concatenate (append '(string) args)))

(defun tree (xml-str)
  "Given a string of (rootless) HTML, generate and return a list tree (using
   xmls)."
  (xmls:parse (concat "<html>" xml-str "</html>")))

;; define the test suite
(def-suite markdown.cl-test :description "markdown.cl test suite")
