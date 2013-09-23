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
  (let* ((str (cl-ppcre:regex-replace-all "&(?!(#[0-9]+|amp);)" xml-str "&amp;")))
    (xmls:parse (concat "<html>" str "</html>"))))

(defun tree-diff (tree1 tree2)
  "Recursively find the first difference between two trees. If none found,
   return nil, otherwise return the two differing as a cons. Only works on trees
   with strings/symbols/numbers."
  (cond
    ((equalp tree1 tree2)
     nil)
    ((and (listp tree1)
          (listp tree2))
     (loop for n1 in tree1
           for n2 in tree2 do
       (let ((diff (tree-diff n1 n2)))
         (when diff
           (return-from tree-diff (list tree1 diff))))))
    ((equal tree1 tree2)
     nil)
    (t
     (list tree1 tree2))))

;; define the test suite
(def-suite markdown.cl-test :description "markdown.cl test suite")


