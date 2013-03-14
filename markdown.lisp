(in-package :markdown)

;; -----------------------------------------------------------------------------
;; some general regexes
;; -----------------------------------------------------------------------------
(defparameter *nl* (coerce #(#\newline) 'string)
  "Holds a string of a single newline character.")

(defparameter *scanner-replace-newlines*
  (cl-ppcre:create-scanner "[\\r\\n]" :single-line-mode t)
  "A scanner to replace any \r or \r\n with \n.")

;; -----------------------------------------------------------------------------
;; block parsing
;; -----------------------------------------------------------------------------
(defparameter *scanner-split-blocks*
  (cl-ppcre:create-scanner "(?<=\\n\\n)(?!( {4,}|\t))" :case-insensitive-mode t :single-line-mode t)
  "A scanner for splitting a markdown document into a set of blocks.")

(defun split-blocks (str)
  (cl-ppcre:split *scanner-split-blocks* str))

;; -----------------------------------------------------------------------------
;; entity parsing
;; -----------------------------------------------------------------------------
(defun parse-entities (str)
  "Replace non-purposeful entities with excaped equivalents."
  (let* ((str (cl-ppcre:regex-replace-all "&(?![a-z]{2,6};)" str "&amp;"))
         (str (cl-ppcre:regex-replace-all "<(?!/?[a-z0-9]+(\s?[a-z]+=\"[^\\\"]+\")*>)" str "&lt;"))
         (str (cl-ppcre:regex-replace-all "(</?[a-z0-9]+(\s?[a-z]+=\"[^\\\"]+\")*)>" str "\\1&gtLOL;"))
         (str (cl-ppcre:regex-replace-all ">" str "&gt;"))
         (str (cl-ppcre:regex-replace-all "&gtLOL;" str ">")))
    str))

;; -----------------------------------------------------------------------------
;; code formatting
;; -----------------------------------------------------------------------------
(defparameter *scanner-format-code*
  (cl-ppcre:create-scanner "^( {4}|\\t)" :case-insensitive-mode t :single-line-mode t)
  "A scanner for sanely formatting code.")

(defun format-code (str)
  (cl-ppcre:regex-replace-all *scanner-format-code* str ""))

(defparameter *scanner-code*
  (cl-ppcre:create-scanner "(?<=\\n\\n)((\\n( {4,}|\\t+)[^\\n]*)+(?=\\n))" :case-insensitive-mode t :single-line-mode t)
  "A scanner for code blocks")

(defun parse-code (str)
  (cl-ppcre:regex-replace-all *scanner-code* str
    (lambda (match &rest regs)
      (declare (ignore match))
      (let* ((regs (cddddr regs))
             (rs (car regs))
             (re (cadr regs))
             (text (subseq str (aref rs 0) (aref re 0))))
        (concatenate 'string *nl* "<pre><code>" (format-code text) *nl* "</code></pre>")))))

;; -----------------------------------------------------------------------------
;; blockquote formatting
;; -----------------------------------------------------------------------------
(defparameter *scanner-format-blockquote*
  (cl-ppcre:create-scanner ">\\s?" :multi-line-mode t)
  "A scanner for sanely formatting blockquotes.")

(defparameter *scanner-lazy-blockquote*
  (cl-ppcre:create-scanner "(\\n>.*?\\n(?=\\n(?!>)))" :case-insensitive-mode t :single-line-mode t)
  "A scanner for finding blockquotes.")

(defparameter *scanner-blockquote*
  (cl-ppcre:create-scanner "((\\n>[^\\n]*)+(?=\\n))" :case-insensitive-mode t :single-line-mode t)
  "A scanner for finding blockquotes.")

(defun format-blockquote (str)
  (multiple-value-bind (start end)
      (cl-ppcre:scan *scanner-blockquote* str)
    (format t "lazy: ~a ~a~%" (- end start) (length str))
    (let* ((str (cl-ppcre:regex-replace-all *scanner-format-blockquote* str "")))
      (if start
          str
          (parse-string str :disable-parsers '(parse-lazy-blockquote))))))

(defun parse-lazy-blockquote (str)
  (cl-ppcre:regex-replace-all *scanner-lazy-blockquote* str
    (lambda (match &rest regs)
      (declare (ignore match))
      (let* ((regs (cddddr regs))
             (rs (car regs))
             (re (cadr regs))
             (text (subseq str (aref rs 0) (aref re 0))))
        (concatenate 'string *nl* (format-blockquote text) *nl*)))))

(defun parse-blockquote (str)
  (cl-ppcre:regex-replace-all *scanner-blockquote* str
    (lambda (match &rest regs)
      (declare (ignore match))
      (let* ((regs (cddddr regs))
             (rs (car regs))
             (re (cadr regs))
             (text (subseq str (aref rs 0) (aref re 0))))
        (concatenate 'string *nl* (format-blockquote text)  *nl*)))))

;; -----------------------------------------------------------------------------
;; anchor formatting
;; -----------------------------------------------------------------------------
(defun parse-links (str)
  str)

;; -----------------------------------------------------------------------------
;; paragraph formatting
;; -----------------------------------------------------------------------------
(defun parse-paragraphs (str)
  str)

;; -----------------------------------------------------------------------------
;; header formatting
;; -----------------------------------------------------------------------------
(defparameter *scanner-setext-header*
  (cl-ppcre:create-scanner "\\n\\s{0,3}([^\\n]+?)\\n(=+|-+)\\n" :case-insensitive-mode t :single-line-mode t)
  "A scanner that finds setext-style headers.")

(defun parse-setext-headers (str)
  (cl-ppcre:regex-replace-all *scanner-setext-header* str
    (lambda (match &rest regs)
      (declare (ignore match))
      (let* ((regs (cddddr regs))
             (rs (car regs))
             (re (cadr regs))
             (type (aref (subseq str (aref rs 1) (aref re 1)) 0))
             (text (subseq str (aref rs 0) (aref re 0))))
        (cond ((char= type #\=)
               (concatenate 'string *nl* *nl* "<h1>" text "</h1>" *nl* *nl* *nl*))
              ((char= type #\-)
               (concatenate 'string *nl* *nl* "<h2>" text "</h2>" *nl* *nl* *nl*)))))
    :preserve-case t))

(defparameter *scanner-atx-header*
  (cl-ppcre:create-scanner "\\n(#{1,6})\\s*(.+?)\\s*(#+)?\\n" :case-insensitive-mode t :single-line-mode t)
  "A scanner that finds atx-style headers.")

(defun parse-atx-headers (str)
  (cl-ppcre:regex-replace-all *scanner-atx-header* str
    (lambda (match &rest regs)
      (declare (ignore match))
      (let* ((regs (cddddr regs))
             (rs (car regs))
             (re (cadr regs))
             (num-hashes (- (aref re 0) (aref rs 0)))
             (tag (format nil "h~a" num-hashes))
             (text (subseq str (aref rs 1) (aref re 1))))
        (concatenate 'string *nl* *nl* "<" tag ">" text "</" tag ">" *nl* *nl*)))
    :preserve-case t))
  
;; -----------------------------------------------------------------------------
;; list formatting
;; -----------------------------------------------------------------------------

;; BROKEN TO ALL SHIT
(defun parse-lists (str)
  (let* ((str (string-trim #(#\newline) str))
         (indent (position-if (lambda (c)
                                (or (char= c #\-)
                                    (char= c #\*)
                                    (char= c #\+))) str))
         (str (string-left-trim #(#\space #\- #\* #\+ #\tab) str))
         (str (concatenate 'string *nl* "<ul><li>" str "</li></ul>" *nl*)))
    (let* ((re (format nil "lol\\s{~a}[-+\\*]\\s*([^\\n]+)" indent))
           (scanner (cl-ppcre:create-scanner re :case-insensitive-mode t :single-line-mode t)))
      (format t "re: ~s~%" re)
      (format t "rep: ~s~%" (cl-ppcre:regex-replace-all scanner str "\\1</li><li>"))))
  "")

(defparameter *scanner-ul*
  (cl-ppcre:create-scanner "((\\n\\s*[-+\\*]\\s?.+?(?=(\\n\\n|\\n(?!\\s+))))+)" :case-insensitive-mode t :single-line-mode t)
  "A scanner for unordered lists.")

(defun parse-ul (str)
  (cl-ppcre:regex-replace-all *scanner-ul* str
    (lambda (match &rest regs)
      (declare (ignore match))
      (let* ((regs (cddddr regs))
             (rs (car regs))
             (re (cadr regs))
             (text (subseq str (aref rs 0) (aref re 0))))
        ;(parse-lists text)
        "")))
  str)

(defun parse-ol (str)
  str)

;; -----------------------------------------------------------------------------
;; general markdown functions
;; -----------------------------------------------------------------------------
(defun prepare-markdown-string (str)
  (let* ((str (cl-ppcre:regex-replace-all *scanner-replace-newlines* str *nl*))
         (str (pad-string str *nl*)))
    str))

(defun pad-string (str padding)
  (concatenate 'string padding str padding))

(defun parse-string (str &key disable-parsers)
  "Parse a markdown string into HTML."
  (let* ((str (prepare-markdown-string str))
         (handlers-pre-block '(
                               parse-atx-headers
                               parse-setext-headers
                               ))
         (handlers-post-block '(
                                parse-lazy-blockquote
                                parse-blockquote
                                parse-code
                                parse-links
                                parse-paragraphs
                                parse-ul
                                parse-ol
                                parse-entities
                                )))
    (dolist (handler handlers-pre-block)
      (unless (find handler disable-parsers)
        (setf str (funcall handler str))))
    ;(format t "str: ~s~%" str)
    (let ((blocks (split-blocks str))
          (new-blocks nil))
      ;(format t "blocks: ~s~%" blocks)
      (dolist (block blocks)
        (setf block (pad-string block *nl*))
        (dolist (handler handlers-post-block)
          (unless (find handler disable-parsers)
            (setf block (funcall handler block))))
        (setf block (string-trim #(#\newline) block))
        (push block new-blocks))
      (reduce (lambda (a b) (concatenate 'string a *nl* b)) (reverse new-blocks)))))

(defun parse-file (path)
  "Parse a markdown file into HTML (returned as a string)."
  (let ((contents (file-contents path)))
    (parse-string contents)))

(defun test (&optional show)
  (let* ((str (format nil "~
header1
====
this is a paragraph

===========

* some bullets
+ are good
  - and dont confuse parsers
- for formatting
  because they wurklol

i made code LOL
---------------
    (defun code ()
      ;; this is some code
      (+ 5 6))

1. numbers
2. can be useful
3. for formatting LOL

### quotes
sometimes i like to quote idiots

-   i want to have a bullet
    with two paragraphs

    so here's the scoop

> ## header quote
quote paragraph
lol paragraph
also 5 > 4 normally

> poop
> > shit 
> fuck

end of markdown...
"))
         (str (parse-string str)))
    (when show str)))

