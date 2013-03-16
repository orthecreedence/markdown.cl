(in-package :markdown)

(defparameter *nl* (coerce #(#\newline) 'string)
  "Holds a string of a single newline character.")

;; -----------------------------------------------------------------------------
;; block parsing
;; -----------------------------------------------------------------------------
(defun split-blocks (str)
  "Splits a markdown document into a set of blocks, each block generally
   consisting of a certain type (list, blockquote, paragraph, etc)."
  (cl-ppcre:split
    (cl-ppcre:create-scanner "(?<=\\n\\n)(?!( {4,}|\t))" :case-insensitive-mode t :single-line-mode t)
    str))

;; -----------------------------------------------------------------------------
;; entity parsing
;; -----------------------------------------------------------------------------
(defun parse-entities (str)
  "Replace non-purposeful entities with escaped equivalents."
  (let* ((str (cl-ppcre:regex-replace-all "&(?![a-z]{2,6};)" str "&amp;"))
         (str (cl-ppcre:regex-replace-all "<(?!/?[a-z0-9]+(\s?[a-z]+=\"[^\\\"]+\")*>)" str "&lt;"))
         (str (cl-ppcre:regex-replace-all "(</?[a-z0-9]+(\s?[a-z]+=\"[^\\\"]+\")*)>" str "\\1&gtLOL;"))
         (str (cl-ppcre:regex-replace-all ">" str "&gt;"))
         (str (cl-ppcre:regex-replace-all "&gtLOL;" str ">")))
    str))

;; -----------------------------------------------------------------------------
;; code formatting
;; -----------------------------------------------------------------------------
(defun format-code (str &key embedded)
  "Sanely formats code blocks."
  (let* ((scanner-shift (cl-ppcre:create-scanner "^ {4}" :multi-line-mode t))
         (scanner-shift-embed (cl-ppcre:create-scanner "^ {8}" :multi-line-mode t))
         (str (cl-ppcre:regex-replace-all
                (if embedded scanner-shift-embed scanner-shift)
                str
                ""))
         (str (string-trim #(#\newline) str))
         (str (cl-ppcre:regex-replace-all
                (cl-ppcre:create-scanner "\\n" :single-line-mode t)
                str
                "{{markdown.cl|newline}}")))
    str))

(defun parse-embedded-code (str)
  "Parses code that is embedded inside something else (a list, for instance).
   Generally, embedded code starts with 8 spaces instead of 4."
  (cl-ppcre:regex-replace-all
    (cl-ppcre:create-scanner "\\n\\n(( {8,}[^\\n]*\\n)+(?=\\n))" :single-line-mode t)
    str
    (lambda (match &rest regs)
      (let* ((regs (cddddr regs))
             (rs (car regs))
             (re (cadr regs))
             (text (subseq match (aref rs 0) (aref re 0))))
        (concatenate 'string "{{markdown.cl|newline}}{{markdown|newline}}<pre><code>" (format-code text :embedded t) "</code></pre>" *nl*)))))

(defun parse-code (str)
  "Parses code sections in markdown."
  (let* ((scanner-in-code (cl-ppcre:create-scanner "^\\n+ {4,}" :single-line-mode t))
         (in-code (cl-ppcre:scan scanner-in-code str)))
    (if in-code
        (cl-ppcre:regex-replace-all
          (cl-ppcre:create-scanner "\\n\\n(( {4,}[^\\n]*\\n)+(?=\\n))" :single-line-mode t)
          str
          (lambda (match &rest regs)
            (let* ((regs (cddddr regs))
                   (rs (car regs))
                   (re (cadr regs))
                   (text (subseq match (aref rs 0) (aref re 0))))
              (concatenate 'string "{{markdown.cl|newline}}<pre><code>" (format-code text) "</code></pre>" *nl*))))
        str)))

;; -----------------------------------------------------------------------------
;; blockquote formatting
;; -----------------------------------------------------------------------------
(defparameter *scanner-lazy-blockquote*
  (cl-ppcre:create-scanner "(\\n>.*?\\n(?=\\n(?!>)))" :case-insensitive-mode t :single-line-mode t)
  "A scanner for finding blockquotes.")

(defparameter *scanner-blockquote*
  (cl-ppcre:create-scanner "((\\n>[^\\n]*)+(\\n|$))" :case-insensitive-mode t :single-line-mode t)
  "A scanner for finding blockquotes.")

(defun format-blockquote (str)
  "Given a string that we know is a blockquote, remove the blockquote formatting
   and recursively parse markdown within the blockquote. If the given blockquote
   is not 'lazy' then lazy blockquote parsing is disabled in the recursive parse
   so as not to screw up formatting."
  (multiple-value-bind (start end)
      (cl-ppcre:scan *scanner-blockquote* str)
    (let* ((start (or start 0))
           (end (or end 0))
           ;; if the *scanner-blockquote* regex matches the *entire* string,
           ;; then we know we have a standard blockquote (non-lazy)
           (standardp (< (abs (- (- end start) (length str))) 1))
           (str (cl-ppcre:regex-replace-all
                  (cl-ppcre:create-scanner "^>\\s?" :multi-line-mode t)
                  str
                  ""))
           (disabled-parsers (when standardp '(parse-lazy-blockquote))))
      (concatenate 'string
        "<blockquote>"
        (parse-string str :disable-parsers disabled-parsers)
        *nl* "</blockquote>"))))

(defun parse-blockquote (str re)
  "Parse a blockquote."
  (cl-ppcre:regex-replace-all re str
    (lambda (match &rest regs)
      (let* ((regs (cddddr regs))
             (rs (car regs))
             (re (cadr regs))
             (text (subseq match (aref rs 0) (aref re 0))))
        (concatenate 'string *nl* (format-blockquote text) *nl*)))))

(defun parse-standard-blockquote (str)
  "Parse a standard-formatted (non-lazy) blockquote:
   
   > this is a 
   > standard blockquote
   > > that allows nesting
   > where multiple lines are preceeded
   > by the '>' character"
  (parse-blockquote str *scanner-blockquote*))

(defun parse-lazy-blockquote (str)
  "Parse lazy blockquotes. A lazy blockquote is one like so:
   
   > this is a blockquote quote
   that continues on the following
   lines until two linebreaks are
   met
   
   Lazy blockquotes are not allowed to nest other blockquotes."
  (parse-blockquote str *scanner-lazy-blockquote*))

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
(defun parse-setext-headers (str)
  "Parse setext headers:
   
   This will be an h1
   =================="
  (cl-ppcre:regex-replace-all
    (cl-ppcre:create-scanner "\\n\\s{0,3}([^\\n]+?)\\n(=+|-+)\\n" :case-insensitive-mode t :single-line-mode t)
    str
    (lambda (match &rest regs)
      (let* ((regs (cddddr regs))
             (rs (car regs))
             (re (cadr regs))
             (type (aref (subseq match (aref rs 1) (aref re 1)) 0))
             (text (subseq match (aref rs 0) (aref re 0))))
        (cond ((char= type #\=)
               (concatenate 'string *nl* *nl* "<h1>" text "</h1>" *nl* *nl* *nl*))
              ((char= type #\-)
               (concatenate 'string *nl* *nl* "<h2>" text "</h2>" *nl* *nl* *nl*)))))
    :preserve-case t))

(defun parse-atx-headers (str)
  "Parses ATX-style headers:
   
   ### This will be an h3 tag lol"
  (cl-ppcre:regex-replace-all
    (cl-ppcre:create-scanner "\\n(#{1,6})\\s*(.+?)\\s*(#+)?\\n" :case-insensitive-mode t :single-line-mode t)
    str
    (lambda (match &rest regs)
      (let* ((regs (cddddr regs))
             (rs (car regs))
             (re (cadr regs))
             (num-hashes (- (aref re 0) (aref rs 0)))
             (tag (format nil "h~a" num-hashes))
             (text (subseq match (aref rs 1) (aref re 1))))
        (concatenate 'string *nl* *nl* "<" tag ">" text "</" tag ">" *nl* *nl*)))
    :preserve-case t))
  
;; -----------------------------------------------------------------------------
;; list formatting
;; -----------------------------------------------------------------------------

(defun pre-format-paragraph-lists (str &optional (join-list-items t))
  "Format lists in paragraph style to be normalized so they aren't chopped up by
   the rest of the parsing."
  (let* ((scanner-format-p-lists (cl-ppcre:create-scanner
                                   "(\\n\\s{0,3}([*+-]|[0-9]+\\. )(([^\\n]+\\n)+|\\n))\\s*\\n {4,}(?!( |\\n))"
                                   :single-line-mode t))
         (scanner-join-p-lists (cl-ppcre:create-scanner
                                 ;"((\\n\\n\\s{0,3}([*+-]|[0-9]+\\. )[^\\n])+\\n\\s{0,3}([*+-]|[0-9]+\\. )[^\\n]+)"
                                 "((\\n\\n\\s{0,3}([*+-]|[0-9]+\\. )[^\\n]+){2,})"
                                 :single-line-mode t))
         (str (if join-list-items
                  (cl-ppcre:regex-replace-all
                    scanner-join-p-lists
                    str
                    (lambda (match &rest regs)
                      (let* ((regs (cddddr regs))
                             (rs (car regs))
                             (re (cadr regs))
                             (text (subseq match (aref rs 0) (aref re 0)))
                             (newline-split (cl-ppcre:create-scanner "\\n\\n(?=([*+-]|[0-9]+\\. ))" :single-line-mode t))
                             (parts (cl-ppcre:split newline-split text)))
                        (reduce (lambda (a b)
                                  (concatenate 'string *nl* a b "{{markdown.cl|paragraph}}" *nl*))
                                parts))))
                  str))
         (str-formatted (cl-ppcre:regex-replace-all scanner-format-p-lists str "\\1{{markdown.cl|paragraph}}")))
    (if (= (length str) (length str-formatted))
        str-formatted
        (pre-format-paragraph-lists str-formatted nil))))

(defun test-list (&optional return)
  (let* ((str (format nil "~
-  bullet1 is a great bullet
  - fuck this shit
- bullet 2 is ucking grand
-   this is a list item with multiple paragraphs

    here it is again

    another paragraph
- bullet 3
is a lazy piece of shit!!
omg poop

1. hai
2. number list!!

-and another
here's a test

    multi-paragraph list

    item


- paragraph

- list

- items

"))
         (str (prepare-markdown-string str)))
    (let* ((block-parse t)
           (str (pre-format-paragraph-lists str)))
      (if block-parse
          (let* ((blocks (split-blocks str))
                 ;(blocks (remove-if 
                 (new-blocks nil))
            (dolist (block blocks)
              (let* ((block (prepare-markdown-string block))
                     (parsed (parse-lists block)))
                (format t "~s~%---~%" block)
                ;(format t "parsed: ~a~%" parsed)
                (push parsed new-blocks)))
            (when return
              ;(reduce (lambda (a b) (concatenate 'string a *nl* b)) (reverse new-blocks))
              (reverse new-blocks)
              ))
          (let ((str (parse-lists str)))
            (when return
              str))))))

(defun join-list-lines (str)
  "Turns lists broken into multiple lines into (per item) so that there's one
   line per item:
   
   - my list item
   broken into multiple
   lines
   
   becomes
   
   - my list item broken into multiple lines"
  (let* ((scanner-join (cl-ppcre:create-scanner
                         ;"(\\n\\s*([*+-]|[0-9]+\\. )[^\\n]+)\\n\\s*(?!(\\n|([*+-]|[0-9]+\\. )| {8, }))"
                         ;"((\\n\\s*([*+-]|[0-9]+\\. )[^\\n]+)(\\n(?!\\s*([*+-]|[0-9]+\\. ))\\s*[^ \\n]+)+)"
                         "(\\n\\s*([*+-]|[0-9]+\\. )[^\\n]+)\\n\\s*(?!\\s*([*+-]|[0-9]+\\. ))"
                         :single-line-mode t))
         (multiple-lines-p (cl-ppcre:scan scanner-join str)))
    (if multiple-lines-p
        (let ((str (cl-ppcre:regex-replace-all scanner-join str "\\1 ")))
          (join-list-lines str))
        str)))

(defun normalize-lists (str)
  "Run bullets/lists through some normalization filters to make them easier to
   parse. Numbered lists are converted to +, regular bullets converted to -.
   This greatly simplifies parsing later on."
  (let* ((scanner-normalize-ul (cl-ppcre:create-scanner "(\\n\\s*)[*+-]\\s+" :single-line-mode t))
         (scanner-normalize-ol (cl-ppcre:create-scanner "(\\n\\s*)[0-9]+\\.\\s+" :single-line-mode t))
         (str (join-list-lines str))
         (str (cl-ppcre:regex-replace-all scanner-normalize-ul str "\\1-"))
         (str (cl-ppcre:regex-replace-all scanner-normalize-ol str "\\1+")))
    str))

(defparameter *list-recursion-level* 0)

(defun format-lists (str indent)
  (incf *list-recursion-level*)
  (when (< *list-recursion-level* 20)
    (decf *list-recursion-level*)
    (return-from format-lists str))
  (flet ((build-splitter (indent)
           (cl-ppcre:create-scanner
             (format nil "^ {~a}[+-]" indent)
             :multi-line-mode t)))
    (let* ((str (string-trim #(#\newline) str))
           (type-char (find-if (lambda (c)
                                 (or (char= c #\-)
                                     (char= c #\+))) str))
           (type (if (eq type-char #\-)
                     :ul
                     :ol))
           (parts (cl-ppcre:split (build-splitter indent) str))
           (parts (remove "" parts :test #'string=)))
      ;(format t "str (~a): ~a~%" indent str)
      (format t "parts (~a): ~s~%" *list-recursion-level* parts)
      ;(return-from format-lists str)
      (prog1
        (concatenate 'string
                     *nl*
                     (if (eq type :ul) "<ul>" "<ol>")
                     *nl*
                     "<li>"
                     *nl*
                     (reduce (lambda (a b)
                               (when b
                                 (setf b (cl-ppcre:regex-replace
                                           (cl-ppcre:create-scanner "\\n(?=\\s*[+-])" :single-line-mode t)
                                           b
                                           (concatenate 'string *nl* *nl*))
                                       ;b (concatenate 'string *nl* b)
                                       b (parse-lists b)))

                               (concatenate 'string *nl* a *nl* "</li>" *nl* "<li>" *nl* b))
                             parts :initial-value nil)
                     *nl*
                     "</li>"
                     *nl*
                     (if (eq type :ul) "</ul>" "</ol>")
                     *nl*)
        (decf *list-recursion-level*)))))

(defparameter *scanner-block-list-pos*
  (cl-ppcre:create-scanner "(^|(?<=\\n))\\s*[+-][^\\n]+" :single-line-mode t)
  "Detects if a block has a ul section.")

(defun parse-list-blocks (str)
  (let ((list-pos (cl-ppcre:scan *scanner-block-list-pos* str)))

    (unless list-pos
      (return-from parse-list-blocks str))

    (let* ((indent (position-if (lambda (c)
                                  (or (char= c #\-)
                                      (char= c #\+)))
                                str
                                :start (or list-pos 0)))
           (indent (if indent
                       (- indent list-pos)
                       0))
           (type-char (find-if (lambda (c)
                                 (or (char= c #\-)
                                     (char= c #\+)))
                            str
                            :start (or list-pos 0)))
           (split-type-char (if (char= (or type-char #\space) #\-)
                                "\\+"
                                "-"))
           (section-splitter (cl-ppcre:create-scanner 
                       (format nil "^(?= {~a}~a)" indent split-type-char)
                       :multi-line-mode t))
           (parts (cl-ppcre:split section-splitter str :limit 1))
           (new-parts nil))
      (dolist (str parts)
        ;(format t "part: ~s~%" parts)
        (let ((part (cond ((and list-pos (zerop list-pos))
                           (concatenate 'string
                                        (format-lists str indent)))
                          (list-pos
                            (let ((garble (subseq str 0 list-pos))
                                  (list-text (subseq str list-pos)))
                              (concatenate 'string garble (parse-list-blocks list-text))))
                          (t str))))
          (push part new-parts)))
      (reduce (lambda (a b) (concatenate 'string a b)) (reverse new-parts)))))

;(defun parse-list-blocks_ (str)
;  (let* ((ul-pos (cl-ppcre:scan *scanner-block-has-ul* str))
;         (ol-pos (cl-ppcre:scan *scanner-block-has-ol* str)))
;
;    (when (or (and ul-pos (zerop ul-pos) ol-pos)
;              (and ol-pos (zerop ol-pos) ul-pos))
;      (flet ((get-list-indent (pos)
;               (when pos
;                 (let ((test (subseq str pos)))
;                   (- (length test)
;                      (1+ (length (string-left-trim #(#\newline #\space) test))))))))
;        (let ((ul-indent (get-list-indent ul-pos))
;              (ol-indent (get-list-indent ol-pos)))
;          (loop while (if (and ul-pos (zerop ul-pos))
;                          (and ol-pos ol-indent (not (= ol-indent ul-indent)))
;                          (and ul-pos ul-indent (not (= ul-indent ol-indent)))) do
;            (if (zerop ul-pos)
;                (setf ol-pos (cl-ppcre:scan *scanner-block-has-ol* str :start (1+ ol-pos))
;                      ol-indent (get-list-indent ol-pos))
;                (setf ul-pos (cl-ppcre:scan *scanner-block-has-ol* str :start (1+ ul-pos))
;                      ul-indent (get-list-indent ul-pos)))))))
;
;    (cond ((and ul-pos
;                ol-pos
;                (zerop ul-pos))
;           (let ((ul-text (subseq str 0 ol-pos))
;                 (rest-text (subseq str ol-pos)))
;             (concatenate 'string (format-lists ul-text :type :ul) *nl* (parse-list-blocks rest-text))))
;          ((and ol-pos
;                ul-pos
;                (zerop ol-pos))
;           (let ((ol-text (subseq str 0 ul-pos))
;                 (rest-text (subseq str ul-pos)))
;             (concatenate 'string (format-lists ol-text :type :ol) *nl* (parse-list-blocks rest-text))))
;          ((or (and ul-pos (zerop ul-pos))
;               (and ol-pos (zerop ol-pos)))
;           (format-lists str :type (if ol-pos :ol :ul)))
;          (ul-pos
;           (let ((garble (subseq str 0 ul-pos))
;                 (ul-text (subseq str ul-pos)))
;             (concatenate 'string garble (parse-list-blocks ul-text))))
;          (ol-pos
;           (let ((garble (subseq str 0 ol-pos))
;                 (ol-text (subseq str ol-pos)))
;             (concatenate 'string garble (parse-list-blocks ol-text))))
;          (t str))))

(defun parse-ol (str)
  str)

(defun parse-lists (str)
  (let* ((str (normalize-lists (normalize-lists str)))
         (str (parse-list-blocks str)))
    str))

;; -----------------------------------------------------------------------------
;; general markdown functions
;; -----------------------------------------------------------------------------
(defun prepare-markdown-string (str)
  "A lot of the regular expressions, in order to maintain simplicity, expect the
   strings they are given to be formatted in a certain way. For instance,
   instead of testing for (^|\\n), if every string is started with a \\n, then
   we can just test for \\n (and leave out testing for the beginning of the 
   string)."
  (let* ((scanner-normalize-newlines (cl-ppcre:create-scanner "[\\r\\n]" :single-line-mode t))
         (scanner-normalize-tabs (cl-ppcre:create-scanner "\\t"))
         (str (cl-ppcre:regex-replace-all scanner-normalize-newlines str *nl*))
         (str (cl-ppcre:regex-replace-all scanner-normalize-tabs str "    "))
         (str (pad-string str *nl*)))
    str))

(defun pad-string (str padding)
  "There's probably a lisp function for this already. Pads the beginning and end
   of the given string with the given padding (also a string)."
  (concatenate 'string padding str padding))

(defun parse-string (str &key disable-parsers)
  "Parse a markdown string into HTML."
  (let* ((str (prepare-markdown-string str))
         (handlers-pre-block '(
                               parse-atx-headers
                               parse-setext-headers
                               parse-embedded-code
                               pre-format-paragraph-lists
                               ))
         (handlers-post-block '(
                                parse-lazy-blockquote
                                parse-standard-blockquote
                                parse-code
                                parse-links
                                parse-paragraphs
                                parse-lists
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
  (format t "--------------~%")
  (let* ((str (format nil "~
header1
====
this is a paragraph

===========

* some bullets
+ are good
  - and dont confuse parsers
  - lol sub bullets
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

* wrap these list

* items in paragraph tags

* plz!!

### quotes
sometimes i like to quote idiots

-   i want to have a bullet
    with two paragraphs

    so here's the scoop

        and here's a code block!

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

