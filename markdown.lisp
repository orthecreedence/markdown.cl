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
;; entity parsing
;; -----------------------------------------------------------------------------
(defun parse-horizontal-rule (str)
  "Make horizontal rules."
  (let* ((scanner-hr (cl-ppcre:create-scanner "^([*_-] ?){3,}$" :multi-line-mode t)))
    (cl-ppcre:regex-replace-all scanner-hr str "<hr>")))

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

(defun parse-embedded-blockquote (str)
  "Parse blockquotes that occur inside a list."
  (let ((scanner-find-list-blockquote (cl-ppcre:create-scanner
                                        "(\\n([*+-]|[0-9]+\\. )[^\\n]+)\\n((\\n\\s{4}>[^\\n]+)+)"
                                        :single-line-mode t))
        (scanner-format-blockquote (cl-ppcre:create-scanner
                                     "^\\s+>"
                                     :multi-line-mode t)))
    (cl-ppcre:regex-replace-all
      scanner-find-list-blockquote
      str
      (lambda (match &rest regs)
        (let* ((regs (cddddr regs))
               (rs (car regs))
               (re (cadr regs))
               (bullet (subseq match (aref rs 0) (aref re 0)))
               (blockquote (subseq match (aref rs 2) (aref re 2)))
               (blockquote (cl-ppcre:regex-replace-all scanner-format-blockquote blockquote ">"))
               (blockquote (concatenate 'string *nl* blockquote)))
          (concatenate 'string bullet (parse-standard-blockquote blockquote)))))))

;; -----------------------------------------------------------------------------
;; anchor formatting
;; -----------------------------------------------------------------------------
(defun parse-links (str)
  str)

;; -----------------------------------------------------------------------------
;; paragraph formatting
;; -----------------------------------------------------------------------------
(defparameter *scanner-find-first-html-block-element*
  (cl-ppcre:create-scanner "<(address|article|aside|audio|blockquote|canvas|dd|
div|dl|fieldset|figcaption|figure|footer|form|h1|h2|h3|h4|h5|h6|header|hgroup|
hr|noscript|ol|output|p|pre|section|table|tfoot|ul|video)( [a-z]+(=\"[^\"]+\")?)*>"
    :extended-mode t
    :single-line-mode t)
  "A scanner that searches for HTML elements that are not inline.")

(defparameter *scanner-find-last-html-block-element*
  (cl-ppcre:create-scanner "</(address|article|aside|audio|blockquote|canvas|dd|
div|dl|fieldset|figcaption|figure|footer|form|h1|h2|h3|h4|h5|h6|header|hgroup|
hr|noscript|ol|output|p|pre|section|table|tfoot|ul|video)>.*?$"
    :extended-mode t
    :single-line-mode t)
  "A scanner that searches for HTML elements that are not inline.")

(defun format-html-blocks-in-paragraph (str)
  (let* ((pos-block-el-start (cl-ppcre:scan *scanner-find-first-html-block-element* str))
         (pos-block-el-end (cl-ppcre:scan *scanner-find-last-html-block-element* str)))
    (if (and pos-block-el-start pos-block-el-end
             (< pos-block-el-start pos-block-el-end))
        (let* ((pos-block-el-end (1+ (position #\> str :start (1+ pos-block-el-end))))
               (str (concatenate 'string
                                 (subseq str 0 pos-block-el-end)
                                 "{{markdown.cl|newline}}{{markdown.cl|paragraph|open}}"
                                 (subseq str pos-block-el-end)))
               (str (concatenate 'string
                                 (subseq str 0 pos-block-el-start)
                                 "{{markdown.cl|paragraph|close}}{{markdown.cl|newline}}"
                                 (subseq str pos-block-el-start))))
          str)
        str)))

(defun paragraph-format (str)
  (if (search "{{markdown.cl|paragraph}}" str)
      (let* ((scanner-split-paragraphs (cl-ppcre:create-scanner "{{markdown\\.cl\\|paragraph}}"
                                                                :single-line-mode t))
             (parts (cl-ppcre:split scanner-split-paragraphs str))
             (parts (remove-if (lambda (p)
                                 (string= (string-trim '(#\newline #\space) p) ""))
                               parts)))
        (concatenate 'string
                     *nl* "{{markdown.cl|paragraph|open}}" *nl*
                     (reduce (lambda (concat part)
                               (let ((part (format-html-blocks-in-paragraph part)))
                                 (if concat
                                     (concatenate 'string
                                                  concat
                                                  *nl* "{{markdown.cl|paragraph|close}}"
                                                  *nl* "{{markdown.cl|paragraph|open}}"
                                                  *nl* part)
                                     part)))
                             parts
                             :initial-value nil)
                     *nl* "{{markdown.cl|paragraph|close}}" *nl*))
      str))

(defun parse-paragraphs (str &key force)
  (let* ((str (string-left-trim '(#\newline #\space) str))
         (block-el-position (cl-ppcre:scan *scanner-find-first-html-block-element* str)))
    (unless force
      (format t "str: ~a~%" str))
    (if (and block-el-position (zerop block-el-position))
        str
        (let* ((str (paragraph-format str))
               (scanner-clean-paragraphs (cl-ppcre:create-scanner
                                           "{{markdown.cl\\|paragraph\\|open}}[ \\n]+{{markdown.cl\\|paragraph\\|close}}"
                                           :single-line-mode t)))
          (cl-ppcre:regex-replace-all scanner-clean-paragraphs str "")))))

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
(defparameter *list-recursion-level* 0)

(defun pre-format-paragraph-lists (str &optional (join-list-items t))
  "Format lists in paragraph style to be normalized so they aren't chopped up by
   the rest of the parsing."
  (let* ((scanner-join-p-lists (cl-ppcre:create-scanner
                                 ;"((\\n\\n\\s{0,3}([*+-]|[0-9]+\\. )[^\\n]+){2,})"
                                 "((\\n\\n\\s{0,3}([*+-]|[0-9]+\\. )[^\\n]+((\\n[^\\n]+)+)?){2,})"
                                 :single-line-mode t))
         (scanner-format-p-lists (cl-ppcre:create-scanner
                                   "(\\n\\s{0,3}([*+-]|[0-9]+\\. )(([^\\n]+\\n)+|\\n))\\s*\\n {4,}(?!( |\\n))"
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
         (str-formatted (cl-ppcre:regex-replace-all scanner-format-p-lists
                                                    str
                                                    "\\1{{markdown.cl|paragraph}}")))
    (if (= (length str) (length str-formatted))
        str-formatted
        (pre-format-paragraph-lists str-formatted nil))))

(defun join-list-lines (str)
  "Turns lists broken into multiple lines into (per item) so that there's one
   line per item:
   
   - my list item
   broken into multiple
   lines
   
   becomes
   
   - my list item broken into multiple lines"
  (let* ((scanner-join (cl-ppcre:create-scanner
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

(defun format-lists (str indent)
  (incf *list-recursion-level*)
  (when (< 20 *list-recursion-level*)
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
      (prog1
        (concatenate 'string
                     *nl*
                     (if (eq type :ul) "<ul>" "<ol>")
                     *nl*
                     "<li>"
                     *nl*
                     (reduce (lambda (concat part)
                               (when part
                                 (setf part (parse-paragraphs part :force t)
                                       part (cl-ppcre:regex-replace
                                              (cl-ppcre:create-scanner "\\n(?=\\s*[+-])" :single-line-mode t)
                                              part
                                              (concatenate 'string *nl* *nl*))
                                       part (parse-lists part)))

                               (if concat
                                   (concatenate 'string *nl* concat *nl* "</li>" *nl* "<li>" *nl* part)
                                   part))
                             parts :initial-value nil)
                     *nl*
                     "</li>"
                     *nl*
                     (if (eq type :ul) "</ul>" "</ol>")
                     *nl*)
        (decf *list-recursion-level*)))))

(defparameter *scanner-block-list-pos*
  (cl-ppcre:create-scanner "(?<=\\n)\\s*[+-][^\\n]+" :single-line-mode t)
  "Detects if a block has a ul/ol section.")

(defun parse-list-blocks (str)
  (let* ((str (concatenate 'string *nl* (string-left-trim #(#\newline) str)))
         (list-pos (cl-ppcre:scan *scanner-block-list-pos* str)))

    (unless list-pos
      (return-from parse-list-blocks str))

    (let* ((indent (position-if (lambda (c)
                                  (or (char= c #\-)
                                      (char= c #\+)))
                                str
                                :start list-pos))
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
                       (format nil "^(?= {~a}~a)" (max 0 (1- indent)) split-type-char)
                       :multi-line-mode t))
           (parts (cl-ppcre:split section-splitter str :limit 2)))

      (if (< 1 (length parts))
          (reduce (lambda (a b)
                    (concatenate 'string a (parse-list-blocks b)))
                  parts
                  :initial-value nil)
          (let ((str (car parts)))
            (cond ((and list-pos (<= list-pos 1))
                   (format-lists (subseq str list-pos) indent))
                  (list-pos
                    (let ((garble (subseq str 0 list-pos))
                          (list-text (subseq str list-pos)))
                      (concatenate 'string garble (parse-list-blocks list-text))))
                  (t str)))))))

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
                               parse-embedded-blockquote
                               parse-horizontal-rule
                               pre-format-paragraph-lists
                               ))
         (handlers-post-block '(
                                parse-lazy-blockquote
                                parse-standard-blockquote
                                parse-code
                                parse-links
                                ;parse-paragraphs
                                parse-lists
                                parse-entities
                                )))
    (dolist (handler handlers-pre-block)
      (unless (find handler disable-parsers)
        (setf str (funcall handler str))))
    (let* ((blocks (split-blocks str))
           (blocks (remove-if (lambda (b)
                                (string= (string-trim '(#\newline #\space) b) ""))
                              blocks))
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

* * *

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

(defun test-list (&optional return)
  (let* ((str (format nil "~
-  bullet1 is a great bullet
  - fuck this shit
  1. this is great!!!
  2. ohhhh heyyyyy yeah!!
- bullet 2 is ucking grand
-   this is a list item with multiple paragraphs

    here it is again

    another paragraph
- bullet 3
is a lazy piece of shit!!
omg poop

1. hai
2. number list!!


* blockquote in list item

    > line1
    > line2


- code in li, bitch

        (defun my-function (value)
          ;; lol
          (1+ value))

-and another
here's a test

    multi-paragraph list

    item


- paragraph

- list

- items
"
))
         (str (prepare-markdown-string str))
         (str (parse-string str)))
    (when return
      str)))

