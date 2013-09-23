(in-package :markdown.cl)

(defparameter *link-references* nil
  "Holds a hash table mapping link ids to URLs.")

(defparameter *tmp-storage* nil
  "Holds a hash table used for temporary blockquote storage.")

;; -----------------------------------------------------------------------------
;; block parsing
;; -----------------------------------------------------------------------------
(defun split-blocks (str)
  "Splits a markdown document into a set of blocks, each block generally
   consisting of a certain type (list, blockquote, paragraph, etc)."
  ;; the following two str replacements makes sure that blockquotes are put into
  ;; their own block
  (let* ((str (cl-ppcre:regex-replace
                (cl-ppcre:create-scanner "(\\n[^>\\n]+)\\n>" :single-line-mode t)
                str
                (concatenate 'string "\\1" *nl* *nl* ">")))
         (str (cl-ppcre:regex-replace
                (cl-ppcre:create-scanner "(\\n>[^\\n]+)\\n(?!>)" :single-line-mode t)
                str
                (concatenate 'string "\\1" *nl* *nl*))))
    (cl-ppcre:split
      (cl-ppcre:create-scanner "(?<=\\n\\n)" :case-insensitive-mode t :single-line-mode t)
      str)))

;; -----------------------------------------------------------------------------
;; entity parsing
;; -----------------------------------------------------------------------------
(defun parse-escaped-characters (str)
  "Parse characters that are escaped with \\"
  (cl-ppcre:regex-replace-all
    "\\\\([.\\\`*_\\[\\]{}#+!-])"
    str
    ;"{{markdown.cl|escaped|\\1}}"
    (lambda (match &rest regs)
      (let* ((regs (cddddr regs))
             (rs (car regs))
             (re (cadr regs))
             (escaped-char (subseq match (aref rs 0) (aref re 0)))
             (id (format nil "esc-char-~a" (hash-table-count *tmp-storage*))))
        (setf (gethash id *tmp-storage*) escaped-char)
        (concatenate 'string "{{markdown.cl|escaped|" id "}}")))))

(defun do-parse-entities (str &key use-markdown-tags)
  "Replace non-purposeful entities with escaped equivalents."
  (let* ((str (cl-ppcre:regex-replace-all "&(?!#?[\\w]{2,6};)" str (if use-markdown-tags "{{markdown.cl|amp}}" "&amp;")))
         (str (cl-ppcre:regex-replace-all "<(?!(/?[\\w]+(\\s?[a-zA-Z-]+=\".*?\")*/?>))" str (if use-markdown-tags "{{markdown.cl|lt}}" "&lt;")))
         (str (cl-ppcre:regex-replace-all "(</?[\\w]+(\\s?[a-zA-Z]+=\".*?\")*)(/?)>" str "\\1\\3{{markdown.cl|gt-tmp}}"))
         (str (cl-ppcre:regex-replace-all ">" str (if use-markdown-tags "{{markdown.cl|gt}}" "&gt;")))
         (str (cl-ppcre:regex-replace-all "{{markdown\\.cl\\|gt-tmp}}" str ">")))
    str))

(defun parse-entities (str)
  "On top of parsing entities:
   
   I am a sicko & a perv   =>   I am a sicko &amp; a perv
   Dogs > cats             =>   Dogs &gt; cats
   <em>I'm the best</em>   =>   <em>I'm the best</em>
   
   also escape the inside of <code> blocks:
   
   <code><div>&copy;</div></code>
   
   becomes:
   
   <code>&lt;div&gt;&amp;copy;&lt;/div&gt;</code>
   
   It does this using the parse-not-in-code function, which operates inside code
   blocks, using do-parse-entities outside code blocks, and escaping everything
   inside code blocks."
  (parse-not-in-code str 'do-parse-entities :escape t))

(defun escape-html (str)
  "Meant to be called on text inside code blocks."
  (let* ((str (cl-ppcre:regex-replace-all "&" str "&amp;"))
         (str (cl-ppcre:regex-replace-all "<" str "&lt;"))
         (str (cl-ppcre:regex-replace-all ">" str "&gt;")))
    str))

;; -----------------------------------------------------------------------------
;; hr parsing
;; -----------------------------------------------------------------------------
(defun parse-horizontal-rule (str)
  "Make horizontal rules. These are (almost?) always wrapped in <p> tags by the
   paragraph parser, but this is taken care of in the final parsing pass."
  (let* ((scanner-hr (cl-ppcre:create-scanner "^([*_-] ?){3,}$" :multi-line-mode t)))
    (cl-ppcre:regex-replace-all scanner-hr str "<hr>")))

;; -----------------------------------------------------------------------------
;; code formatting
;; -----------------------------------------------------------------------------
(defun parse-not-in-code (str parser-fn &key escape in-code-fn)
  "Given a string and a parsing function, run the parsing function over the
   parts of the string that are not inside any code block.
   
   Also has the ability to escape the internals of code blocks."
  (let ((parts (cl-ppcre:split
                 (cl-ppcre:create-scanner "{{(?=markdown\\.cl\\|code)" :multi-line-mode t)
                 str))
        (depth 0))
    (reduce
      (lambda (a b)
        (cond ((eq (search "markdown.cl|code|open" b) 0)
               (incf depth))
              ((eq (search "markdown.cl|code|close" b) 0)
               (decf depth)))
        (concatenate 'string
                     (when a
                       (concatenate 'string a "{{"))
                     (cond ((zerop depth)
                            (funcall parser-fn b))
                           ((or escape in-code-fn)
                            (let* ((tag-end-pos (+ 2 (position #\} b)))
                                   (str (subseq b tag-end-pos))
                                   (str (if escape
                                            (escape-html str)
                                            str))
                                   (str (if in-code-fn
                                            (funcall in-code-fn str)
                                            str)))
                              (concatenate 'string
                                           (subseq b 0 tag-end-pos)
                                           str)))
                           (t b))))
      parts :initial-value nil)))

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
        (concatenate 'string
                     "{{markdown.cl|newline}}{{markdown.cl|newline}}"
                     "<pre>{{markdown.cl|code|open}}"
                     (format-code text :embedded t)
                     "{{markdown.cl|code|close}}</pre>" *nl*)))))

(defun parse-code (str)
  "Parses code sections in markdown."
  (let* ((scanner-in-code (cl-ppcre:create-scanner "^\\n+ {4,}" :single-line-mode t))
         (in-code (cl-ppcre:scan scanner-in-code str)))
    (if in-code
        (cl-ppcre:regex-replace-all
          (cl-ppcre:create-scanner "\\n+(( {4,}[^\\n]*\\n)+(?=\\n))" :single-line-mode t)
          str
          (lambda (match &rest regs)
            (let* ((regs (cddddr regs))
                   (rs (car regs))
                   (re (cadr regs))
                   (text (subseq match (aref rs 0) (aref re 0))))
              (concatenate 'string
                           "{{markdown.cl|newline}}<pre>{{markdown.cl|code|open}}"
                           (format-code text)
                           "{{markdown.cl|code|close}}</pre>" *nl*))))
        str)))

;; -----------------------------------------------------------------------------
;; blockquote formatting
;; -----------------------------------------------------------------------------
(defparameter *scanner-blockquote*
  (cl-ppcre:create-scanner "((\\n>[^\\n]*)+(\\n|$))" :single-line-mode t)
  "A scanner for finding blockquotes.")

(defun format-blockquote (str)
  "Given a string that we know is a blockquote, remove the blockquote formatting
   and recursively parse markdown within the blockquote. If the given blockquote
   is not 'lazy' then lazy blockquote parsing is disabled in the recursive parse
   so as not to screw up formatting."
  (let* ((str (cl-ppcre:regex-replace-all
                (cl-ppcre:create-scanner "^> ?" :multi-line-mode t)
                str
                ""))
         (disabled-parsers '(convert-lazy-blockquote-to-standard
                             parse-entities
                             escape-code-internals
                             cleanup-paragraphs
                             cleanup-newlines
                             cleanup-code)))
    (concatenate 'string
      *nl* "<blockquote>" *nl*
      (parse str :disable-parsers disabled-parsers)
      *nl* "</blockquote>")))

(defun convert-lazy-blockquote-to-standard (str)
  "Converts a lazy blockquote:
   
   > this a blockquote that
   spans multiple lines but
   im too lazy to add the '>'
   at the beginning of each line
   
   into:
   
   > this a blockquote that
   > spans multiple lines but
   > im too lazy to add the '>'
   > at the beginning of each line"
  (let* ((scanner-lazy-bq (cl-ppcre:create-scanner
                            "(\\n>[^\\n]+(\\n(?!>)[^\\n]+)+)"
                            :single-line-mode t)))
    (cl-ppcre:regex-replace-all
      scanner-lazy-bq
      str
      (lambda (match &rest regs)
        (let* ((regs (cddddr regs))
               (rs (car regs))
               (re (cadr regs))
               (text (subseq match (aref rs 0) (aref re 0))))
          (cl-ppcre:regex-replace-all
            (cl-ppcre:create-scanner "\\n(?!>)([^\\n]+)" :single-line-mode t)
            text
            (concatenate 'string *nl* "> \\1")))))))

(defparameter *scanner-lazy-blockquote*
  (cl-ppcre:create-scanner "(\\n>.*?\\n(?=\\n(?!>)))" :single-line-mode t)
  "A scanner for finding blockquotes.")

(defun parse-embedded-blockquote (str)
  "Parse blockquotes that occur inside a list. This must be a separate step,
   otherwise things can get wonky when parsing lists. The idea is to find
   blockquotes that are embedded in lists *before* the lists are processed, then
   turn them into what the list parser views as a standard paragraph.
   
   Instead of injecting embedded blockquotes directly into the list string, they
   are saved in a hash table and injected afterwards for more accurate parsing."
  (let* ((scanner-find-list-blockquote
           (cl-ppcre:create-scanner
             "(\\n([*+-]|[0-9]+\\. )[^\\n]+(\\n\\n?(?! *([*+-]|[0-9]+\\.|>))[^\\n]+)*)\\n?((\\n {4,}>[^\\n]*)+)"
             :single-line-mode t))
         (scanner-format-blockquote (cl-ppcre:create-scanner
                                      "^\\s+>"
                                      :multi-line-mode t))
         (scanner-clean-newlines (cl-ppcre:create-scanner "\\n+" :single-line-mode t))
         (str (cl-ppcre:regex-replace-all
                scanner-find-list-blockquote
                str
                (lambda (match &rest regs)
                  ;; this can get nasty, depending on what parts matches the scan
                  ;; (if you couldn't tell from the above regex). what we're doing
                  ;; is trying to piece together all of the groups we saved above
                  ;; to construct the original text for the bullet, as well as pull
                  ;; out *and process* the blockquote text, which is then concated
                  ;; back onto the bullet.
                  (let* ((regs (cddddr regs))
                         (rs (car regs))
                         (re (cadr regs))
                         ;; get the original bullet text (between the bullet and
                         ;; the blockquote) prepared
                         (bullet (subseq match (aref rs 0) (aref re 0)))
                         ;(bullet (if (aref rs 5)
                         ;            (concatenate 'string bullet
                         ;                         (subseq match (aref rs 5) (aref re 5)))
                         ;            bullet))
                         ;; pull out the blockquote and process it
                         (blockquote (subseq match (aref rs 4) (aref re 4)))
                         (blockquote (cl-ppcre:regex-replace-all
                                       scanner-format-blockquote
                                       blockquote
                                       ">"))
                         (blockquote (concatenate 'string *nl* blockquote))
                         (blockquote (parse-blockquote blockquote))
                         (blockquote (cl-ppcre:regex-replace-all scanner-clean-newlines blockquote *nl*)))
                    (let ((blockquote-id (format nil "bq-~a" (hash-table-count *tmp-storage*))))
                      ;; save the blockquote for later
                      (setf (gethash blockquote-id *tmp-storage*) blockquote)
                      ;; sew the original bullet onto the blockquote placeholder
                      (concatenate 'string bullet *nl* *nl* "    {{markdown.cl|blockquote|" blockquote-id "}}")))))))
    (if (cl-ppcre:scan scanner-find-list-blockquote str)
        (parse-embedded-blockquote str)
        str)))

(defun inject-saved-blockquotes (str)
  (let* ((scanner-blockquote-placeholder (cl-ppcre:create-scanner "{{markdown\\.cl\\|blockquote\\|(bq-[^}]+)}}"
                                                                  :multi-line-mode t)))
    (cl-ppcre:regex-replace-all
      scanner-blockquote-placeholder
      str
      (lambda (match &rest regs)
        (let* ((regs (cddddr regs))
               (rs (car regs))
               (re (cadr regs))
               (id (subseq match (aref rs 0) (aref re 0)))
               (text (gethash id *tmp-storage*))
               (text (or text "")))
          text)))))

(defun parse-blockquote (str)
  "Parse a blockquote recursively, using the passed-in regex."
  (cl-ppcre:regex-replace-all *scanner-blockquote* str
    (lambda (match &rest regs)
      (let* ((regs (cddddr regs))
             (rs (car regs))
             (re (cadr regs))
             (text (subseq match (aref rs 0) (aref re 0))))
        (concatenate 'string *nl* (format-blockquote text))))))

;; -----------------------------------------------------------------------------
;; link/image formatting
;; -----------------------------------------------------------------------------
(defun gather-link-references (str)
  "Look for any link references in the document:
   
   [link-id]: http://my-url.com
   [4]: http://my-link.com (optional title)
   [mylink]: http://my-url.com/lol 'kewl link brah'
   [omg]: http://lol.com/wtf \"rofl\"

   and parse them into the *link-references* hash table. The data will be pulled
   out when parse-links is called.
   
   Note that as a side effect, this also gathers image references =]."
  (let* ((scanner-find-link-refs (cl-ppcre:create-scanner
                                   "\\n {0,3}\\[([^\\]]+)\\]: +([^\\s]+)( *\\n? *[\"'(](.*?)[\"')])? *"
                                   :single-line-mode t
                                   :case-insensitive-mode t)))
    (cl-ppcre:regex-replace-all
      scanner-find-link-refs
      str
      (lambda (match &rest regs)
        (let* ((regs (cddddr regs))
               (rs (car regs))
               (re (cadr regs))
               (id (subseq match (aref rs 0) (aref re 0)))
               (id (string-downcase id))
               (url (subseq match (aref rs 1) (aref re 1)))
               (title (if (aref rs 3)
                          (subseq match (aref rs 3) (aref re 3))
                          nil)))
          (setf (gethash id *link-references*) (list :url url :title title))
          "")))))

(defun make-link (url text title)
  (concatenate 'string
               "<a href=\"" url "\""
               (when title
                 (concatenate 'string " title=\"" title "\""))
               ">" text "</a>"))

(defun make-image (url alt title)
  (concatenate 'string
               "<img src=\"" url "\" "
               "alt=\"" alt "\""
               (when title
                 (concatenate 'string " title=\"" title "\""))
               "/>"))

(defun parse-links-ref (str)
  "Parse links that are reference-style:
     [link text][id]"
  (let* ((scanner-links-id (cl-ppcre:create-scanner "!?\\[([^\\]]+)\\](\\n| )?\\[([^\\]]*)\\]")))
    (cl-ppcre:regex-replace-all
      scanner-links-id
      str
      (lambda (match &rest regs)
        (let* ((is-image (char= (aref match (caddr regs)) #\!))
               (regs (cddddr regs))
               (rs (car regs))
               (re (cadr regs))
               (text (subseq match (aref rs 0) (aref re 0)))
               (id (subseq match (aref rs 2) (aref re 2)))
               (id (if (string= "" id)
                     text
                     id))
               (id (string-downcase id))
               (match (gethash id *link-references*))
               (url (getf match :url))
               (title (getf match :title)))
          (if is-image
              (make-image url text title)
              (make-link url text title)))))))

(defun parse-links-self (str)
  "Parse links that are self contained (not a reference):
     [my link text](http://url.com \"title\")"
  (let* ((scanner-links-self (cl-ppcre:create-scanner
                               "!?\\[([^\\]]+)\\](\\n| )?\\(([^ ()]+)( \"(.*?)\")?\\)")))
    (cl-ppcre:regex-replace-all
      scanner-links-self
      str
      (lambda (match &rest regs)
        (let* ((is-image (char= (aref match (caddr regs)) #\!))
               (regs (cddddr regs))
               (rs (car regs))
               (re (cadr regs))
               (text (subseq match (aref rs 0) (aref re 0)))
               (url (subseq match (aref rs 2) (aref re 2)))
               (title (if (aref rs 4)
                        (subseq match (aref rs 4) (aref re 4))
                        nil)))
          (if is-image
              (make-image url text title)
              (make-link url text title)))))))

(defun parse-quick-links (str)
  "Parse quick-link style:
     <http://killtheradio.net>"
  (let* ((scanner-links-quick (cl-ppcre:create-scanner "<([0-9a-z]+://[^>]+)>" :case-insensitive-mode t))
         (scanner-email-quick (cl-ppcre:create-scanner "<([^>]+@[^>]+)>"))
         (str (cl-ppcre:regex-replace-all scanner-links-quick str "<a href=\"\\1\">\\1</a>" :preserve-case t))
         (str (cl-ppcre:regex-replace-all
                scanner-email-quick
                str
                ;; TODO entity escaping/obfuscation
                "<a href=\"mailto:\\1\">\\1</a>"
                :preserve-case t)))
    str))
  
(defun parse-links (str)
  "Parse all link styles. It's important to note that because the image/link
   syntax is so similar, the following parsers handle both images and links."
  (let* ((str (parse-links-ref str))
         (str (parse-links-self str))
         (str (parse-quick-links str)))
    str))

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
  (cl-ppcre:create-scanner ".*</(address|article|aside|audio|blockquote|canvas|dd|
div|dl|fieldset|figcaption|figure|footer|form|h1|h2|h3|h4|h5|h6|header|hgroup|
hr|noscript|ol|output|p|pre|section|table|tfoot|ul|video)>"
    :extended-mode t
    :single-line-mode t)
  "A scanner that searches for HTML elements that are not inline.")

(defun format-html-blocks-in-paragraph (str)
  "This is a very helpful function which turns:
   
   <p>this is my text<div>this is inside a block</div> more text</p>
   
   into:
   
   <p>this is my text</p><div>this is inside a block</div><p>more text</p>
   
   In other words, it unwraps <p> tags from around HTML block elements, and does
   so such that all text between the first block tag found and after the last
   block tag found is left untouched (and unwrapped by <p>)."
  (let* ((pos-block-el-start (cl-ppcre:scan *scanner-find-first-html-block-element* str))
         (pos-block-el-end (multiple-value-list
                             (cl-ppcre:scan *scanner-find-last-html-block-element* str)))
         (pos-block-el-end (when (car pos-block-el-end)
                             (aref (caddr pos-block-el-end) 0))))
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
  "This function looks for {{markdown.cl|paragraph}} tags and splits up the text
   given accordingly, adding opening/closing markdown.cl paragraph tags around
   each of the splits. It then uses format-html-blocks-in-paragraph to remove
   any paragraph tags that shouldn't be there."
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

(defun parse-paragraphs (str &key pre-formatted)
  "This formats paragraphs in text. Most blocks given to is are treated as
   paragraph blocks until otherwise noted. If it detects that another parser
   added in paragraph tags, it will skip the block *unless* the pre-formatted
   key arg is T (meaning that the string being passed in has paragraph tags
   in it that need to be dealt with).
   
   This function also does its best to clean the output by ridding us of empty
   paragraph blocks."
  (let ((has-paragraphs-already-p (search "{{markdown.cl|paragraph|open}}" str)))
    (if (and has-paragraphs-already-p
             (not pre-formatted))
        str
        (let* ((str (string-trim '(#\newline #\space) str))
               (str (if pre-formatted
                        str
                        (concatenate 'string str "{{markdown.cl|paragraph}}")))
               (str (paragraph-format str))
               (scanner-clean-markup (cl-ppcre:create-scanner
                                       "{{markdown.cl\\|paragraph\\|open}}[\\s\\n]+{{markdown.cl\\|paragraph\\|close}}"
                                       :single-line-mode t))
               (scanner-clean-newlines (cl-ppcre:create-scanner
                                         "\\n+(?={{markdown\\.cl\\|paragraph\\|close}})"
                                         :single-line-mode t))
               (str (cl-ppcre:regex-replace-all scanner-clean-markup str ""))
               (str (cl-ppcre:regex-replace-all scanner-clean-newlines str *nl*)))
          str))))

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
               (concatenate 'string *nl* *nl* "<h2>" text "</h2>" *nl* *nl* *nl*)))))))

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
        (concatenate 'string *nl* *nl* "<" tag ">" text "</" tag ">" *nl* *nl*)))))
  
;; -----------------------------------------------------------------------------
;; list formatting
;; -----------------------------------------------------------------------------
(defparameter *list-recursion-level* 0)

(defun pre-format-paragraph-lists (str &optional (join-list-items t))
  "Format lists in paragraph style to be normalized so they aren't chopped up by
   the rest of the parsing."
  (flet ((join-p-lists (str type)
           (let* ((scanner-join-p-lists (cl-ppcre:create-scanner
                                          "((\\n\\n\\s{0,3}([*+-])[^\\n]+((\\n[^\\n]+)+)?){2,})"
                                          :single-line-mode t))
                  (scanner-join-p-nums (cl-ppcre:create-scanner
                                         "((\\n\\n\\s{0,3}([0-9]+\\. )[^\\n]+((\\n[^\\n]+)+)?){2,})"
                                         :single-line-mode t))
                  (main-scanner (if (eq type :number)
                                    scanner-join-p-nums
                                    scanner-join-p-lists)))
             (if join-list-items
                 (cl-ppcre:regex-replace-all
                   main-scanner
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
                 str))))
    (let* ((scanner-format-p-lists (cl-ppcre:create-scanner
                                     "(\\n\\s{0,3}([*+-]|[0-9]+\\. )(([^\\n]+\\n)+|\\n))\\s*\\n {4,}(?!( |\\n))"
                                     :single-line-mode t))
           (str (join-p-lists str :list))
           (str (join-p-lists str :number))
           (str-formatted (cl-ppcre:regex-replace-all scanner-format-p-lists
                                                      str
                                                      "\\1{{markdown.cl|paragraph}}")))
      (if (= (length str) (length str-formatted))
          str-formatted
          (pre-format-paragraph-lists str-formatted nil)))))

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
        (let* ((str (cl-ppcre:regex-replace-all scanner-join str "\\1{{markdown.cl|space}}"))
               (str (cl-ppcre:regex-replace-all
                      (cl-ppcre:create-scanner "({{markdown\\.cl(\\|[a-z]+)+}}){{markdown\\.cl\\|space}}"
                                               :single-line-mode t)
                      str
                      "\\1"))
               (str (cl-ppcre:regex-replace-all
                      (cl-ppcre:create-scanner "{{markdown\\.cl\\|space}}({{markdown\\.cl(\\|[a-z]+)+}})"
                                               :single-line-mode t)
                      str
                      "\\1"))
               (str (cl-ppcre:regex-replace-all "{{markdown\\.cl\\|space}}" str " ")))
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
  "This is the function that actually makes lists happen. Once all the blocks
   have been diced up into neat little packages ready for formatting, they are
   handed off to format-lists.
   
   This function is responsible for adding the <ul>/<ol>/<li> tags around list
   items, making sure to only do this for items using the correct indentation
   level.
   
   List items inject any saved blockquotes (via inject-saved-blockquotes) before
   moving on to paragraph processing. This step is essential because a lot of
   the blockquote formatting can screw up the splitting of list items correctly,
   resulting in <p> blocks in really weird places.
   
   List items are run through the paragraph filters, have a minimal amount of
   formatting applied to make sure the recursion goes smoothly, and then are
   recursively concated onto the final string."
  (incf *list-recursion-level*)

  ;; make sure to not infinitely recurse, in case of bugs
  ;; TODO: make this a configurable value!!
  (when (< 20 *list-recursion-level*)
    (decf *list-recursion-level*)
    (return-from format-lists str))

  (flet ((build-splitter (indent)
           ;; return a scanner that splits list items up by indent level
           (cl-ppcre:create-scanner
             (format nil "^ {~a}[+-]" indent)
             :multi-line-mode t)))
    ;; find out what kind of list we're formatting, and split up the list items
    ;; according to indent
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
                     ;; concat the list items into one big string after they've
                     ;; been properly processed
                     (reduce (lambda (concat part)
                               ;; do some paragraph/list formatting
                               (let* ((part (inject-saved-blockquotes part))
                                      (part (parse-paragraphs part :pre-formatted t))
                                      ;; make sure first list item in this parsed
                                      ;; block starts with two \n\n so it gets properly
                                      ;; separated
                                      (part (cl-ppcre:regex-replace
                                              (cl-ppcre:create-scanner "\\n(?=\\s*[+-])" :single-line-mode t)
                                              part
                                              (concatenate 'string *nl* *nl*)))
                                      ;; recurse!
                                      (part (parse-lists part)))
                                 ;; build the list items
                                 (if concat
                                     (concatenate 'string *nl* concat *nl* "</li>" *nl* "<li>" *nl* part)
                                     part)))
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
  "This function takes a list block, and splits it into sub-blocks depending on
   list type (in other words, if a numbered list directly follows a normal list,
   the two are processed separately). This is done recursively.
   
   It also detects the amount of intent the list uses, which it sends into
   `format-lists` when an entire block has been singled out."
  (let* ((str (concatenate 'string *nl* (string-left-trim #(#\newline) str)))
         (list-pos (cl-ppcre:scan *scanner-block-list-pos* str)))

    ;; return if no list present
    (unless list-pos
      (return-from parse-list-blocks str))

    ;; find the indent level of the main list
    (let* ((indent (position-if (lambda (c)
                                  (or (char= c #\-)
                                      (char= c #\+)))
                                str
                                :start list-pos))
           (indent (if indent
                       (- indent list-pos)
                       0))
           ;; find the character used for the list at this point, we'll have
           ;; either "-" (a bullet list) or "+" (a numbered list)
           (type-char (find-if (lambda (c)
                                 (or (char= c #\-)
                                     (char= c #\+)))
                            str
                            :start (or list-pos 0)))
           ;; this will be a character of type-char: if type-char is #\-, then
           ;; split-type-char will be #\+ (and vice versa). this allows us to
           ;; split up different list types into different blocks.
           (split-type-char (if (char= (or type-char #\space) #\-)
                                "\\+"
                                "-"))
           ;; split up the list sections (only split the first different list
           ;; type)
           (section-splitter (cl-ppcre:create-scanner 
                       (format nil "^(?= {~a}~a)" (max 0 (1- indent)) split-type-char)
                       :multi-line-mode t))
           (parts (cl-ppcre:split section-splitter str :limit 2)))

      (if (< 1 (length parts))
          ;; we got two parts!! for each part, recursively call parse-list-blocks
          (reduce (lambda (a b)
                    (concatenate 'string a (parse-list-blocks b)))
                  parts
                  :initial-value nil)

          ;; nope, only one part (the main list)
          (let ((str (car parts)))
            (cond ((and list-pos (<= list-pos 1))
                   ;; our main list is at the starting position
                   (format-lists (subseq str list-pos) indent))
                  (list-pos
                    ;; our main list is somewhere in the middle of this block.
                    ;; this probably shouldn't happen, but "probably" and
                    ;; "shouldn't" don't mix well with programming, so let's
                    ;; test for it, splitting the first non-list section
                    ;; (called "garble") from the list, and parsing only the
                    ;; list.
                    (let ((garble (subseq str 0 list-pos))
                          (list-text (subseq str list-pos)))
                      (concatenate 'string garble (parse-list-blocks list-text))))
                  (t str)))))))

(defun parse-lists (str)
  "Parse lists (both bullet and number lists). First, normalizes them (which
   makes them a whole lot easier to parse) then recursively parses them."
  (let* ((str (normalize-lists str))
         (str (parse-list-blocks str)))
    str))

;; -----------------------------------------------------------------------------
;; span-level functions
;; -----------------------------------------------------------------------------
(defun do-parse-double-code (str)
  "Parse ``...`` code blocks."
  (let* ((scanner-code (cl-ppcre:create-scanner "``(.*?)``" :single-line-mode t))
         (str (cl-ppcre:regex-replace-all scanner-code str "{{markdown.cl|code|open}}\\1{{markdown.cl|code|close}}")))
    str))

(defun do-parse-code (str)
  "Parse `...` code blocks."
  (let* ((scanner-code (cl-ppcre:create-scanner "`(.*?)`" :single-line-mode t))
         (str (cl-ppcre:regex-replace-all scanner-code str "{{markdown.cl|code|open}}\\1{{markdown.cl|code|close}}")))
    str))

(defun parse-inline-code (str)
  "Parse `...` code blocks."
  (let* ((str (parse-not-in-code str 'do-parse-double-code))
         (str (parse-not-in-code str 'do-parse-code)))
    str))

(defun do-parse-em (str)
  "Parse *, _, **, and __."
  (let* ((scanner-strong*-process (cl-ppcre:create-scanner "\\*\\*(.*?)\\*\\*" :single-line-mode t))
         (scanner-strong_-process (cl-ppcre:create-scanner "__(.*?)__" :single-line-mode t))
         (scanner-em*-process (cl-ppcre:create-scanner "\\*(.*?)\\*" :single-line-mode t))
         (scanner-em_-process (cl-ppcre:create-scanner "_(.*?)_" :single-line-mode t))
         (str (cl-ppcre:regex-replace-all scanner-strong*-process str "<strong>\\1</strong>"))
         (str (cl-ppcre:regex-replace-all scanner-strong_-process str "<strong>\\1</strong>"))
         (str (cl-ppcre:regex-replace-all scanner-em*-process str "<em>\\1</em>"))
         (str (cl-ppcre:regex-replace-all scanner-em_-process str "<em>\\1</em>")))
    str))

(defun parse-em (str)
  "Parse *, _, **, and __, but only in non-code blocks. It's tricky though,
   because our <em>/<strong> elements must be able to span across <code> blocks.
   What we do it replace any * objects in <code> blocks with a meta string,
   process the em/strong tags, and then replace hte meta char. Works great."
  (let* ((str (parse-not-in-code str 'identity
                :in-code-fn (lambda (str)
                              (cl-ppcre:regex-replace-all
                                "\\*"
                                str
                                "{{markdown.cl|star}}"))))
         (str (do-parse-em str))
         (str (cl-ppcre:regex-replace-all
                "{{markdown\\.cl\\|star}}"
                str
                "*")))
    str))

(defun do-parse-br (str)
  "Parse <br> tags (when a line ends with two spaces)."
  (cl-ppcre:regex-replace-all
    (cl-ppcre:create-scanner "([^ ]+)  (\\n)" :single-line-mode t)
    str
    "\\1<br/>\\2"))

(defun parse-br (str)
  "Parse <br> tags (when a line ends with two spaces)."
  (parse-not-in-code str 'do-parse-br))

;; -----------------------------------------------------------------------------
;; cleanup functions
;; -----------------------------------------------------------------------------
(defun cleanup-code (str)
  "Let's convert our {{markdown.cl|code|...}} tags to <code> tags."
  (let* ((str (cl-ppcre:regex-replace-all "{{markdown\\.cl\\|code\\|open}}" str "<code>"))
         (str (cl-ppcre:regex-replace-all "{{markdown\\.cl\\|code\\|close}}" str "</code>")))
    str))

(defun cleanup-newlines (str)
  "Here we remove excess newlines and convert any markdown.cl newlines into real
   ones."
  (let* ((scanner-clean (cl-ppcre:create-scanner "\\n+" :single-line-mode t))
         (scanner-newline (cl-ppcre:create-scanner "{{markdown\\.cl\\|newline}}"))
         (str (cl-ppcre:regex-replace-all scanner-clean str *nl*))
         (str (cl-ppcre:regex-replace-all scanner-newline str *nl*)))
    str))

(defun cleanup-hr (str)
  "Due to the way processing <hr> tags occurs, they are always wrapped in <p>
   blocks. Instead of trying to figure out a way to NOT wrap them in <p> blocks
   (which would surely screw up the rest of the paragraph formatting) it makes
   more sense to let it happen, then fix in the final pass."
  (cl-ppcre:regex-replace-all
    (cl-ppcre:create-scanner
      "{{markdown\\.cl\\|paragraph\\|open}}[\\n\\s]+<hr>[\\n\\s]+{{markdown\\.cl\\|paragraph\\|close}}"
      :single-line-mode t)
    str
    "<hr/>"))

(defun cleanup-paragraphs (str)
  "Remove any empty paragraph blocks (it does happen sometimes) and convert all
   markdown.cl paragraphs into real <p> tags."
  (let* ((scanner-p-empty (cl-ppcre:create-scanner
                            "{{markdown\\.cl\\|paragraph\\|open}}[\\n\\s]+{{markdown\\.cl\\|paragraph\\|close}}"
                            :single-line-mode t))
         (scanner-p-open (cl-ppcre:create-scanner "{{markdown\\.cl\\|paragraph\\|open}}"))
         (scanner-p-close (cl-ppcre:create-scanner "{{markdown\\.cl\\|paragraph\\|close}}"))
         (str (cl-ppcre:regex-replace-all scanner-p-empty str ""))
         (str (cl-ppcre:regex-replace-all scanner-p-open str "<p>"))
         (str (cl-ppcre:regex-replace-all scanner-p-close str "</p>")))
    str))

(defun cleanup-escaped-characters (str)
  "Convert escaped characters back to non-escaped."
  (cl-ppcre:regex-replace-all
    "{{markdown\\.cl\\|escaped\\|(.+?)}}"
    str
    (lambda (match &rest regs)
      (let* ((regs (cddddr regs))
             (rs (car regs))
             (re (cadr regs))
             (id (subseq match (aref rs 0) (aref re 0))))
        (gethash id *tmp-storage*)))))

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

(defun parse (markdown-string &key disable-parsers)
  "Parse a markdown string into HTML."
  (when (string= (string-trim '(#\newline #\space) markdown-string) "")
    (return-from parse markdown-string))
  (let* ((*html-chunks* (if *html-chunks*
                            *html-chunks*
                            (make-hash-table :test #'eq)))
         (*link-references* (if *link-references*
                                *link-references*
                                (make-hash-table :test #'equal)))
         (*tmp-storage* (if *tmp-storage*
                            *tmp-storage*
                            (make-hash-table :test #'equal)))
         (str markdown-string)  ; i don't want to hear it
         (str (pre-process-markdown-html str))
         (str (prepare-markdown-string str))
         (handlers-pre-block '(parse-escaped-characters
                               gather-link-references
                               parse-atx-headers
                               parse-setext-headers
                               parse-embedded-code
                               parse-embedded-blockquote
                               parse-horizontal-rule
                               pre-format-paragraph-lists
                               convert-lazy-blockquote-to-standard))
         (handlers-post-block '(parse-blockquote
                                parse-code
                                parse-br
                                parse-links
                                parse-lists
                                parse-paragraphs
                                parse-inline-code
                                parse-em))
         (handlers-post-reduce '(parse-entities
                                 cleanup-code
                                 cleanup-newlines
                                 cleanup-hr
                                 cleanup-paragraphs
                                 cleanup-escaped-characters
                                 post-process-markdown-html)))
    (dolist (handler handlers-pre-block)
      (unless (find handler disable-parsers)
        (setf str (funcall handler str))))
    ;(format t "---str---~%~a~%~%" str)
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

      (let ((str (reduce (lambda (a b) (concatenate 'string a *nl* b)) (reverse new-blocks))))
        (dolist (handler handlers-post-reduce)
          (unless (find handler disable-parsers)
            (setf str (funcall handler str))))
        str))))

(defun parse-file (path &key disable-parsers)
  "Parse a markdown file into HTML (returned as a string)."
  (let ((contents (file-contents path)))
    (parse contents :disable-parsers disable-parsers)))

