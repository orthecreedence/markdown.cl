(in-package :markdown.cl)

(defparameter *block-level-elements*
  '(address article aside audio blockquote canvas dd 
    div dl fieldset figcaption figure footer form h1 h2 h3 h4 h5 h6 header hgroup 
    hr noscript ol output p pre section table tfoot ul video)
  "Stores all HTML tags considered block-level.")

(defparameter *html-chunks* nil
  "Holds a hash table that harbors HTML tags from the destructive forces of
   markdown parsing until they are ready to be injected back into the document.")

(defun block-element-p (tag-name)
  "Test if a given HTML tag is a block-level element."
  (let ((tag-sym (if (symbolp tag-name)
                     tag-name
                     (intern (string-upcase tag-name) :markdown.cl))))
    (find tag-sym *block-level-elements*)))

(defun stash-html-block-tags (str)
  "Finds all top-level HTML block-level tags and saves them for later."
  ;; note we have to convert all &, <, > to {{markdown.cl|amp,lt,gt}} so that
  ;; XML parsing works. they are converted back both before markdown parsing and
  ;; after HTML blocks are placed back into the doc
  (let* ((str (cl-ppcre:regex-replace-all "&" str "{{markdown.cl|amp}}"))
         (str (do-parse-entities str :use-markdown-tags t))
         ;; save code blocks from the impending HTML blockage
         (str (cl-ppcre:regex-replace-all
                (cl-ppcre:create-scanner "(^|\\n\\n)( {4,})<" :single-line-mode t)
                str "\\1\\2{{markdown.cl|lt}}"))
         (str (cl-ppcre:regex-replace-all "<br/?>" str "{{markdown.cl|br}}"))
         (tree (xmls:parse (concatenate 'string "<markdown>" str "</markdown>")))
         (children (cddr tree))
         (parts nil)
         (block-id 0))
    (dolist (child children)
      (cond ((stringp child)
             (push child parts))
            ((block-element-p (car child))
             (let ((id (incf block-id)))
               (setf (gethash id *html-chunks*) (xmls:toxml child))
               (push (format nil "~a{{markdown.cl|htmlblock|~a}}~a" *nl* id *nl*) parts)))
            (t
             ;; fix xmls' slefless act of converting <a href=""></a> into
             ;; <a href=""/> (self-closing tags)
             (let* ((child (append (list (car child))
                                   (list (cadr child))
                                   (list "{{markdown.cl|nil}}")
                                   (cddr child)))
                    (html (xmls:toxml child))
                    (html (cl-ppcre:regex-replace-all "{{markdown\\.cl\\|nil}}" html "")))
               (push html parts)))))
    (let* ((str (reduce (lambda (&optional a b)
                          (concatenate 'string a *nl* *nl* b))
                        (reverse parts)))
           ;; convert escaped characters back (fixing code blocks in the process)
           (str (cl-ppcre:regex-replace-all "{{markdown\\.cl\\|amp}}" str "&"))
           (str (cl-ppcre:regex-replace-all "{{markdown\\.cl\\|lt}}" str "<"))
           (str (cl-ppcre:regex-replace-all "{{markdown\\.cl\\|gt}}" str ">")))
      str)))

(defun replace-html-blocks (str)
  "Find any {{markdown.cl|htmlblock|...}} tags and replace them with their saved
   content."
  (cl-ppcre:regex-replace-all
    (cl-ppcre:create-scanner "(<p>\\s*)?{{markdown\\.cl\\|htmlblock\\|([0-9]+)}}(\\s*</p>)?"
                             :single-line-mode t)
    str
    (lambda (match &rest regs)
      (let* ((regs (cddddr regs))
             (rs (car regs))
             (re (cadr regs))
             (id (parse-integer (subseq match (aref rs 1) (aref re 1))))
             (html (gethash id *html-chunks*)))
        (if html html "")))))

(defun cleanup-markdown-tags (str)
  (let* ((str (cl-ppcre:regex-replace-all "{{markdown\\.cl\\|amp}}" str "&"))
         (str (cl-ppcre:regex-replace-all "{{markdown\\.cl\\|lt}}" str "<"))
         (str (cl-ppcre:regex-replace-all "{{markdown\\.cl\\|gt}}" str ">"))
         (str (cl-ppcre:regex-replace-all "{{markdown\\.cl\\|br}}" str "<br/>")))
    str))

(defun pre-process-markdown-html (str)
  "This function performs any needed parsing on existing HTML of a markdown string."
  (stash-html-block-tags str))

(defun post-process-markdown-html (str)
  "This function does any needed cleanup to marry inline HTML and markdown."
  (let* ((str (replace-html-blocks str))
         (str (cleanup-markdown-tags str)))
    str))

