(in-package :markdown.cl)

(define-condition error-parsing-html (error) ()
  (:documentation "Thrown then xmls cannot parse the HTML in a document (make
                   sure your <img> tags are closed."))

(defparameter *block-level-elements*
  '(address article aside audio blockquote canvas dd 
    div dl fieldset figcaption figure footer form h1 h2 h3 h4 h5 h6 header hgroup 
    hr noscript ol output p pre section table tfoot ul video script)
  "Stores all HTML tags considered block-level.")

(defparameter *html-chunks* nil
  "Holds a hash table that harbors HTML tags from the destructive forces of
   markdown parsing until they are ready to be injected back into the document.")

(defun escape-html-entities (str)
  "Hide HTML entities from the HTML parser. It doesn't like them. It has the
   death penalty on 12 systems."
  (let* ((str (cl-ppcre:regex-replace-all "&(#[0-9]{1,5}|\\w{2,6});" str "{{markdown.cl|entity|\\1}}"))
         (str (cl-ppcre:regex-replace-all "<br\\s?/?>" str "{{markdown.cl|br}}"))
         (str (cl-ppcre:regex-replace-all "&" str "{{markdown.cl|amp}}")))
    str))

(defun block-element-p (tag-name)
  "Test if a given HTML tag is a block-level element."
  (let ((tag-sym (if (symbolp tag-name)
                     tag-name
                     (intern (string-upcase tag-name) :markdown.cl))))
    (find tag-sym *block-level-elements*)))

(defun fix-a-tags (str)
  "XMLS mangles our <a> tags. Fix"
  (cl-ppcre:regex-replace-all
    (cl-ppcre:create-scanner "<a(([\\s\\n]*[a-zA-Z-]+=\".*?\")*)/>" :single-line-mode t)
    str
    (concatenate 'string "<a\\1></a>" *nl*)))

(defun fix-inline-links (str)
  "Fix <http://teh-link.com> links, which messes with XMLS' mind."
  (cl-ppcre:regex-replace-all "<([a-z]+://[^ ]+)>" str "<a href=\"\\1\">\\1</a>"))

(defun stash-html-block-tags (str)
  "Finds all top-level HTML block-level tags and saves them for later. Does so
   by incrementally searching for the next line starting with a block-level tag
   and using xmls to parse it, adding a placeholder in its stead. Inline
   elements are just added back into the parts array (not saved). This allows
   them to be markdown-processed. str is modified destructively as the loop
   progresses, making sure we don't get stuck in endless loop finding the same
   tags over again."
  ;; create a scanner that searches for heml tags at the beginning of a line
  (let* ((html-scanner (cl-ppcre:create-scanner
                         "(^|\\n)<[\\w]+(\\s?[a-zA-Z-]+=\".*?\")*/?>"
                         :single-line-mode t))
         (parts nil)
         (block-id 0)
         ;; xmls hates non-closed image tags, so add a slash to the end of them
         ;; (XHTML style)
         (str (cl-ppcre:regex-replace-all
                (cl-ppcre:create-scanner "(<img\\s+.*?>)" :single-line-mode t)
                str
                (lambda (match &rest regs)
                  (let* ((regs (cddddr regs))
                         (rs (car regs))
                         (re (cadr regs))
                         (text (subseq match (aref rs 0) (aref re 0))))
                    (cl-ppcre:regex-replace-all "/?>$" text "/>")))))
         ;; xmls also destroys any script tags with excessive escaping. give
         ;; them safe passage using the block-replace method
         (str (cl-ppcre:regex-replace-all
                (cl-ppcre:create-scanner "(<script.*?</script>)" :single-line-mode t)
                str
                (lambda (match &rest regs)
                  (let* ((regs (cddddr regs))
                         (rs (car regs))
                         (re (cadr regs))
                         (text (subseq match (aref rs 0) (aref re 0)))
                         (id (incf block-id)))
                    (setf (gethash id *html-chunks*) text)
                    (format nil "~a{{markdown.cl|htmlblock|~a}}~a" *nl* id *nl*))))))
    (loop for pos = (cl-ppcre:scan html-scanner str)
          while pos do
      (push (subseq str 0 pos) parts)
      (let* ((xml-tree (concatenate 'string "<cl-markdown>" (subseq str pos) "</cl-markdown>"))
             (tree (xmls:parse xml-tree :compress-whitespace nil))
             (children (xmls:node-children tree))
             (child (car children))
             (next (cdr children)))
        ;; push any non-html into parts, updating children as we go along
        (loop while (stringp child) do
          (push child parts)
          (setf children (cdr children)
                next (cdr children)
                child (car children)))
        (unless children
          (error 'error-parsing-html))
        (if (block-element-p (xmls:node-name child))
            (let ((id (incf block-id)))
              (setf (gethash id *html-chunks*) (xmls:toxml child))
              (push (format nil "~a~a{{markdown.cl|htmlblock|~a}}~a~a" *nl* *nl* id *nl* *nl*) parts))
            (push (fix-a-tags (xmls:toxml child)) parts))
        (let* ((next (mapcar
                       (lambda (child)
                         (if (stringp child)
                             child
                             (xmls:toxml child)))
                       next))
               (next (reduce (lambda (&optional a b)
                               (concatenate 'string a *nl* *nl* b))
                             next)))
          (setf str next))))
    (let* ((final (reduce (lambda (&optional a b)
                            (concatenate 'string a *nl* *nl* b))
                          (reverse parts)))
           (final (concatenate 'string final str)))
      final)))

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
         (str (cl-ppcre:regex-replace-all "{{markdown\\.cl\\|entity\\|(.*?)}}" str "&\\1;"))
         (str (cl-ppcre:regex-replace-all "{{markdown\\.cl\\|lt}}" str "<"))
         (str (cl-ppcre:regex-replace-all "{{markdown\\.cl\\|gt}}" str ">"))
         (str (cl-ppcre:regex-replace-all "{{markdown\\.cl\\|br}}" str "<br/>")))
    str))

(defun pre-process-markdown-html (str)
  "This function performs any needed parsing on existing HTML of a markdown string."
  (let* ((str (escape-html-entities str))
         (str (fix-inline-links str))
         (str (stash-html-block-tags str)))
    str))

(defun post-process-markdown-html (str)
  "This function does any needed cleanup to marry inline HTML and markdown."
  ;; note we do TWO passes of block replacement. this is intentional!!! in some
  ;; instances a block will be nested in another (mainly script tags)
  (let* ((str (replace-html-blocks str))
         (str (replace-html-blocks str))
         (str (replace-html-blocks str))
         (str (fix-inline-links str))
         (str (cleanup-markdown-tags str)))
    str))

