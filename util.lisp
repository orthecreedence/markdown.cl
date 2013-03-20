(in-package :markdown)

(defparameter *nl* (coerce #(#\newline) 'string)
  "Holds a string of a single newline character.")

(defun file-contents (path)
  "Sucks up an entire file from PATH into a freshly-allocated string,
   returning two values: the string and the number of bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

