(in-package :markdown.cl-test)

(defun run-tests ()
  (run! 'markdown.cl-test)
  (run! 'markdown.cl-table-test))
