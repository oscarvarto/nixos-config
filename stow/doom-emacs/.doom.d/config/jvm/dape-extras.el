;;; dape-extras.el -*- lexical-binding: t; no-byte-compile: t;-*-

(defun dape-find-file (&optional default)
  "Read filename without any ignored extensions at project root.
DEFAULT specifies which file to return on empty input."
  (let ((completion-ignored-extensions nil)
        (default-directory (funcall dape-cwd-fn)))
    (expand-file-name
     (read-file-name (if default
                         (format "Program (default %s): " default)
                       "Program : ")
                     default-directory
                     default t))))

(defun dape-find-file-buffer-default ()
  "Read filename at project root, defaulting to current buffer."
  (dape-find-file (buffer-file-name)))

(provide 'dape-extras)
