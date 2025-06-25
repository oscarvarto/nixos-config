;;; my-large-files-config.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; Configuration for handling large files efficiently
;; Disables TreeSitter and other heavy features for large files

;; Threshold for considering a file "large" (in bytes)
;; Default: 512KB (512 * 1024 bytes)
(defvar my/large-file-threshold (* 512 1024)
  "File size threshold in bytes above which files are considered large.
Large files will have TreeSitter and other heavy features disabled.")

;; Alternative thresholds you can uncomment:
;; (setq my/large-file-threshold (* 1024 1024))   ; 1MB
;; (setq my/large-file-threshold (* 2 1024 1024)) ; 2MB
;; (setq my/large-file-threshold (* 5 1024 1024)) ; 5MB

;; Features to disable for large files
(defvar my/large-file-disabled-features
  '(tree-sitter-mode
    tree-sitter-hl-mode
    line-reminder-mode
    ts-fold-mode
    ts-fold-indicators-mode
    combobulate-mode
    lsp-mode
    company-mode
    corfu-mode
    flycheck-mode
    syntax-highlighting
    font-lock-mode
    hl-line-mode)
  "List of features to disable for large files.")

;; Function to check if current buffer contains a large file
(defun my/large-file-p ()
  "Return t if current buffer is visiting a large file."
  (when buffer-file-name
    (let* ((attrs (file-attributes buffer-file-name))
           (file-size (and attrs (nth 7 attrs))))
      (and file-size
           (numberp file-size)
           (> file-size my/large-file-threshold)))))

;; Function to disable heavy features for large files
(defun my/disable-features-for-large-file ()
  "Disable heavy features when opening large files."
  (when (my/large-file-p)
    (let* ((attrs (file-attributes buffer-file-name))
           (file-size (and attrs (nth 7 attrs))))
      (when file-size
        (message "Large file detected (%s bytes). Disabling heavy features for better performance."
                 (file-size-human-readable file-size))))

    ;; Disable TreeSitter specifically
    (when (bound-and-true-p tree-sitter-mode)
      (tree-sitter-mode -1))
    (when (bound-and-true-p tree-sitter-hl-mode)
      (tree-sitter-hl-mode -1))

    ;; Disable other TreeSitter-related features
    (when (bound-and-true-p ts-fold-mode)
      (ts-fold-mode -1))
    (when (bound-and-true-p ts-fold-indicators-mode)
      (ts-fold-indicators-mode -1))
    (when (bound-and-true-p combobulate-mode)
      (combobulate-mode -1))
    ;; Disable line-reminder with additional cleanup
    (when (bound-and-true-p line-reminder-mode)
      (condition-case nil
          (progn
            (line-reminder-mode -1)
            ;; Clean up any existing overlays to prevent errors
            (when (fboundp 'line-reminder--clear-all-ovs)
              (line-reminder--clear-all-ovs))
            ;; Remove any timers that might cause issues
            (when (boundp 'line-reminder--timer)
              (when line-reminder--timer
                (cancel-timer line-reminder--timer)
                (setq line-reminder--timer nil))))
        (error nil)))

    ;; Disable LSP for large files
    (when (bound-and-true-p lsp-mode)
      (lsp-mode -1))

    ;; Disable completion backends
    (when (bound-and-true-p company-mode)
      (company-mode -1))
    (when (bound-and-true-p corfu-mode)
      (corfu-mode -1))

    ;; Disable syntax checking
    (when (bound-and-true-p flycheck-mode)
      (flycheck-mode -1))

    ;; Optionally disable font-lock (syntax highlighting) for very large files
    ;; Uncomment the next lines if you want to disable syntax highlighting entirely
    ;; (when (bound-and-true-p font-lock-mode)
    ;;   (font-lock-mode -1))
    ;; (when (bound-and-true-p global-font-lock-mode)
    ;;   (setq-local font-lock-defaults nil))

    ;; Switch to fundamental-mode for very large files (optional)
    ;; Uncomment if you want to completely disable major mode features
    ;; (fundamental-mode)

    ;; Set some buffer-local variables for better performance
    (setq-local bidi-display-reordering nil)
    (setq-local bidi-paragraph-direction 'left-to-right)
    (setq-local buffer-read-only nil) ; Ensure we can still edit

    ;; Disable undo for large files (optional - uncomment if needed)
    ;; (setq-local buffer-undo-list t)

    ;; Display a message about what was disabled
    (let* ((attrs (file-attributes buffer-file-name))
           (file-size (and attrs (nth 7 attrs))))
      (when file-size
        (message "TreeSitter and heavy features disabled for large file. File size: %s"
                 (file-size-human-readable file-size))))))

;; Enhanced version of the TreeSitter check that considers file size
(defun my/should-enable-tree-sitter-p-with-size-check ()
  "Check if tree-sitter should be enabled, considering file size."
  (and (not (my/large-file-p))
       (not (memq major-mode my/tree-sitter-disabled-modes))
       (my/tree-sitter-language-available-p major-mode)))

;; Hook to check file size when opening files
(defun my/check-file-size-on-find-file ()
  "Check file size and disable features if necessary when opening files."
  (my/disable-features-for-large-file))

;; Hook to check file size when switching buffers
(defun my/check-file-size-on-buffer-switch ()
  "Check file size when switching to a buffer."
  (when (and buffer-file-name
             (my/large-file-p))
    (my/disable-features-for-large-file)))

;; User command to manually toggle large file mode
(defun my/toggle-large-file-mode ()
  "Manually toggle large file optimizations for current buffer."
  (interactive)
  (if (get 'my/large-file-mode 'state)
      (progn
        (put 'my/large-file-mode 'state nil)
        (message "Large file mode disabled. Heavy features may be re-enabled."))
    (progn
      (my/disable-features-for-large-file)
      (put 'my/large-file-mode 'state t)
      (message "Large file mode enabled. Heavy features disabled."))))

;; User command to set custom threshold
(defun my/set-large-file-threshold (size)
  "Set custom threshold for large files.
SIZE should be in bytes."
  (interactive "nEnter file size threshold in bytes: ")
  (setq my/large-file-threshold size)
  (message "Large file threshold set to %s (%s)"
           size
           (file-size-human-readable size)))

;; Add hooks
(add-hook 'find-file-hook #'my/check-file-size-on-find-file)
(add-hook 'buffer-list-update-hook #'my/check-file-size-on-buffer-switch)

;; Integration with existing TreeSitter configuration
;; Override the existing function to include size check
(eval-after-load 'my-tree-sitter-config
  '(progn
     (fset 'my/should-enable-tree-sitter-p 'my/should-enable-tree-sitter-p-with-size-check)
     (message "Large file support integrated with TreeSitter configuration")))

;; Key bindings
(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Toggle large file mode" "L" #'my/toggle-large-file-mode))

(provide 'my-large-files-config)
