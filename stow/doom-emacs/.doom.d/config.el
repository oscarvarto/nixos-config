;;; $DOOMDIR/config.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Fix locale settings to prevent "LANG=en_MX.UTF-8 cannot be used" warning
;; This happens because macOS system locale is set to en_MX but that locale doesn't exist
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;; Load Path Setup
(defvar my/config-el-dir doom-user-dir)

;; Add path to core utilities and load them
(add-to-list 'load-path (expand-file-name "config/core" my/config-el-dir))
(require 'my-paths) ; Make my/doom-private-dir available

;; Now load other core configs using require (they are in config/core/)
(require 'my-defaults-config)
(require 'my-banner-config)
(require 'my-gui-appearance-config)
(require 'my-project-cleanup-config)

;; Helper function for loading
(defun my/load-config (feature group) ; Group is mandatory
  "Load a configuration file FEATURE from GROUP subdirectory."
  ;; Paths are relative to this file (config.el)
  (let ((path (format "config/%s/my-%s-config" group feature)))
    (load! path)))

(my/load-config 'tree-sitter 'misc)
(my/load-config 'large-files 'misc)
(my/load-config 'yasnippet 'misc)

(my/load-config 'treemacs 'ui)

(my/load-config 'lsp 'lsp)

(my/load-config 'jdk 'jvm)
(my/load-config 'jvm-build 'jvm)
(my/load-config 'jvm-compile 'jvm)
(my/load-config 'jvm-navigation 'jvm)
(my/load-config 'lsp-java 'jvm)
(my/load-config 'clojure 'jvm)
(my/load-config 'scala 'jvm)

(my/load-config 'rust 'languages)
(my/load-config 'python 'languages)

;;; Tool-specific configurations
(my/load-config 'mu4e 'misc)
;;(my/load-config 'vterm 'misc)
(my/load-config 'eat 'misc)

(my/load-config 'tabnine-gui 'ai)
(my/load-config 'gptel 'ai)

(my/load-config 'db 'misc)

;; Trying dape
;; (my/load-config 'dap 'lsp)

;;; Org mode configuration
;;(my/load-config 'obsidian 'writing)
(my/load-config 'org 'writing)

;;; External application interfaces
(my/load-config 'eaf 'misc)

;;; Fix for daemon-mode and hook errors
;; Complete fix for the "wrong-type-argument number-or-marker-p nil" error
;; This error occurs specifically with emacsclient (daemon mode) and certain file types like Rust

;; Daemon-mode specific fixes
;; (when (daemonp)
;;   (message "Emacs daemon detected, applying daemon-specific safety measures")
;;
;;   ;; Ensure environment variables are properly set in daemon mode
;;   (defun my/setup-daemon-environment ()
;;     "Set up environment variables for daemon mode."
;;     (let ((path-from-shell (shell-command-to-string "$SHELL -c 'echo $PATH'")))
;;       (when (and path-from-shell (not (string-empty-p (string-trim path-from-shell))))
;;         (setenv "PATH" (string-trim path-from-shell))
;;         (setq exec-path (split-string (string-trim path-from-shell) path-separator))))
;;     (message "Daemon environment setup completed"))
;;
;;   ;; Set up environment after daemon initialization
;;   (add-hook 'server-after-make-frame-hook #'my/setup-daemon-environment)
;;
;;   ;; Add safety wrapper for mode hooks in daemon mode
;;   (defun my/safe-mode-hook-wrapper (original-hook)
;;     "Safely wrap mode hooks to prevent daemon-mode errors."
;;     (lambda (&rest args)
;;       (condition-case err
;;           (apply original-hook args)
;;         (error
;;          (message "Warning: Mode hook failed in daemon mode: %s" err)))))
;;
;;   ;; Apply safe wrappers to problematic hooks after some delay
;;   (run-with-timer 2 nil
;;                   (lambda ()
;;                     (message "Applying daemon-mode hook safety wrappers"))))

;; Remove the problematic hooks immediately (before any packages load)
;; (setq doom-first-file-hook (delq 'global-git-commit-mode doom-first-file-hook))
;; (setq doom-first-file-hook (delq 'recentf-mode doom-first-file-hook))
;;
;; ;; Also remove them from the hook variable directly
;; (remove-hook 'doom-first-file-hook 'global-git-commit-mode)
;; (remove-hook 'doom-first-file-hook 'recentf-mode)
;;
;; ;; Wrap all file hooks with error handling to prevent crashes
;; (defun my/safe-run-hooks (hook-var &rest args)
;;   "Safely run hooks with error handling."
;;   (dolist (hook (symbol-value hook-var))
;;     ;; Only try to run actual functions, skip non-function values
;;     (cond
;;      ((functionp hook)
;;       (condition-case err
;;           (if args
;;               (apply hook args)
;;             (funcall hook))
;;         (error
;;          (message "Warning: Hook %s in %s failed: %s" hook hook-var err))))
;;      ((eq hook t)
;;       ;; Skip 't' values that sometimes get added to hooks
;;       nil)
;;      (t
;;       (message "Warning: Non-function value %s found in %s, skipping" hook hook-var)))))
;;
;; ;; Override the problematic doom-run-hook function temporarily
;; (defun my/safe-doom-run-hook (hook)
;;   "Safely run a doom hook with comprehensive error handling."
;;   (when (boundp hook)
;;     (my/safe-run-hooks hook)))
;;
;; ;; Debug function to help identify the source of the error
;; (defun my/debug-file-hooks ()
;;   "Debug function to identify problematic hooks."
;;   (message "doom-first-file-hook contents: %S" doom-first-file-hook)
;;   (message "find-file-hook contents: %S" find-file-hook))
;;
;; ;; Add debugging to after-init
;; (add-hook 'doom-after-init-hook #'my/debug-file-hooks)
;;
;; ;; Safe way to enable git-commit-mode later if needed
;; (defun my/enable-git-commit-when-safe ()
;;   "Enable git-commit-mode only when it's safe to do so."
;;   (when (and (fboundp 'global-git-commit-mode)
;;              (boundp 'global-git-commit-mode))
;;     (condition-case err
;;         (unless global-git-commit-mode
;;           (global-git-commit-mode 1)
;;           (message "Successfully enabled global-git-commit-mode"))
;;       (error
;;        (message "Could not enable global-git-commit-mode: %s" err)))))
;;
;; ;; Enable git-commit-mode after everything is loaded
;; (add-hook 'doom-after-init-hook #'my/enable-git-commit-when-safe 90)
;;
;; ;; Safe way to enable recentf-mode later if needed
;; (defun my/enable-recentf-when-safe ()
;;   "Enable recentf-mode only when it's safe to do so."
;;   (when (fboundp 'recentf-mode)
;;     (condition-case err
;;         (unless recentf-mode
;;           (recentf-mode 1)
;;           (message "Successfully enabled recentf-mode"))
;;       (error
;;        (message "Could not enable recentf-mode: %s" err)))))
;;
;; ;; Enable recentf-mode after everything is loaded
;; (add-hook 'doom-after-init-hook #'my/enable-recentf-when-safe 80)
;;
;; ;; Function to clean up invalid hook values
;; (defun my/clean-hook-values (hook-var)
;;   "Remove invalid values (like t) from HOOK-VAR."
;;   (when (boundp hook-var)
;;     (let* ((original-hooks (symbol-value hook-var))
;;            (cleaned-hooks (cl-remove-if-not #'functionp original-hooks))
;;            (removed-count (- (length original-hooks) (length cleaned-hooks))))
;;       (set hook-var cleaned-hooks)
;;       (when (> removed-count 0)
;;         (message "Cleaned %s, removed %d invalid values" hook-var removed-count)))))
;;
;; ;; Clean up find-file-hook after initialization
;; (defun my/cleanup-hooks ()
;;   "Clean up hooks that might contain invalid values."
;;   (my/clean-hook-values 'find-file-hook)
;;   (my/clean-hook-values 'doom-first-file-hook))
;;
;; ;; Run cleanup after initialization
;; (add-hook 'doom-after-init-hook #'my/cleanup-hooks 70)
;;
;; ;; Additional safety: wrap find-file-hook execution
;; (defun my/safe-find-file-hook ()
;;   "Safely run find-file-hook."
;;   (my/safe-run-hooks 'find-file-hook))
;;
;; ;; Replace the standard find-file-hook runner with our safe version
;; ;; Only apply advice to find-file-hook, not all hooks
;; (advice-add 'run-hooks :around
;;             (lambda (orig-fun &rest hooks)
;;               (if (and (= (length hooks) 1)
;;                        (eq (car hooks) 'find-file-hook))
;;                   (my/safe-run-hooks 'find-file-hook)
;;                 (apply orig-fun hooks))))
