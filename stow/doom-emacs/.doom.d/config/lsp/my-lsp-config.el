;;; my-lsp-config.el  -*- lexical-binding: t; no-byte-compile: t; -*-
(setenv "NODE_PATH" "/Users/oscarvarto/.volta/bin")

(with-eval-after-load 'lsp-mode
  ;;(setq lsp-enable-file-watchers nil)
  (setq lsp-file-watch-threshold 1000)
  (require 'cl-lib)

  (cl-pushnew "[/\\\\]target\\'" lsp-file-watch-ignored-directories :test #'equal)
  (cl-pushnew "/Users/oscarvarto/\\.cargo\\(/.*\\|\\'\\)"
              lsp-file-watch-ignored-directories
              :test #'equal)
  (cl-pushnew "[/\\\\]dependencies\\'" lsp-file-watch-ignored-directories :test #'equal)
  ;; Ignore nested target directories
  (cl-pushnew "[/\\\\]target[/\\\\].*" lsp-file-watch-ignored-directories :test #'equal)
  ;; Add ZIP file pattern using cl-pushnew to avoid duplicates
  (cl-pushnew "\\.zip\\'"  ; Double escaping for Emacs strings
              lsp-file-watch-ignored-files
              :test #'equal))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-indentation nil)
  (setq lsp-log-io nil)
  :custom
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable t)
  :hook ((java-ts-mode java-mode) . lsp)
  :hook ((java-ts-mode java-mode) . ts-fold-indicators-mode)
  :hook ((scala-ts-mode scala-mode) . lsp)
  :hook ((scala-ts-mode scala-mode) . ts-fold-indicators-mode)
  :hook ((typescript-ts-mode typescript-mode) . lsp)
  :hook ((typescript-ts-mode typescript-mode) . ts-fold-indicators-mode)
  ;; if you want which-key integration
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

(use-package lsp-ui
  :init
  (setq lsp-ui-doc-popup-enabled t
        lsp-ui-doc-popup-max-width 0.8
        lsp-ui-peek-always-show nil)
  :commands lsp-ui-mode)

(use-package! eldoc-box
  :custom
  (eldoc-box-only-multi-line t))

(map! :map (emacs-lisp-mode-map lsp-mode-map)
      :localleader
      :desc "Show docs at point" "<f1>" #'eldoc-box-help-at-point)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(load! "my-lsp-booster-config")
;; (load! "my-lsp-java-config")
;; (require 'dap-netcore)
;; (setq dap-netcore-download-url "file:///Users/oscarvarto/git-repos/netcoredbg/netcoredbg-osx-arm64.tar.gz")

(load! "my-lsp-nu-config")

;; Enable word wrap in echo area
(setq resize-mini-windows t)
(setq truncate-partial-width-windows nil)
(setq truncate-lines nil)

;; Specifically for eldoc
(setq eldoc-echo-area-use-multiline-p t)

;; Set maximum width for echo area (120 characters)
(setq max-mini-window-height 0.5)
(setq resize-mini-windows-safe t)
(setq truncate-string-ellipsis "...")
(setq message-truncate-lines nil)

(setq-default fill-column 120)
(add-hook 'help-mode-hook #'visual-line-mode)
(add-hook 'help-mode-hook #'visual-fill-column-mode)

(provide 'my-lsp-config)
