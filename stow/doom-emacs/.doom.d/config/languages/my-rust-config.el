;;; my-rust-config.el -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'lsp-clangd)

(setq lsp-clients-clangd-args
      '("-j=8"
        "--background-index"
        "--clang-tidy"
        "--completion-style=detailed"
        "--header-insertion=never"
        "--header-insertion-decorators=0"))
(set-lsp-priority! 'clangd 2)

(custom-set-faces
 '(rustic-compilation-column ((t (:inherit compilation-column-number))))
 '(rustic-compilation-line ((t (:foreground "fuchsia")))))

(defun rustic-mode-auto-save-hook ()
  "Enable auto-saving in rustic-mode buffers."
  (when buffer-file-name
    (setq-local compilation-ask-about-save nil)))
(add-hook 'rustic-mode-hook 'rustic-mode-auto-save-hook)

(setq rustic-rustfmt-args "+nightly")
(setq rustic-rustfmt-config-alist '((hard_tabs . t) (skip_children . nil)))

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))

(use-package rustic
  :after lsp-mode
  :config
  (setq rustic-format-on-save nil)
  :custom
  (rustic-cargo-use-last-stored-arguments t)
  (let ((toolchain (or (string-trim (shell-command-to-string "rustup default | cut -d'-' -f1"))
                       (getenv "RUST_TOOLCHAIN")
                       "nightly")))
    (setq rustic-analyzer-command `("rustup" "run" ,toolchain "rust-analyzer")))
  ;; (setq rustic-analyzer-command `("rustup" "run" "nightly" "rust-analyzer"))
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  ;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil))

(with-eval-after-load 'rustic-mode
  (add-hook! 'rustic-mode-hook #'lsp-ui-mode #'flymake-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
