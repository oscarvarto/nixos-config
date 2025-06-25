;; -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'my-paths) ; Ensure my/get-path is available

(add-load-path! (my/get-path :eaf)) ; Use path defined in my-paths.el

(use-package! eaf
  :custom
  ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker nil)
  (browse-url-browser-function 'eaf-open-browser)
  (setq eaf-pyqterminal-font-family "MonoLisaVariable Nerd Font")
  :config

  (defalias 'browse-web #'eaf-open-browser)
  (require 'eaf-browser)

  (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki

(provide 'my-eaf-config)
