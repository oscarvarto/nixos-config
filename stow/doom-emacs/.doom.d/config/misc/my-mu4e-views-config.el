;;; my-mu4e-views-config -*- lexical-binding: t; no-byte-compile: t; -*-

;; Load mu4e-views after mu4e is loaded
(global-set-key (kbd "M-s-z") 'mu4e-views-mu4e-headers-windows-only)

;; Function to check if current client supports GUI features
(defun mu4e-views-client-supports-gui-p ()
  "Return t if current client supports GUI features like webkit, nil otherwise."
  (and (display-graphic-p)
       ;; Additional checks for GUI capabilities
       (not (getenv "TERM"))  ;; TERM is set in terminal emacsclient
       ;; Check if we can create GUI widgets
       (condition-case nil
           (progn
             ;; Try to access window system
             (when (and (boundp 'window-system) window-system)
               ;; Check if we have actual GUI display and webkit support
               (and (display-pixel-width)
                    (fboundp 'make-xwidget)
                    (featurep 'xwidget-internal)))
             t)
         (error nil))))
(use-package! mu4e-views
    ;; :after mu4e
    :config
    ;; Set default view method based on client capabilities
    (if (mu4e-views-client-supports-gui-p)
        (progn
          (message "Configuring mu4e-views for GUI client with webkit support")
          (setq mu4e-views-default-view-method "html-nonblock"))
      (progn
        (message "Configuring mu4e-views for terminal client - using text view")
        (setq mu4e-views-default-view-method "text")))
    
    (setq mu4e-views-next-previous-message-behaviour 'always-switch-to-headers) ;; when pressing n and p stay in the current window
    (setq mu4e-views-mu4e-html-email-header-style
        "<style type=\"text/css\">
.mu4e-mu4e-views-mail-headers { font-family: sans-serif; font-size: 10pt; margin-bottom: 30px; padding-bottom: 10px; border-bottom: 1px solid #ccc; color: #000; background: white; }
.mu4e-mu4e-views-header-row { display:block; padding: 1px 0 1px 0; }
.mu4e-mu4e-views-mail-header { display: inline-block; text-transform: capitalize; font-weight: bold; }
.mu4e-mu4e-views-header-content { display: inline-block; padding-right: 8px; }
.mu4e-mu4e-views-email { display: inline-block; padding-right: 8px; }
.mu4e-mu4e-views-attachment { display: inline-block; padding-right: 8px; }
</style>")

    ;; Function to dynamically switch view method based on current client
    (defun mu4e-views-configure-for-current-client ()
      "Configure mu4e-views based on current client capabilities."
      (if (mu4e-views-client-supports-gui-p)
          (progn
            (message "Switching to HTML view for GUI client")
            (mu4e-views-mu4e-use-view-msg-method "html-nonblock"))
        (progn
          (message "Switching to text view for terminal client")
          (mu4e-views-mu4e-use-view-msg-method "text"))))
    
    ;; Set the default view method based on client type
    (mu4e-views-configure-for-current-client)
    
    ;; Add hook to reconfigure when opening messages
    (add-hook 'mu4e-view-mode-hook #'mu4e-views-configure-for-current-client)

    (map! :map mu4e-headers-mode-map
          :n "M-b" #'mu4e-views-cursor-msg-view-window-up
          :n "M-f" #'mu4e-views-cursor-msg-view-window-down
          :localleader
          :desc "Message action"        "a"   #'mu4e-views-mu4e-view-action
          :desc "Scroll message down"   "b"   #'mu4e-views-cursor-msg-view-window-up
          :desc "Scroll message up"     "f"   #'mu4e-views-cursor-msg-view-window-down
          :desc "Open attachment"       "o"   #'mu4e-views-mu4e-view-open-attachment
          :desc "Save attachment"       "s"   #'mu4e-views-mu4e-view-save-attachment
          :desc "Save all attachments"  "S"   #'mu4e-views-mu4e-view-save-all-attachments
          :desc "Set view method"       "v"   #'mu4e-views-mu4e-select-view-msg-method)) ;; select viewing method

  ;; Evil bindings for xwidget webkit browsers
(map! :map xwidget-webkit-mode-map
      ;;:n "M-s-z" #'quit-window
      :n "Z Z" #'mu4e-views-mu4e-headers-windows-only ;;close-xwidget-mail-window
      :n "gr"  #'xwidget-webkit-reload
      :n "y"   #'xwidget-webkit-copy-selection-as-kill
      :n "s-c" #'xwidget-webkit-copy-selection-as-kill
      :n "t"   #'xwidget-webkit-browse-url
      :n "TAB" #'xwidget-webkit-forward
      :n "C-o" #'xwidget-webkit-back
      :n "G"   #'xwidget-webkit-scroll-bottom
      :n "gg"  #'xwidget-webkit-scroll-top
      :n "C-b" #'xwidget-webkit-scroll-down
      :n "C-f" #'xwidget-webkit-scroll-up
      :n "M-=" #'xwidget-webkit-zoom-in
      :n "M--" #'xwidget-webkit-zoom-out
      :n "k"   #'xwidget-webkit-scroll-down-line
      :n "j"   #'xwidget-webkit-scroll-up-line)

(provide 'my-mu4e-views-config)
