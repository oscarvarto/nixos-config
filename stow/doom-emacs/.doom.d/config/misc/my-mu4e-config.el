;;; -*- lexical-binding: t; no-byte-compile: t; -*-

;; Load path utilities
(require 'my-paths)

(after! mu4e
  ;; Set the base maildir path
  (setq mu4e-maildir "~/Maildir")
  (setq mu4e-index-update-error-warning nil)

  (require 'org-msg)

  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
      org-msg-startup "hidestars indent inlineimages"
      org-msg-greeting-fmt "\nHi%s,\n\n"
      ;; Include both email addresses in recipient names
      org-msg-recipient-names '(("oscarvarto@fastmail.com" . "Oscar")
                                ("contact@oscarvarto.mx"   . "Oscar"))
      org-msg-greeting-name-limit 3
      org-msg-default-alternatives '((new           . (text html))
                                     (reply-to-html . (text html))
                                     (reply-to-text . (text)))
      org-msg-convert-citation t
      org-msg-signature "

---
Regards,
#+begin_signature
*Oscar Vargas Torres*
#+end_signature")

  ;; Function to detect if current client is GUI or terminal
  (defun mu4e-current-client-is-gui-p ()
    "Return t if current client supports GUI features like webkit, nil otherwise."
    (and (display-graphic-p)
         ;; Additional checks for GUI capabilities
         (not (getenv "TERM"))  ;; TERM is set in terminal emacsclient
         ;; Check if we can create GUI widgets
         (condition-case nil
             (progn
               ;; Try to access window system
               (when (and (boundp 'window-system) window-system)
                 ;; Check if we have actual GUI display
                 (display-pixel-width))
               t)
           (error nil))))
  ;; Dynamic configuration function that checks client type each time
  (defun mu4e-configure-for-client-type ()
    "Configure mu4e based on current client type (GUI vs terminal)."
    (if (mu4e-current-client-is-gui-p)
        ;; GUI mode: use mu4e-views for HTML rendering
        (progn
          (message "Configuring mu4e for GUI client")
          ;; Load mu4e-views configuration
          (load (expand-file-name "config/misc/my-mu4e-views-config" doom-user-dir))
          ;; Enable HTML viewing
          (setq mu4e-view-prefer-html t)
          (setq mu4e-view-show-images t)
          ;; Allow GUI notifications
          (setq mu4e-notification-support t))
      ;; Terminal mode: use textual view
      (progn
        (message "Configuring mu4e for terminal client")
        ;; Prefer text over HTML in terminal
        (setq mu4e-view-prefer-html nil)
        ;; Use built-in text renderer
        (setq mu4e-view-use-gnus t)
        ;; Don't show images in terminal
        (setq mu4e-view-show-images nil)
        ;; Use text-based attachment handling
        (setq mu4e-view-attachment-actions
              '(("view in browser" . mu4e-action-view-in-browser)
                ("save" . mu4e-view-save-attachment-single)
                ("open" . mu4e-view-open-attachment)))
        ;; Configure headers display for terminal
        (setq mu4e-headers-visible-lines 20)
        (setq mu4e-headers-visible-columns 120)
        ;; Disable automatic HTML conversion
        (setq mu4e-view-auto-mark-as-read nil)
        ;; Configure text rendering for better terminal display
        (setq mu4e-view-fill-headers t)
        (setq mu4e-view-wrap-lines t)
        ;; Set reasonable column width for terminal
        (setq fill-column 80)
        ;; Better handling of quoted text in terminal
        (setq mu4e-view-date-format "%Y-%m-%d %H:%M")
        ;; Improve readability of headers in terminal
        (setq mu4e-view-fields '(:from :to :cc :subject :date :mailing-list))
        ;; Disable GUI notifications in terminal mode
        (setq mu4e-notification-support nil)
        (setq mu4e-alert-interesting-mail-query nil)
        ;; Use simple message-based notifications instead of GUI
        (setq mu4e-index-update-in-background nil)
        ;; Override notification function to avoid GUI calls
        (defun mu4e-terminal-notification-function (fringe msg)
          "Simple terminal notification function that doesn't use GUI."
          (when msg
            (message "Mail: %s" (mu4e-message-field msg :subject))))
        (setq mu4e-notification-function #'mu4e-terminal-notification-function)
        ;; Terminal-specific keybindings for better text navigation
        (map! :map mu4e-view-mode-map
              :n "q" #'mu4e-headers-query-prev
              :n "TAB" #'mu4e-view-headers-next
              :n "<backtab>" #'mu4e-view-headers-prev
              :n "RET" #'mu4e-scroll-up
              :n "<backspace>" #'mu4e-scroll-down
              :n "a" #'mu4e-view-action))))

  ;; Configure for the current client type when mu4e is loaded
  (mu4e-configure-for-client-type)

  ;; Also reconfigure when switching contexts or frames
  (add-hook 'mu4e-main-mode-hook #'mu4e-configure-for-client-type)
  (add-hook 'mu4e-headers-mode-hook #'mu4e-configure-for-client-type)

  ;; Interactive functions to manually override view mode for testing
  (defun mu4e-force-gui-mode ()
    "Force mu4e to use GUI mode settings (for testing)."
    (interactive)
    (setq mu4e-view-prefer-html t)
    (setq mu4e-view-show-images t)
    (setq mu4e-notification-support t)
    (when (featurep 'mu4e-views)
      (mu4e-views-mu4e-use-view-msg-method "html-nonblock"))
    (message "Forced mu4e to GUI mode"))

  (defun mu4e-force-terminal-mode ()
    "Force mu4e to use terminal mode settings (for testing)."
    (interactive)
    (setq mu4e-view-prefer-html nil)
    (setq mu4e-view-use-gnus t)
    (setq mu4e-view-show-images nil)
    (setq mu4e-notification-support nil)
    (when (featurep 'mu4e-views)
      (mu4e-views-mu4e-use-view-msg-method "text"))
    (message "Forced mu4e to terminal mode"))

  (defun mu4e-auto-detect-client-mode ()
    "Automatically detect and configure mu4e for current client type."
    (interactive)
    (mu4e-configure-for-client-type)
    (when (featurep 'mu4e-views)
      (mu4e-views-configure-for-current-client))
    (message "Auto-detected and configured mu4e for current client type"))

  ;; Define contexts using the standard mu4e approach instead of mu4easy-contexts
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "fastmail"
          :enter-func (lambda () (mu4e-message "Entering fastmail context"))
          :leave-func (lambda () (mu4e-message "Leaving fastmail context"))
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches
                           msg '(:from :to :cc :bcc) '("contact@oscarvarto.mx" "oscarvarto@fastmail.com"))))
          :vars '(;; (user-mail-address . "oscarvarto@fastmail.com")
                  (user-mail-address . "contact@oscarvarto.mx")
                  (user-full-name . "Oscar Vargas Torres")
                  (mu4e-sent-folder . "/oscarvarto-fastmail/Sent")
                  (mu4e-drafts-folder . "/oscarvarto-fastmail/Drafts")
                  (mu4e-trash-folder . "/oscarvarto-fastmail/Trash")
                  (mu4e-refile-folder . "/oscarvarto-fastmail/Archive")
                  ;; (mu4e-compose-signature . (concat "Regards,\n" "Oscar Vargas Torres\n"))
                  (mu4e-compose-format-flowed . t)
                  (smtpmail-queue-dir . "/Users/oscarvarto/Maildir/oscarvarto-fastmail/queue/cur")
                  (message-send-mail-function . smtpmail-send-it)
                  (smtpmail-smtp-user . "oscarvarto@fastmail.com")
                  (smtpmail-smtp-server . "smtp.fastmail.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type . starttls)
                  (smtpmail-debug-info . t)
                  (smtpmail-debug-verbose . t)
                  (mu4e-maildir-shortcuts . (("/oscarvarto-fastmail/INBOX"    . ?i)
                                             ("/oscarvarto-fastmail/Sent"     . ?s)
                                             ("/oscarvarto-fastmail/Trash"    . ?t)
                                             ("/oscarvarto-fastmail/Archive"  . ?a)
                                             ("/oscarvarto-fastmail/Drafts"   . ?d)))))))

  (setq mail-user-agent 'mu4e-user-agent)
  ;; Set context policy to match email.org
  (setq mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'always-ask)

  ;; 1Password integration using 1passel
  (require '1passel)

  ;; Initialize 1Password session if needed
  (defun mu4e-ensure-1password-session ()
    "Ensure 1Password session is active."
    (when (or (string= 1passel-session "session-token")
              (string-empty-p 1passel-session))
      (1passel-login)
      (message "1Password session initialized for mu4e")))

  ;; Function to get password from 1Password for a specific service
  (defun mu4e-1password-get-password (service-name)
    "Get password from 1Password for SERVICE-NAME."
    (mu4e-ensure-1password-session)
    (let* ((json (json-or-login))
           (filtered-ids (1passel-extract-loop json))
           (item-id (cdr (assoc service-name filtered-ids))))
      (if item-id
          (string-trim
           (shell-command-to-string
            (format "op item get %s --session=%s --fields password" item-id 1passel-session)))
        (error "Could not find 1Password item for %s" service-name))))

  ;; Custom auth-source backend for 1Password
  (defun mu4e-1password-auth-source-search (&rest spec)
    "Custom auth-source function that retrieves credentials from 1Password."
    (let* ((host (plist-get spec :host))
           (user (plist-get spec :user))
           (port (plist-get spec :port))
           (service-name (cond
                          ((and (equal host "smtp.fastmail.com") (equal user "oscarvarto@fastmail.com"))
                           "Fastmail SMTP")
                          ((and (equal host "imap.fastmail.com") (equal user "oscarvarto@fastmail.com"))
                           "Fastmail IMAP")
                          (t nil))))
      (when service-name
        (condition-case err
            (list (list :host host
                       :user user
                       :port port
                       :secret (lambda () (mu4e-1password-get-password service-name))))
          (error
           (message "Failed to get password from 1Password for %s: %s" service-name (error-message-string err))
           nil)))))

  ;; Set up auth-source to use our custom 1Password backend
  (setq auth-sources '(mu4e-1password-auth-source-search))

  ;; Initialize 1Password session when mu4e starts
  (add-hook 'mu4e-main-mode-hook #'mu4e-ensure-1password-session)

  ;; Mail sending configuration
  (setq send-mail-function #'smtpmail-send-it
        message-send-mail-function #'smtpmail-send-it)

  ;; Other global mu4e settings
  (setq mu4e-change-filenames-when-moving t
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-compose-dont-reply-to-self t
        mu4e-confirm-quit nil
        mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval 300)

  ;; Customize mu4e faces to use theme default background
  (custom-set-faces!
   '(mu4e-view-body-face :background nil :inherit default)
   '(mu4e-view-header-key-face :background nil)
   '(mu4e-view-header-value-face :background nil)
   '(mu4e-view-header-title-face :background nil)
   '(mu4e-view-special-header-value-face :background nil)
   '(mu4e-view-link-face :background nil)
   '(mu4e-view-attach-number-face :background nil)
   '(mu4e-view-contact-face :background nil)
   '(mu4e-cited-1-face :background nil)
   '(mu4e-cited-2-face :background nil)
   '(mu4e-cited-3-face :background nil)
   '(mu4e-cited-4-face :background nil)
   '(mu4e-cited-5-face :background nil)
   '(mu4e-cited-6-face :background nil)
   '(mu4e-cited-7-face :background nil))

  ;; Force gnus article mode (used by mu4e for text rendering) to use default background
  (custom-set-faces!
   '(gnus-article :background nil :inherit default))

  ;; Override HTML email background colors
  (setq mu4e-html2text-command 'mu4e-shr2text)

  ;; Configure shr (HTML renderer) to use theme colors
  (after! shr
    (setq shr-use-colors nil)  ;; Disable HTML colors, use theme colors instead
    (setq shr-color-visible-luminance-min 60)  ;; Improve contrast
    (custom-set-faces!
     '(shr-text :background nil :inherit default)))

  ;; Additional configuration to override email content background
  (defun mu4e-override-email-background ()
    "Remove background colors from email content."
    (when (eq major-mode 'mu4e-view-mode)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "style=\"[^\"]*background[^\"]*\"" nil t)
          (replace-match "")))))

  (add-hook 'mu4e-view-mode-hook #'mu4e-override-email-background))

  ;; Debug auth issues if needed
  ;; (setq auth-source-debug t)


(provide 'my-mu4e-config)
