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

  (load (expand-file-name "config/misc/my-mu4e-views-config" doom-user-dir))
  
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

  ;; Configure auth-source to look in both .authinfo.gpg and .mbsyncpass files
  (require 'auth-source)

  (setq auth-sources
        (list (my/expand-home-path ".authinfo.gpg")))

  ;; Ensure epg knows to use pinentry for GPG password prompts
  (setq epg-pinentry-mode 'loopback)

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
        mu4e-update-interval 300))

  ;; Debug auth issues if needed
  ;; (setq auth-source-debug t)


(provide 'my-mu4e-config)
