;;; ~/.doom.d/+mu4e.el -*- lexical-binding: t; -*-

;; E-mail setup
(after! mu4e
  (set-email-account! "sehn.tech"
    '((mu4e-sent-folder       . "/mailbox/Sent")
      (mu4e-drafts-folder     . "/mailbox/Drafts")
      (mu4e-trash-folder      . "/mailbox/Trash")
      (mu4e-refile-folder     . "/mailbox/Archive/2019")
      (mu4e-drafts-folder     . "/mailbox/Drafts")
      (mu4e-compose-signature . "---\nLinus Sehn\nGraduate Student | International Relations and Computer Science\nFU Berlin, HU Berlin, Uni Potsdam\n[[mailto:mail@sehn.tech][mail@sehn.tech]]")
      (smtpmail-smtp-user     . "linus@sehn.tech")
      (user-mail-address      . "linus@sehn.tech")
      (user-full-name         . "Linus Sehn"))
    t)

  (setq mu4e-compose-complete-addresses 't
        mu4e-sent-messages-behavior 'sent
        mu4e-update-interval 300
        smtpmail-debug-info 't
        smtpmail-smtp-user "linus@sehn.tech"
        smtpmail-smtp-server "smtp.mailbox.org"
        smtpmail-default-smtp-server "smtp.mailbox.org"
        smtpmail-stream-type 'ssl
        smtpmail-smtp-service 465)

  ;; Don't set -T flag for normal delete operation
  (setf (alist-get 'trash mu4e-marks)
      (list :char '("d" . "▼")
            :prompt "dtrash"
            :dyn-target (lambda (target msg)
                          (mu4e-get-trash-folder msg))
            :action (lambda (docid msg target)
                      ;; Here's the main difference to the regular trash mark,
                      ;; no +T before -N so the message is not marked as
                      ;; IMAP-deleted:
                      (mu4e~proc-move docid (mu4e~mark-check-target target) "-N")))
      )
  )

(def-package! org-mu4e
  :after mu4e
  :config
  (setq org-mu4e-link-query-in-headers-mode nil
        org-mu4e-convert-to-html t
        mu4e-compose-mode-hook nil)

  ;; Only render to html once. If the first send fails for whatever reason,
  ;; org-mu4e would do so each time you try again.
  (add-hook! 'message-send-hook
    (setq-local org-mu4e-convert-to-html nil)))


;; mu4e-alert
(def-package! mu4e-alert
  :after mu4e
  :hook ((after-init . mu4e-alert-enable-mode-line-display)
         (after-init . mu4e-alert-enable-notifications))
  :config (mu4e-alert-set-default-style 'libnotify))
