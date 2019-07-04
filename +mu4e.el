;;; ~/.doom.d/+mu4e.el -*- lexical-binding: t; -*-

;; E-mail setup
(after! mu4e
  (set-email-account! "sehn.tech"
    '((mu4e-sent-folder       . "/mailbox/Sent")
      (mu4e-drafts-folder     . "/mailbox/Drafts")
      (mu4e-trash-folder      . "/mailbox/Trash")
      (mu4e-refile-folder     . "/mailbox/Archive/2019")
      (mu4e-drafts-folder     . "/mailbox/Drafts")
      (smtpmail-smtp-user     . "linus@sehn.tech")
      (user-mail-address      . "linus@sehn.tech")
      (user-full-name         . "Linus Sehn")
      (mu4e-compose-signature . "---\nLinus Sehn\nGraduate Student | International Relations and Computer Science\nFU Berlin, HU Berlin, Uni Potsdam\n[[mailto:mail@sehn.tech][mail@sehn.tech]]"))
    t)

  (setq mu4e-compose-complete-addresses 't
        mu4e-sent-messages-behavior 'sent
        smtpmail-debug-info   't
        smtpmail-smtp-user    "linus@sehn.tech"
        smtpmail-smtp-server  "smtp.mailbox.org"
        smtpmail-default-smtp-server "smtp.mailbox.org"
        smtpmail-stream-type  'ssl
        smtpmail-smtp-service 465))
