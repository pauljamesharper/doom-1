(setq user-full-name "Linus Sehn"
      user-mail-address "linus@sehn.tech")

(setq bookmark-default-file "~/.doom.d/bookmarks")

(setq doom-font (font-spec :family "Iosevka" :size 16)
      doom-variable-pitch-font (font-spec :family "Roboto")
      doom-unicode-font (font-spec :family "all-the-icons")
      doom-big-font (font-spec :family "Iosevka" :size 19))

(setq doom-theme 'doom-one
      doom-themes-enable-bold t
      dired-dwim-target t
      display-time-24hr-format t
      display-time-default-load-average nil)

(display-time-mode 1)

(after! writeroom-mode
  (setq writeroom-fullscreen-effect t))

(setq ispell-dictionary "en_GB")

(defun my/save-to-dict ()
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

(setq langtool-language-tool-jar "~/.langtool")

(defun my/save-to-dict ()
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

(defun my/switch-to-de-dict ()
  (interactive)
  (ispell-change-dictionary "de_DE")
  (flyspell-buffer))

(defun my/switch-to-en-dict ()
  (interactive)
  (ispell-change-dictionary "en_GB")
  (flyspell-buffer))

(map! :leader
      (:prefix-map ("d" . "dict")
        :desc "Add to dictionary"      "a"     #'my/save-to-dict
        :desc "Change to german"       "g"     #'my/switch-to-de-dict
        :desc "Change to english"      "e"     #'my/switch-to-en-dict))

(after! mu4e
  (setq mu4e-root-maildir "~/.mail/")
  (set-email-account! "sehn.tech"
                      '((mu4e-sent-folder       . "/mailbox/Sent")
                        (mu4e-drafts-folder     . "/mailbox/Drafts")
                        (mu4e-trash-folder      . "/mailbox/Trash")
                        (mu4e-refile-folder     . "/mailbox/Archive/2020")
                        (mu4e-compose-signature . "---\nLinus Sehn\nGraduate Student | International Relations and Computer Science\nFU Berlin, HU Berlin, Uni Potsdam\nlinus@sehn.tech | https://sehn.tech")
                        (smtpmail-smtp-user     . "linus@sehn.tech")
                        (user-mail-address      . "linus@sehn.tech")
                        (user-full-name         . "Linus Sehn"))
                      t)

  (setq mu4e-compose-complete-addresses 't
        mu4e-use-fancy-chars 'nil
        mu4e-sent-messages-behavior 'sent
        mu4e-update-interval 300
        smtpmail-debug-info 't
        smtpmail-smtp-user "linus@sehn.tech"
        smtpmail-smtp-server "smtp.mailbox.org"
        smtpmail-default-smtp-server "smtp.mailbox.org"
        smtpmail-stream-type 'ssl
        smtpmail-smtp-service 465))

(after! mu4e
  (setf (alist-get 'trash mu4e-marks)
        (list :char '("d" . "▼")
              :prompt "dtrash"
              :dyn-target (lambda (target msg)
                          (mu4e-get-trash-folder msg))
              :action (lambda (docid msg target)
                        ;; Here's the main difference to the regular trash mark,
                        ;; no +T before -N so the message is not marked as
                        ;; IMAP-deleted:
                        (mu4e~proc-move docid (mu4e~mark-check-target target) "-N")))))

(use-package! org-mu4e
  :after mu4e
  :config
  (setq org-mu4e-convert-to-html t
        mu4e-compose-mode-hook nil)

  ;; Only render to html once. If the first send fails for whatever reason,
  ;; org-mu4e would do so each time you try again.
  (add-hook! 'message-send-hook
    (setq-local org-mu4e-convert-to-html nil)))

(use-package! mu4e-alert
  :after mu4e
  :hook (after-init . mu4e-alert-enable-mode-line-display)
  :config (mu4e-alert-set-default-style 'libnotify))

(setq doom-modeline-mu4e t)
(mu4e-alert-enable-mode-line-display)

(map! :leader
      (:desc "e-mail" "e" #'mu4e))

(setq deft-directory "~/org"
      deft-recursive t
      deft-recursive-ignore-dir-regexp
        (concat "\\(?:"
                "\\."
                "\\|\\.\\."
                "\\\|.+stversions"
                "\\|code"
                "\\|auto"
                "\\|_minted.*"
                "\\)$"))

(defun my/kill-buffer-regexp (regexp)
  "Kill buffers matching REGEXP without asking for permission."
  (interactive "sKill buffers matching this regexp: ")
  (cl-letf (((symbol-function 'kill-buffer-ask) #'kill-buffer))
    (kill-matching-buffers regexp)))

(defun my/show-org-notes ()
  (interactive)
  (kill-buffer-regexp "*Deft*")
  (setq-default deft-directory "~/org")
  (deft))

(defun my/show-course-notes ()
  (interactive)
  (kill-buffer-regexp "*Deft*")
  (setq-default deft-directory "~/org/archive/courses")
  (deft))

(setq org-directory "~/org")

(use-package! org-download
  :after org
  :config
  (setq-default org-download-method 'directory
                org-download-image-dir "./images"
                org-download-heading-lvl nil))

(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROJ(p)" "|" "DONE(d)")
          (sequence "[ ](T)" "[-](P)" "[?](M)" "|" "[X](D)")
          (sequence "NEXT(n)" "WAIT(w)" "HOLD(h)" "|" "ABRT(c)")
          (sequence "TOREAD(r)" "|" "READ(R)"))))

(after! org
  (setq org-capture-templates
      '(("t" "TODO" entry
        (file+headline "~/org/actions.org" "Other")
        "* TODO [#A] %?\n%a\n")
        ("a" "APPOINTMENT" entry
        (file+headline "~/org/calendar.org" "Appointments")
        "* %?\n%(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
        ("d" "DISTRACTION" entry
        (file "~/org/distractions.org")
        "* %?\n%T\n** What was I doing\n** What was the trigger?"))
      ))

(setq org-clock-mode-line-total 'today)

(use-package! org-clock-budget
  :after org
  :config
  ;; set colors for different budget exhaustion states
  (setq org-clock-budget-ratio-faces '((1.0 hydra-face-red)
                                       (0.95 font-lock-type-face)
                                       (0.5 ivy-confirm-face)
                                       (0.0 font-lock-keyword-face))
  ;; set time-format to h:mm
        org-duration-format (quote h:mm))
  ;; make popup-buffer larger
  (set-popup-rule! "^\\*Org clock budget report" :size 0.35 :quit nil))

;; some custom functions for displaying
(defun show-yearly-clock-budget ()
  "Show yearly org-clock budget"
  (interactive)
  (setq org-clock-budget-intervals '(("BUDGET_YEAR" org-clock-budget-interval-this-year)))
  (org-clock-budget-report)
  )

(defun show-monthly-clock-budget ()
  "Show monthly org-clock budget"
  (interactive)
  (setq org-clock-budget-intervals '(("BUDGET_MONTH" org-clock-budget-interval-this-month)))
  (org-clock-budget-report)
  )

(defun show-weekly-clock-budget ()
  "Show yearly org-clock budget"
  (interactive)
  (setq org-clock-budget-intervals '(("BUDGET_WEEK" org-clock-budget-interval-this-week)))
  (org-clock-budget-report)
  )

(map! :map org-mode-map
      (:localleader
        :desc "Show yearly budget"     "y"     #'show-yearly-clock-budget
        :desc "Show monthly budget"    "m"     #'show-monthly-clock-budget
        :desc "Show weekly budget"     "w"     #'show-weekly-clock-budget
        ))

(use-package! org-caldav
  :after org
  :init
  (setq org-caldav-url "https://dav.mailbox.org/caldav"
        org-caldav-calendar-id "Y2FsOi8vMC80NQ"
        org-caldav-inbox "~/org/caldav.org"
        org-caldav-files '("~/org/calendar.org"
                           "~/org/actions.org"
                           "~/org/someday.org"))
  :config
  (setq org-icalendar-timezone "Europe/Berlin"
        org-icalendar-alarm-time 15
        org-icalendar-include-todo t
        org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due)
        org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo)
        org-icalendar-exclude-tags '("weekly" "daily" "monthly")
        org-caldav-exclude-tags '("weekly" "daily" "monthly")))

(after! org
  (setq org-agenda-files (list org-directory)
        org-habit-show-done-always-green 't)

(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-start-day "+0d")
  (setq org-agenda-span 'day)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-dim-blocked-tasks nil) ;; makes main tasks visible in agenda-view
  (setq org-agenda-files '("~/org/actions.org"
                           "~/org/cs.org"
                          "~/org/strategy.org"
                          "~/org/reading.org"
                          "~/org/watching.org"
                          "~/org/calendar.org"
                          "~/org/outreach.org"
                          "~/org/caldav.org"))
  (setq org-super-agenda-groups '((:name "Today"
                                         :time-grid t)
                                  (:name "Overdue"
                                         :deadline past)
                                  (:name "Due today"
                                          :deadline today)
                                  (:name "Due soon"
                                          :deadline future)
                                  (:name "Habits"
                                         :habit t)
                                  (:name "Reschedule or start"
                                         :scheduled past)
                                  (:name "Start today"
                                          :scheduled today)
                                  (:name "Start soon"
                                          :scheduled future)
                                  ))
    :config
    (org-super-agenda-mode)))

(after! org-roam
  (setq org-roam-directory "~/org/roam"))

(defun org-roam--title-to-slug (title)
    "Convert TITLE to a filename-suitable slug. Uses hyphens rather than underscores."
    (cl-flet* ((nonspacing-mark-p (char)
                                  (eq 'Mn (get-char-code-property char 'general-category)))
               (strip-nonspacing-marks (s)
                                       (apply #'string (seq-remove #'nonspacing-mark-p
                                                                   (ucs-normalize-NFD-string s))))
               (cl-replace (title pair)
                           (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-")  ;; convert anything not alphanumeric
                      ("--*" . "-")  ;; remove sequential underscores
                      ("^-" . "")  ;; remove starting underscore
                      ("-$" . "")))  ;; remove ending underscore
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
        (s-downcase slug))))

(after! org-roam
  (setq org-roam-capture-templates
               '(("d" "default"
                  plain (function org-roam-capture--get-point)
                  "%?\n\n\nbibliography:./biblio/library.bib"
                  :file-name "${slug}"
                  :head "#+HUGO_BASE_DIR:~/Projects/personal-website\n#+TITLE: ${title}\n"
                  :unnarrowed t)
                 ("r" "ref" plain (function org-roam-capture--get-point)
                  "%?\n\n\nbibliography:./biblio/library.bib"
                  :file-name "${slug}"
                  :head "#+HUGO_BASE_DIR:~/Projects/personal-website\n#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n"
                  :unnarrowed t))))

(defun my/org-roam--backlinks-list-with-content (file)
  (with-temp-buffer
    (if-let* ((backlinks (org-roam--get-backlinks file))
              (grouped-backlinks (--group-by (nth 0 it) backlinks)))
        (progn
          ;; no display of the number of backlinks
          ;; (insert (format "\n\n** %d Backlink(s)\n"
          ;;                 (length backlinks)))
          (dolist (group grouped-backlinks)
            (let ((file-from (car group))
                  (bls (cdr group)))
              (insert (format "** [[file:%s][%s]]\n"
                              file-from
                              (org-roam--get-title-or-slug file-from)))
              (dolist (backlink bls)
                (pcase-let ((`(,file-from _ ,props) backlink))
                  (insert (s-trim (s-replace "\n" " " (plist-get props :content))))
                  (insert "\n\n")))))))
    (buffer-string)))

  (defun my/org-export-preprocessor (backend)
    (let ((links (my/org-roam--backlinks-list-with-content (buffer-file-name))))
      (unless (string= links "")
        (save-excursion
          (goto-char (point-max))
          (insert (concat "\n* Backlinks\n") links)))))

  (add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor)

(use-package! org-ref
  :config
  (setq reftex-default-bibliography
        '("~/org/roam/biblio/library.bib"
          "~/org/roam/biblio/platform_state_surveillance.bib"
          "~/org/roam/biblio/stusti_predpol.bib")
        org-ref-default-bibliography
        '("~/org/roam/biblio/library.bib"
          "~/org/roam/biblio/platform_state_surveillance.bib"
          "~/org/roam/biblio/stusti_predpol.bib")
        org-ref-bibliography-notes "~/org/roam/"
        org-ref-pdf-directory "~/Library"
        bibtex-completion-library-path "~/Library/"
        bibtex-completion-notes-path "~/org/roam/"
        bibtex-completion-pdf-field "file"
        ))

(after! org
  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")))

(after! ox-hugo
  (setq org-hugo-default-section-directory "roam"))

(use-package! org-ref-ox-hugo
  :after org org-ref ox-hugo
  :config
  (add-to-list 'org-ref-formatted-citation-formats
               '("md"
                 ("article" . "${author}, *${title}*, ${journal}, *${volume}(${number})*, ${pages} (${year}). ${doi}")
                 ("inproceedings" . "${author}, *${title}*, In ${editor}, ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("book" . "${author}, *${title}* (${year}), ${address}: ${publisher}.")
                 ("phdthesis" . "${author}, *${title}* (Doctoral dissertation) (${year}). ${school}, ${address}.")
                 ("inbook" . "${author}, *${title}*, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("incollection" . "${author}, *${title}*, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("proceedings" . "${editor} (Eds.), _${booktitle}_ (${year}). ${address}: ${publisher}.")
                 ("unpublished" . "${author}, *${title}* (${year}). Unpublished manuscript.")
                 ("misc" . "${author} (${year}). *${title}*. Retrieved from [${howpublished}](${howpublished}). ${note}.")
                 (nil . "${author}, *${title}* (${year})."))))

(map! :map org-mode-map
      (:localleader
        :desc "Show yearly budget"     "y"     #'show-yearly-clock-budget
        :desc "Show monthly budget"    "m"     #'show-monthly-clock-budget
        :desc "Show weekly budget"     "w"     #'show-weekly-clock-budget
        ))

(setq projectile-project-search-path '("~/Projects" "/home/lino"))

(setenv "WORKON_HOME" "/home/lino/anaconda3/envs")
(pyvenv-mode 1)

(use-package! mathpix
  :custom ((mathpix-app-id "mathpix_sehn_tech_b5ad38")
           (mathpix-app-key "f965173bcdbfec889c20")))

(map! :leader
      (:prefix-map ("i" . "insert")
        :desc "Insert math from screen" "m" #'mathpix-screenshot))

(toggle-frame-fullscreen)
