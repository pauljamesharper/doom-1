(setq user-full-name "Linus Sehn"
      user-mail-address "linus@sehn.tech"
      projectile-project-search-path '("~/Projects" "/home/lino")
      bookmark-default-file "~/.doom.d/bookmarks")

(setq doom-font (font-spec :family "Iosevka" :size 18)
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

(set-popup-rules!
 '(("^\*helm"
    :size 0.45 :select t :modeline t :quit t :ttl t)))

(add-hook! 'text-mode-hook auto-fill-mode)

(toggle-frame-fullscreen)

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

(map! :leader
      (:desc "e-mail" "e" #'mu4e))

(setq org-directory "~/org")

(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROJ(p)" "|" "DONE(d)")
          (sequence "[ ](T)" "[-](P)" "[?](M)" "|" "[X](D)")
          (sequence "NEXT(n)" "WAIT(w)" "HOLD(h)" "|" "ABRT(c)")
          (sequence "TOREAD(r)" "|" "READ(R)"))))

(use-package! org-download
  :after org
  :config
  (setq-default org-download-method 'directory
                org-download-image-dir "./images"
                org-download-heading-lvl nil))

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

(setq centaur-lsp 'lsp-mode)
(cl-defmacro lsp-org-babel-enable (lang)
    "Support LANG in org source code block."
    (cl-check-type lang stringp)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
      `(progn
         (defun ,intern-pre (info)
           (let ((filename (or (->> info caddr (alist-get :file))
                               buffer-file-name)))
             (unless filename
               (user-error "LSP:: specify `:file' property to enable."))

             (setq buffer-file-name filename)
             (pcase centaur-lsp
               ('eglot
                (and (fboundp 'eglot) (eglot)))
               ('lsp-mode
                (and (fboundp 'lsp-deferred)
                     ;; `lsp-auto-guess-root' MUST be non-nil.
                     (setq lsp-buffer-uri (lsp--path-to-uri filename))
                     (lsp-deferred))))))
         (put ',intern-pre 'function-documentation
              (format "Enable `%s' in the buffer of org source block (%s)."
                      centaur-lsp (upcase ,lang)))

         (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Prepare local buffer environment for org source block (%s)."
                          (upcase ,lang))))))))

(defun lsp-org()
    (interactive)
    (defvar org-babel-lang-list
        '("python" "ipython"))
    (dolist (lang org-babel-lang-list)
      (eval `(lsp-org-babel-enable ,lang))))

(add-hook! 'org-src-mode-hook 'lsp-org)
(add-hook! 'org-src-mode-hook 'lsp)

(after! org-roam
  (setq org-roam-directory "~/org/roam"))

(use-package! mathpix
  :custom ((mathpix-app-id "mathpix_sehn_tech_b5ad38")
           (mathpix-app-key "f965173bcdbfec889c20")))

(map! :leader
      (:prefix-map ("i" . "insert")
        :desc "Insert math from screen" "m" #'mathpix-screenshot))

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
                  "%?\n\n\nbibliography:biblio/library.bib"
                  :file-name "${slug}"
                  :head "#+TITLE: ${title}\n#+HUGO_BASE_DIR:~/Projects/personal-website\n\n*Links* ::  "
                  :unnarrowed t))))

(after! (org org-roam)
    (defun my/org-roam--backlinks-list (file)
      (if (org-roam--org-roam-file-p file)
          (--reduce-from
           (concat acc (format "- *[[file:%s][%s]]*\n"
                               (file-relative-name (car it) org-roam-directory)
                               (org-roam--get-title-or-slug (car it))))
           "" (org-roam-db-query [:select [from]
                                  :from links
                                  :where (= to $s1)
                                  :and from :not :like $s2] file "%private%"))
        ""))
    (defun my/org-export-preprocessor (_backend)
      (let ((links (my/org-roam--backlinks-list (buffer-file-name))))
        (unless (string= links "")
          (save-excursion
            (goto-char (point-max))
            (insert (concat "\n* Backlinks\n" links))))))
    (add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor))

(setq! +biblio-pdf-library-dir "~/Library/"
       +biblio-default-bibliography-files
       '("~/org/roam/biblio/library.bib"
         "~/org/roam/biblio/stusti_predpol.bib"
         "~/org/roam/biblio/platform_state_surveillance.bib")
       +biblio-notes-path "~/org/roam/")

(after! org-roam-bibtex
    (setq orb-preformat-keywords
          '("=key=" "title" "url" "file" "author-or-editor" "keywords" "year"))
    (setq orb-templates
          '(("r" "ref" plain (function org-roam-capture--get-point)
             ""
             :file-name "${slug}"
             :head "#+TITLE: Notes on: ${title} (${author-or-editor}, ${year})\n#+HUGO_BASE_DIR:~/Projects/personal-website\n#+ROAM_KEY: ${ref}

*Links* ::
\n* Summary\n#+begin_src toml :front_matter_extra t
subtitle = \"\"
summary = \"\"
tags = [\"reading note\", \"\"]\n#+end_src
\n* Main points\n:PROPERTIES:\n:Custom_ID: ${=key=}\n:NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n:NOTER_PAGE:\n:END:\n\n"
             :unnarrowed t))))

(use-package! company-bibtex
  :after org
  :config
  (set-company-backend! 'org-mode 'company-bibtex)
  (setq company-bibtex-bibliography
    '("/home/lino/org/roam/biblio/library.bib"
        "/home/lino/org/roam/biblio/stusti_predpol.bib")
    company-bibtex-org-citation-regex "cite:"))

(map! :map org-mode-map
      ("M-i" #'org-ref-helm-insert-cite-link)
      ("M-e" #'org-ref-update-pre-post-text)
      ("M-p" #'my/org-ref-open-pdf-at-point)
      ("M-n" #'org-roam-insert)
      (:leader
        (:prefix "i"
          :desc "Cite source" "c" #'org-ref-helm-insert-cite-link
          )))

(use-package! org-ref
  :when (featurep! :lang org)
  :after (org bibtex-completion)
  :preface
  (setq org-ref-completion-library #'org-ref-helm-bibtex))
  :config
  ;; Although the name is helm-bibtex, it is actually a bibtex-completion function
  ;; it is the legacy naming of the project helm-bibtex that causes confusion.
  (setq org-ref-open-pdf-function 'org-ref-get-pdf-filename-helm-bibtex)
  ;; org-roam-bibtex will define handlers for note taking so not needed to use the
  ;; ones set for bibtex-completion
  (unless (featurep! :lang org +roam)
    ;; Allow org-ref to use the same template mechanism as {helm,ivy}-bibtex for
    ;; multiple files if the user has chosen to spread their notes.
    (setq org-ref-notes-function (if (directory-name-p org-ref-notes-directory)
                                     #'org-ref-notes-function-many-files
                                   #'org-ref-notes-function-one-file
                                   )))

(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results)))
    (funcall bibtex-completion-pdf-open-function (car (bibtex-completion-find-pdf key)))))
(setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)

(after! org
  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")))

(after! ox-hugo
  (setq org-hugo-default-section-directory "roam"))

(use-package! org-ref-ox-hugo
  :after org org-ref ox-hugo
  :config
  (add-to-list 'org-ref-formatted-citation-formats
               '("md"
                 ("article" . "${author}. (${year}). *${title}*, ${journal}, *${volume}(${number})*, ${pages} ${doi}.")
                 ("inproceedings" . "${author}. *${title}*, In ${editor}, ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("book" . "${author-or-editor}. (${year}). *${title}*.")
                 ("phdthesis" . "${author}. *${title}* (Doctoral dissertation) (${year}). ${school}, ${address}.")
                 ("inbook" . "${author}. *${title}*, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("incollection" . "${author}. *${title}*, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("proceedings" . "${editor} (Eds.), _${booktitle}_ (${year}). ${address}: ${publisher}.")
                 ("unpublished" . "${author}. *${title}* (${year}). Unpublished manuscript.")
                 ("misc" . "${author} (${year}). *${title}*. Retrieved from [${url}](${url}). ${note}.")
                 (nil . "${author}. (${year}). *${title}* "))))

(defun my/org-ref-get-md-bibliography (&optional sort)
  "Create an md bibliography when there are keys.
if SORT is non-nil the bibliography is sorted alphabetically by key."
  (let ((keys (org-ref-get-bibtex-keys sort)))
    (when keys
      (concat
       "\n"
       (mapconcat (lambda (x) (org-ref-get-bibtex-entry-md x)) keys "\n\n")
       "\n"))))

(defun org-ref-bibliography-format (keyword desc format)
  "Formatting function for bibliography links."
  (cond
   ((eq format 'org) (org-ref-get-org-bibliography))
   ((eq format 'ascii) (org-ref-get-ascii-bibliography))
   ((eq format 'md) (my/org-ref-get-md-bibliography))
   ((eq format 'odt) (org-ref-get-odt-bibliography))
   ((eq format 'html) (org-ref-get-html-bibliography))
   ((eq format 'latex)
    ;; write out the latex bibliography command
    (format "\\bibliography{%s}"
	    (replace-regexp-in-string
	     "\\.bib" ""
	     (mapconcat
	      'identity
	      (mapcar 'file-relative-name
		      (split-string keyword ","))
	      ","))))))

(map! :map org-mode-map
      (:localleader
        (:prefix ("a" . "attachments")
          "c" #'org-download-screenshot
          "y" #'org-download-yank
          )))

(setq deft-directory "~/org/roam"
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

(map! :map pdf-view-mode-map
      "C-c i" 'org-noter-insert-note)
