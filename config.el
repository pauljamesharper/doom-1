;; -*- lexical-binding: t -*-

(setq user-full-name "Linus Sehn"
      user-mail-address "linus@sehn.tech"
      projectile-project-search-path '("~/Projects" "/home/lino")
      bookmark-default-file "~/.doom.d/bookmarks")

(setq doom-font (font-spec :family "Iosevka" :size 16)
      doom-variable-pitch-font (font-spec :family "Rubik")
      doom-unicode-font (font-spec :family "all-the-icons")
      doom-big-font (font-spec :family "Iosevka" :size 20))

(setq doom-theme 'doom-one
      doom-themes-enable-bold t
      dired-dwim-target t
      display-time-24hr-format t
      display-time-default-load-average nil)

(display-time-mode 1)

(defun my/toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (if (eq
         (if (numberp alpha)
             alpha
           (cdr alpha)) ; may also be nil
         100)
        (set-frame-parameter nil 'alpha '(90 . 75))
      (set-frame-parameter nil 'alpha '(100 . 100)))))

(set-popup-rules!
  '(("^\*helm"
     :size 0.35 :select t :modeline n :quit t)))

(set-popup-rule! "eldoc" :side 'right :size 85)
(set-popup-rule! "helpful" :side 'right :size 85)

(add-hook! 'text-mode-hook auto-fill-mode)

(after! company-box
  (setq company-box-max-candidates 10))

(use-package! dired-x
  :unless (featurep! +ranger)
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store\\'"
                "\\|^.project\\(?:ile\\)?\\'"
                "\\|^.\\(svn\\|git\\)\\'"
                "\\|^.ccls-cache\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"
                ))
  ;; Disable the prompt about whether I want to kill the Dired buffer for a
  ;; deleted directory. Of course I do!
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  ;; Let OS decide how to open certain files
  (when-let (cmd (cond (IS-MAC "open")
                       (IS-LINUX "xdg-open")
                       (IS-WINDOWS "start")))
    (setq dired-guess-shell-alist-user
          `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.odt\\'" "libreoffice"))))
  (map! :map dired-mode-map
        :localleader
        "h" #'dired-omit-mode))

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

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
        
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

(use-package! mu4e
  :config
  (remove-hook 'mu4e-main-mode-hook 'evil-collection-mu4e-update-main-view))

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

(setq org-directory "~/Exocortex")

(after! org-roam
  (setq org-roam-directory "~/Exocortex"
        org-roam-db-location "~/Exocortex/.exocortex.db"
        org-roam-file-exclude-regexp ".*archive.org"))

(after! org
  (use-package! org-super-agenda
    :after org-agenda
    :init
    (setq org-habit-show-done-always-green 't
          org-agenda-prefix-format
          '((agenda . " %?-12t% s")
            (todo . " %i %-12:c")
            (tags . " %i %-12:c")
            (search . " %i %-12:c")))
    (setq org-agenda-window-setup 'current-window)
    (setq org-agenda-start-day "+0d")
    (setq org-agenda-span 'day)
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-agenda-start-on-weekday nil)
    (setq org-agenda-dim-blocked-tasks nil) ;; makes main tasks visible in agenda-view
    (setq org-agenda-files '("~/Exocortex/org/actions.org"
                             "~/Exocortex/org/actions-compsci.org"
                             "~/Exocortex/org/calendar.org"
                             "~/Exocortex/org/caldav.org"))
    (setq org-super-agenda-groups '((:name "Today"
                                     :time-grid t)
                                    (:name "Due today"
                                     :deadline today)
                                    (:name "Overdue"
                                     :deadline past)
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

(after! org
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; This task is paused/on hold because of me
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")) ; Task was completed
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("IDEA" . +org-todo-onhold)
          ("PROJ" . +org-todo-project))))

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
        org-duration-format (quote h:mm)
        org-clock-budget-default-sort-column '("BUDGET_WEEK" budget desc))
  ;; make popup-buffer larger
  (set-popup-rule! "^\\*Org clock budget report" :size 0.2 :quit nil))

;; some custom functions for displaying
(defun show-yearly-clock-budget ()
  "Show yearly org-clock budget"
  (interactive)
  (setq org-clock-budget-intervals '(("BUDGET_YEAR" org-clock-budget-interval-this-year)))
  (org-clock-budget-report))

(defun show-monthly-clock-budget ()
  "Show monthly org-clock budget"
  (interactive)
  (setq org-clock-budget-intervals '(("BUDGET_MONTH" org-clock-budget-interval-this-month)))
  (org-clock-budget-report))

(defun show-weekly-clock-budget ()
  "Show yearly org-clock budget"
  (interactive)
  (setq org-clock-budget-intervals '(("BUDGET_WEEK" org-clock-budget-interval-this-week)))
  (org-clock-budget-report))

(map! :map org-mode-map
      (:localleader
       :desc "Show weekly budget"     "w"     #'show-weekly-clock-budget
       ))

(after! org
  (setq org-capture-templates
        '(("t" "TODO" entry
           (file+headline "~/org/actions.org" "Other")
           "* TODO [#A] %?\n%a\n")
          ("a" "APPOINTMENT" entry
           (file+headline "~/org/calendar.org" "2020_Q4")
           "* %?\n%(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n"))))

(add-hook! 'org-mode-hook 'anki-editor-mode)
(after! org
  (setq anki-editor-ignored-org-tags '("noexport")))

(defun my/search-exocortex ()
  "Perform a text search on ~/Exocortex."
  (interactive)
  (require 'org)
  (let ((default-directory "~/Exocortex"))
    (+default/search-project-for-symbol-at-point "")))

(defun my/search-public ()
  "Perform a text search on ~/Projects/exocortex-public."
  (interactive)
  (let ((default-directory "~/Projects/exocortex-public"))
    (+default/search-cwd "")))

(setq! +biblio-pdf-library-dir "~/Exocortex/pdfs/"
       +biblio-default-bibliography-files "~/Exocortex/bib/library.bib"
       +biblio-notes-path "~/Exocortex/refs/")

(after! org-roam
  (setq org-roam-capture-templates
        '(("z" "zettel"
           plain (function org-roam-capture--get-point)

           :file-name "zettel/${slug}"
           :head "#+title: ${title}\n#+hugo_base_dir:~/Projects/personal-website

Links ::
\n#+begin_src toml :front_matter_extra t
subtitle = \"\"
summary = \"\"
tags = [\"concept\", \"\"]
share = true
profile = true \n#+end_src

%?

bibliography:../bib/library.bib"
           :unnarrowed t))))

(after! org-roam
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "refs/${slug}"
           :head "#+title: Notes on: ${title}
#+hugo_base_dir:~/Projects/personal-website
#+hugo_section:refs
#+roam_key: ${ref}

Source :: ${ref}\n
Links ::
\n#+begin_src toml :front_matter_extra t
subtitle = \"\"
summary = \"\"
tags = [\"\"]
share = true
profile = true\n#+end_src

%?

# Don't forget to snapshot item in Zotero if important
bibliography:../bib/library.bib"
           :unnarrowed t))))

(use-package! org-roam-protocol
  :after org-protocol)

(after! org-roam-bibtex
  (setq orb-preformat-keywords
        '("=key=" "title" "url" "file" "author-or-editor" "keywords" "year"))
  (setq orb-templates
        '(("c" "cite-ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "refs/${=key=}"
           :head "#+title: Notes on: ${title} (${author-or-editor}, ${year})\n#+hugo_base_dir:~/Projects/personal-website\n#+hugo_section:refs\n#+roam_key: ${ref}

Links ::
\n#+begin_src toml :front_matter_extra t
subtitle = \"\"
summary = \"\"
tags = [\"\", \"\"]
share = true
profile = true \n#+end_src

\n* Main points\n:PROPERTIES:\n:NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n:NOTER_PAGE:\n:END:\n\n

%?

\n
bibliography:../bib/library.bib
"
           :unnarrowed t))))

(after! org-roam
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
        (s-downcase slug)))))

(after! org-noter
  (setq org-noter-always-create-frame nil
        org-noter-kill-frame-at-session-end nil))

(after! pdf-view
  (setq pdf-annot-default-annotation-properties
        '((t (label . "Linus Sehn"))
          (text (icon . "Note")
                (color . "#ff0000"))
          (highlight (color . "yellow"))
          (squiggly (color . "orange"))
          (strike-out (color . "red"))
          (underline (color . "blue"))))
  (setq pdf-annot-color-history
        '("#ffff00" "#ff6e6e" "#8cc8ff" "#6eff6e" "#c882c9")))

(use-package! org-download
  :after org
  :config
  (setq-default org-download-method 'directory
                org-download-image-dir "../img"
                org-download-heading-lvl nil))

(after! org
  (setq org-src-window-setup 'current-window
        org-babel-python-command "python3"))

(after! org
  (defun org-babel-tangle-jump ()
    "Jump to tangle file for the source block at point."
    (interactive)
    (let (file org-babel-pre-tangle-hook org-babel-post-tangle-hook)
      (cl-letf (((symbol-function 'write-region) (lambda (start end filename &rest _ignore)
                                                   (setq file filename)))
                ((symbol-function 'delete-file) #'ignore))
        (org-babel-tangle '(4)))
      (when file
        (setq file (expand-file-name file))
        (if (file-readable-p file)
            (find-file file)
          (error "Cannot open tangle file %S" file))))))

(use-package! mathpix
  :custom ((mathpix-app-id "mathpix_sehn_tech_b5ad38")
           (mathpix-app-key "f965173bcdbfec889c20")))

(after! org
  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")
        org-export-with-smart-quotes t))

(defun publish-dir-org ()
  "Publish all org files in a directory"
  (interactive)
  (save-excursion
    (mapc
     (lambda (file)
       (with-current-buffer
       (find-file-noselect file)
       (org-hugo-export-to-md)))
       (file-expand-wildcards  "*.org"))))

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

(after! ox-hugo
  (setq org-hugo-default-section-directory "zettel"))

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

(use-package! citeproc-org
  :after org
  :config
  (citeproc-org-setup))

(after! citeproc-org
  (setq citeproc-org-suppress-affixes-cite-link-types '("citet" "cite*")
        citeproc-org-suppress-author-cite-link-types '("cite*")
        citeproc-org-ignore-backends '(latex beamer icalendar)))

(after! org-ref
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
    "Redefined Formatting function for bibliography links
     using my custom md bibliogrpyh function."
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
            ",")))))))

(after! geiser-mode
    (setq geiser-active-implementations '(mit)))

(map! :leader
      (:prefix-map ("e" . "exocortex")
       :desc "Search for name" "e" #'org-roam-find-file
       :desc "Search for symbol" "x" #'my/search-exocortex
       :desc "Search public for symbol" "w" #'my/search-public
       :desc "Search zettel" "c" #'org-roam-bibtex-find-non-ref-file
       :desc "Search refs" "r" #'org-roam-find-ref)
      (:prefix-map ("d" . "dict")
       :desc "Add to dictionary" "a" #'my/save-to-dict
       :desc "Change to german" "g" #'my/switch-to-de-dict
       :desc "Change to english" "e" #'my/switch-to-en-dict)
      (:prefix-map ("i" . "insert")
       :desc "Insert math from screen" "m" #'mathpix-screenshot)
      (:prefix ("t" . "toggle/tangle")
       :desc "Detangle" "d" #'org-babel-detangle
       :desc "Transparency" "p" #'my/toggle-transparency))

(map! :map org-mode-map
      ("M-i" #'org-ref-ivy-insert-cite-link)
      ("M-e" #'my/org-ref-update-pre-post-text)
      ("M-p" #'my/org-ref-open-pdf-at-point)
      ("M-n" #'org-ref-open-notes-at-point)
      ("M-r" #'org-roam-insert)
      (:leader
       (:desc "Show todos" "z" #'ivy-magit-todos)
       (:prefix ("c" . "code/cite")
        :desc "Cite source" "i" #'org-ref-ivy-insert-cite-link
        :desc "Open pdf at point" "p" #'my/org-ref-open-pdf-at-point
        :desc "Open notes at point" "n" #'org-ref-open-notes-at-point)
       (:prefix ("t" . "toggle/tangle")
        :desc "Tangle src blocks" "t" #'org-babel-tangle
        :desc "Jump to src block" "j" #'org-babel-tangle-jump)
       (:prefix "i"
        :desc "Cite source" "c" #'org-ref-helm-insert-cite-link
        :desc "Insert anki note" "a" #'anki-editor-insert-note)
       (:prefix ("a" . "anki")
        :desc "Push notes to anki" "p" #'anki-editor-push-notes
        :desc "Cloze region" "c" #'anki-editor-cloze-dwim))
      (:localleader
       (:prefix ("b" . "tables")
        "w" #'show-weekly-clock-budget
        "m" #'show-monthly-clock-budget
        "y" #'show-yearly-clock-budget)
       (:prefix ("a" . "attachments")
        "c" #'org-download-screenshot
        "y" #'org-download-yank )))

(map! :map pdf-view-mode-map
      "C-c i" 'org-noter-insert-note)
