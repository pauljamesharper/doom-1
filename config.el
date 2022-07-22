;; -*- lexical-binding: t -*-

(map!
 ;; Currently not using tabs
 ;; ("M-a" #'centaur-tabs-backward)
 ;; ("M-d" #'centaur-tabs-forward)
 ;; ("M-A" #'centaur-tabs-move-current-tab-to-left)
 ;; ("M-D" #'centaur-tabs-move-current-tab-to-right)
 ("M-q" #'kill-current-buffer)
 ("M-Q" #'evil-quit)
 :leader
 (:prefix-map ("a" . "ansible")
  :desc "Decrypt buffer" "d" #'ansible-decrypt-buffer
  :desc "Encrypt buffer" "e" #'ansible-encrypt-buffer)
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
  :desc "Transparency" "p" #'my/toggle-transparency)
 (:prefix ("f" . "file")
  :desc "Open neotree" "t" #'+neotree/open))

(map! :map org-mode-map
      ("M-i" #'org-ref-ivy-insert-cite-link)
      ("M-u" #'my/org-ref-update-pre-post-text)
      ("M-p" #'my/org-ref-open-pdf-at-point)
      ("M-n" #'org-ref-open-notes-at-point)
      ("M-r" #'org-roam-insert)
      (:leader
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
       (:prefix ("a" . "anki/ansible")
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

(setq user-full-name "Linus Sehn"
      user-mail-address "linus@sehn.tech")

(setq bookmark-default-file "~/.doom.d/bookmarks")

;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Location of project repositories
(setq projectile-project-search-path
      '("~/Exocortex/Projects"))

;; Don't use mixed-pitch-mode for any buffer type
(setq +zen-mixed-pitch-modes '())
;; Make text only slightly bigger
(setq +zen-text-scale 1.1)

(setq doom-font (font-spec :family "Source Code Pro" :size 22)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 22)
      doom-big-font (font-spec :family "Source Code Pro" :size 30))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq doom-theme 'doom-dracula
      display-time-24hr-format t
      display-time-default-load-average nil)
(display-time-mode 1)

(use-package! theme-magic
  :commands theme-magic-from-emacs
  :config
  (defadvice! theme-magic--auto-extract-16-doom-colors ()
    :override #'theme-magic--auto-extract-16-colors
    (list
     (face-attribute 'default :background)
     (doom-color 'error)
     (doom-color 'success)
     (doom-color 'type)
     (doom-color 'keywords)
     (doom-color 'constants)
     (doom-color 'functions)
     (face-attribute 'default :foreground)
     (face-attribute 'shadow :foreground)
     (doom-blend 'base8 'error 0.1)
     (doom-blend 'base8 'success 0.1)
     (doom-blend 'base8 'type 0.1)
     (doom-blend 'base8 'keywords 0.1)
     (doom-blend 'base8 'constants 0.1)
     (doom-blend 'base8 'functions 0.1)
     (face-attribute 'default :foreground))))

(run-with-idle-timer 0.1 nil (lambda () (add-hook 'doom-load-theme-hook 'theme-magic-from-emacs)))

(defun my/toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (if (eq
         (if (numberp alpha)
             alpha
           (cdr alpha)) ; may also be nil
         100)
        (set-frame-parameter nil 'alpha '(93 . 93))
      (set-frame-parameter nil 'alpha '(100 . 100)))))

(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

(defvar fancy-splash-image-template
  (expand-file-name "splash/img/emacs-e-template.svg" doom-private-dir)
  "Default template svg used for the splash image, with substitutions from ")

(defvar fancy-splash-sizes
  `((:height 300 :min-height 50 :padding (0 . 2))
    (:height 250 :min-height 42 :padding (2 . 4))
    (:height 200 :min-height 35 :padding (3 . 3))
    (:height 150 :min-height 28 :padding (3 . 3))
    (:height 100 :min-height 20 :padding (2 . 2))
    (:height 75  :min-height 15 :padding (2 . 1))
    (:height 50  :min-height 10 :padding (1 . 0))
    (:height 1   :min-height 0  :padding (0 . 0)))
  "list of plists with the following properties
  :height the height of the image
  :min-height minimum `frame-height' for image
  :padding `+doom-dashboard-banner-padding' (top . bottom) to apply
  :template non-default template file
  :file file to use instead of template")

(defvar fancy-splash-template-colours
  '(("$colour1" . keywords) ("$colour2" . type) ("$colour3" . base5) ("$colour4" . base8))
  "list of colour-replacement alists of the form (\"$placeholder\" . 'theme-colour) which applied the template")

(unless (file-exists-p (expand-file-name "theme-splashes" doom-cache-dir))
  (make-directory (expand-file-name "theme-splashes" doom-cache-dir) t))

(defun fancy-splash-filename (theme-name height)
  (expand-file-name (concat (file-name-as-directory "theme-splashes")
                            theme-name
                            "-" (number-to-string height) ".svg")
                    doom-cache-dir))

(defun fancy-splash-clear-cache ()
  "Delete all cached fancy splash images"
  (interactive)
  (delete-directory (expand-file-name "theme-splashes" doom-cache-dir) t)
  (message "Cache cleared!"))

(defun fancy-splash-generate-image (template height)
  "Read TEMPLATE and create an image if HEIGHT with colour substitutions as
   described by `fancy-splash-template-colours' for the current theme"
  (with-temp-buffer
    (insert-file-contents template)
    (re-search-forward "$height" nil t)
    (replace-match (number-to-string height) nil nil)
    (dolist (substitution fancy-splash-template-colours)
      (goto-char (point-min))
      (while (re-search-forward (car substitution) nil t)
        (replace-match (doom-color (cdr substitution)) nil nil)))
    (write-region nil nil
                  (fancy-splash-filename (symbol-name doom-theme) height) nil nil)))

(defun fancy-splash-generate-images ()
  "Perform `fancy-splash-generate-image' in bulk"
  (dolist (size fancy-splash-sizes)
    (unless (plist-get size :file)
      (fancy-splash-generate-image (or (plist-get size :template)
                                       fancy-splash-image-template)
                                   (plist-get size :height)))))

(defun ensure-theme-splash-images-exist (&optional height)
  (unless (file-exists-p (fancy-splash-filename
                          (symbol-name doom-theme)
                          (or height
                              (plist-get (car fancy-splash-sizes) :height))))
    (fancy-splash-generate-images)))

(defun get-appropriate-splash ()
  (let ((height (frame-height)))
    (cl-some (lambda (size) (when (>= height (plist-get size :min-height)) size))
             fancy-splash-sizes)))

(setq fancy-splash-last-size nil)
(setq fancy-splash-last-theme nil)
(defun set-appropriate-splash (&rest _)
  (let ((appropriate-image (get-appropriate-splash)))
    (unless (and (equal appropriate-image fancy-splash-last-size)
                 (equal doom-theme fancy-splash-last-theme)))
    (unless (plist-get appropriate-image :file)
      (ensure-theme-splash-images-exist (plist-get appropriate-image :height)))
    (setq fancy-splash-image
          (or (plist-get appropriate-image :file)
              (fancy-splash-filename (symbol-name doom-theme) (plist-get appropriate-image :height))))
    (setq +doom-dashboard-banner-padding (plist-get appropriate-image :padding))
    (setq fancy-splash-last-size appropriate-image)
    (setq fancy-splash-last-theme doom-theme)
    (+doom-dashboard-reload)))

(add-hook 'window-size-change-functions #'set-appropriate-splash)
(add-hook 'doom-load-theme-hook #'set-appropriate-splash)

(defvar splash-phrase-source-folder
  (expand-file-name "splash/phrases" doom-private-dir)
  "A folder of text files with a fun phrase on each line.")

(defvar splash-phrase-sources
  (let* ((files (directory-files splash-phrase-source-folder nil "\\.txt\\'"))
         (sets (delete-dups (mapcar
                             (lambda (file)
                               (replace-regexp-in-string "\\(?:-[0-9]+-\\w+\\)?\\.txt" "" file))
                             files))))
    (mapcar (lambda (sset)
              (cons sset
                    (delq nil (mapcar
                               (lambda (file)
                                 (when (string-match-p (regexp-quote sset) file)
                                   file))
                               files))))
            sets))
  "A list of cons giving the phrase set name, and a list of files which contain phrase components.")

(defvar splash-phrase-set
  (nth (random (length splash-phrase-sources)) (mapcar #'car splash-phrase-sources))
  "The default phrase set. See `splash-phrase-sources'.")

(defun splase-phrase-set-random-set ()
  "Set a new random splash phrase set."
  (interactive)
  (setq splash-phrase-set
        (nth (random (1- (length splash-phrase-sources)))
             (cl-set-difference (mapcar #'car splash-phrase-sources) (list splash-phrase-set))))
  (+doom-dashboard-reload t))

(defvar splase-phrase--cache nil)

(defun splash-phrase-get-from-file (file)
  "Fetch a random line from FILE."
  (let ((lines (or (cdr (assoc file splase-phrase--cache))
                   (cdar (push (cons file
                                     (with-temp-buffer
                                       (insert-file-contents (expand-file-name file splash-phrase-source-folder))
                                       (split-string (string-trim (buffer-string)) "\n")))
                               splase-phrase--cache)))))
    (nth (random (length lines)) lines)))

(defun splash-phrase (&optional set)
  "Construct a splash phrase from SET. See `splash-phrase-sources'."
  (mapconcat
   #'splash-phrase-get-from-file
   (cdr (assoc (or set splash-phrase-set) splash-phrase-sources))
   " "))

(defun doom-dashboard-phrase ()
  "Get a splash phrase, flow it over multiple lines as needed, and make fontify it."
  (mapconcat
   (lambda (line)
     (+doom-dashboard--center
      +doom-dashboard--width
      (with-temp-buffer
        (insert-text-button
         line
         'action
         (lambda (_) (+doom-dashboard-reload t))
         'face 'doom-dashboard-menu-title
         'mouse-face 'doom-dashboard-menu-title
         'help-echo "Random phrase"
         'follow-link t)
        (buffer-string))))
   (split-string
    (with-temp-buffer
      (insert (splash-phrase))
      (setq fill-column (min 70 (/ (* 2 (window-width)) 3)))
      (fill-region (point-min) (point-max))
      (buffer-string))
    "\n")
   "\n"))

(defadvice! doom-dashboard-widget-loaded-with-phrase ()
  :override #'doom-dashboard-widget-loaded
  (setq line-spacing 0.2)
  (insert
   "\n\n"
   (propertize
    (+doom-dashboard--center
     +doom-dashboard--width
     (doom-display-benchmark-h 'return))
    'face 'doom-dashboard-loaded)
   "\n"
   (doom-dashboard-phrase)
   "\n"))

;; remove unneeded shortcuts
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1))
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))

(setq +treemacs-git-mode 'deferred)

(after! company
  (setq +lsp-company-backends '(company-tabnine :separate company-capf company-yasnippet))
  (setq company-show-numbers t
        company-idle-delay 0
        company-minimum-prefix-length 2
        company-show-quick-access t
        company-quick-access-modifier 'super))

(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

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

(setq org-directory "~/Dropbox/Exocortex")

(after! org
  (use-package! org-super-agenda
    :after org-agenda
    :init
    (setq org-agenda-skip-scheduled-if-done 't)
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
    ;; (setq org-agenda-dim-blocked-tasks nil) ;; makes main tasks visible in agenda-view
    (setq org-agenda-files
          '("~/Dropbox/Exocortex/org/projects-active.org" "~/Dropbox/Exocortex/org/caldav.org" "~/Dropbox/Exocortex/org/actions.org" "~/Dropbox/Exocortex/org/calendar.org" "~/Dropbox/Exocortex/org/someday.org"))
    (setq org-super-agenda-groups
          '(
            (:name "Open deep tasks this quarter"
             :tag ("@deep"))
            (:name "Open shallow tasks this quarter"
             :tag ("@shallow"))
            ;; (:name "Overdue"
            ;;  :deadline past)
            ;; (:name "Due soon"
            ;;  :deadline future)
            ;; (:name "Habits"
            ;;  :habit t)
            ;; (:name "Start today"
            ;;  :scheduled today)
            ;; (:name "Start soon"
            ;;  :scheduled future)
            ;; (:name "Reschedule or review"
            ;;  :scheduled past)
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
           "FIX(f)"   ; Something that needs fixing
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
  (setq org-caldav-url "https://posteo.de:8443/calendars/pjharper"
        org-caldav-calendar-id "default"
        org-caldav-inbox "~/Dropbox/Exocortex/org/caldav.org"
        org-caldav-files '("~/Dropbox/Exocortex/org/calendar.org"
                           "~/Dropbox/Exocortex/org/actions.org"
                           "~/Dropbox/Exocortex/org/someday.org"))
  :config
  (setq org-icalendar-timezone "Africa/Nairobi"
        org-icalendar-alarm-time 15
        org-icalendar-include-todo t
        org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due)
        org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo)
        org-icalendar-exclude-tags '("weekly" "daily" "monthly")
        org-caldav-exclude-tags '("weekly" "daily" "monthly")))

(setq org-clock-mode-line-total 'today)

(add-hook
 'org-mode-hook
 (lambda ()

   ;; Org clock string to Gnome top bar. Needs :
   ;; https://extensions.gnome.org/extension/974/short-memo/
   (defun current-task-to-status ()
     (interactive)
     (if (fboundp 'org-clocking-p)
         (if (org-clocking-p)
             (f-write-text
              (s-replace-all '(("(" . "") (")" . ""))
                (org-clock-get-clock-string))
                'utf-8 "/home/pharper/.clock")
           (f-write-text "No active clock! What are you doing?"
              'utf-8 "/home/lino/.clock")
           )))
   ;; update clock message every minute
   (run-with-timer 0 15 'current-task-to-status)

   ;; update clock immediately on clock-in / clock-out
   (defun my-org-clock-message (old-function &rest arguments)
     (apply old-function arguments)
     (current-task-to-status))
   (advice-add #'org-clock-in :around #'my-org-clock-message)
   (advice-add #'org-clock-out :around #'my-org-clock-message)
   ))

(after! org
  (setq org-capture-templates
        '(("t" "TODO" entry
           (file+headline "~/Dropbox/Exocortex/org/actions.org" "Other")
           "* TODO %?\n%a\n")
          ("a" "APPOINTMENT" entry
           (file+headline "~/Dropbox/Exocortex/org/calendar.org" "2022_Q2")
           "* %?\n%(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n"))))

(after! org
  (setq org-journal-dir "~/Dropbox/Exocortex/org/journal"
        org-journal-file-format "%Y.org"
        org-journal-encrypt-journal 't
        org-journal-file-type 'yearly))

(after! org-roam
  (setq org-roam-directory "~/Dropbox/Exocortex/"
        org-roam-db-location "~/Dropbox/Exocortex/roam.sqlite"
        ;; don't match my private org stuff
        org-roam-file-exclude-regexp "/org"))

(defun my/search-exocortex ()
  "Perform a text search on ~/Exocortex."
  (interactive)
  (require 'org)
  (let ((default-directory "~/Dropbox/Exocortex"))
    (+default/search-project-for-symbol-at-point "")))

(defun my/search-public ()
  "Perform a text search on ~/Projects/exocortex-public."
  (interactive)
  (let ((default-directory "~/Dropbox/Projects/exocortex-public"))
    (+default/search-cwd "")))

(after! citar
  (setq citar-bibliography '("~/Dropbox/Exocortex/bib/library.bib")
        citar-library-paths '("~/Dropbox/Exocortex/pdfs")
        citar-notes-path '("~/Dropbox/Exocortex/refs")
        citar-file-open-note-function 'orb-citar-edit-note
        citar-file-note-org-include '(org-id org-roam-ref)
        citar-at-point-function 'embark-act
        bibtex-completion-bibliography '("~/Dropbox/Exocortex/bib/library.bib"))
        ;; bibtex-completion-notes-path '("~/Exocortex/refs"))
  ;; set icon   s
  (setq citar-symbols
   `((file . (,(all-the-icons-icon-for-file "foo.pdf" :face 'all-the-icons-dred) .
              ,(all-the-icons-icon-for-file "foo.pdf" :face 'citar-icon-dim)))
     (note . (,(all-the-icons-icon-for-file "foo.txt") .
              ,(all-the-icons-icon-for-file "foo.txt" :face 'citar-icon-dim)))
     (link .
         (,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'all-the-icons-dpurple) .
          ,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'citar-icon-dim)))))
  ;; Here we define a face to dim non 'active' icons, but preserve alignment
  (defface citar-icon-dim
      '((((background dark)) :foreground "#282c34")
        (((background light)) :foreground "#fafafa"))
       "Face for obscuring/dimming icons"
       :group 'all-the-icons-faces))

(use-package! org-roam-ui
    :after org
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(add-hook! 'org-mode-hook 'anki-editor-mode)
(after! org
  (setq anki-editor-ignored-org-tags '("noexport")))

(after! org-roam
  (setq org-roam-capture-templates
        '(("z" "zettel"
           plain (function org-roam-capture--get-point)

           :file-name "zettel/${slug}"
           :head "#+title: ${title}\n#+hugo_base_dir:~/Dropbox/Projects/personal-website

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
#+hugo_base_dir:~/Dropbox/Projects/personal-website
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
  (setq org-noter-always-create-frame t
        org-noter-kill-frame-at-session-end t))

(after! pdf-view
  (setq pdf-annot-default-annotation-properties
        '((t (label . "Paul Harper"))
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
                ;; org-download-screenshot-method "grimshot save area %s"
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
           (mathpix-app-key "f965173bcdbfec889c20")
           ;; (mathpix-screenshot-method "grimshot save area %s")
           ))

(after! org
  (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s")))

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

(after! ox-hugo
  (setq org-hugo-default-section-directory "post"))

(use-package! string-inflection
  :commands (string-inflection-all-cycle
             string-inflection-toggle
             string-inflection-camelcase
             string-inflection-lower-camelcase
             string-inflection-kebab-case
             string-inflection-underscore
             string-inflection-capital-underscore
             string-inflection-upcase)
  :init
  (map! :leader :prefix ("rr" . "rename symbol according to convention")
        :desc "cycle" "r" #'string-inflection-all-cycle
        :desc "toggle" "t" #'string-inflection-toggle
        :desc "CamelCase" "c" #'string-inflection-camelcase
        :desc "downCase" "d" #'string-inflection-lower-camelcase
        :desc "kebab-case" "k" #'string-inflection-kebab-case
        :desc "under_score" "_" #'string-inflection-underscore
        :desc "Upper_Score" "u" #'string-inflection-capital-underscore
        :desc "UP_CASE" "U" #'string-inflection-upcase))

;; (add-hook! 'yaml-mode-hook '(lambda () (ansible 1)))

(setq ansible-vault-password-file "~/.vault_pass.sh")

;; (use-package! lsp-tailwindcss
;;   :init
;;   (setq! lsp-tailwindcss-add-on-mode t))
(add-hook 'js2-mode-hook #'format-all-mode)
(setq +format-with-lsp nil)
