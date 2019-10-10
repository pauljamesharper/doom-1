;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

;; ORG-FUNCTIONS
(defun my/org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(defun my/org-cycle-hide-drawers (state)
"Re-hide all drawers after a visibility state change."
(when (and (derived-mode-p 'org-mode)
          (not (memq state '(overview folded contents))))
  (save-excursion
    (let* ((globalp (memq state '(contents all)))
        (beg (if globalp
                (point-min)
                (point)))
        (end (if globalp
                (point-max)
                (if (eq state 'children)
                  (save-excursion
                    (outline-next-heading)
                    (point))
                  (org-end-of-subtree t)))))
    (goto-char beg)
    (while (re-search-forward org-drawer-regexp end t)
      (save-excursion
        (beginning-of-line 1)
        (when (looking-at org-drawer-regexp)
          (let* ((start (1- (match-beginning 0)))
                (limit
                  (save-excursion
                    (outline-next-heading)
                      (point)))
                (msg (format
                        (concat
                          "org-cycle-hide-drawers:  "
                          "`:END:`"
                          " line missing at position %s")
                        (1+ start))))
            (if (re-search-forward "^[ \t]*:END:" limit t)
              (outline-flag-region start (point-at-eol) t)
              (user-error msg))))))))))

(defun my/org-cycle-hide-properties-everywhere ()
  (interactive)
  (my/org-cycle-hide-drawers 'all)
  )

(defun my/org-cycle-hide-properties-children ()
  (interactive)
  (my/org-cycle-hide-drawers 'children)
  )

;; HOOKS
(add-hook! 'org-mode-hook 'my/org-cycle-hide-properties-everywhere)
(add-hook! 'org-mode-hook (lambda () (require 'org-ref)))

;;WIHTIN ORG-MODE
(after! org

  (add-to-list 'org-modules 'org-habit t)
  (setq org-directory "~/org/"
    org-agenda-files (list org-directory))

  ;; Org-Agenda
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-start-day "-1d")
  (setq org-agenda-span 5)
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-files '("~/org/actions.org"
                          "~/org/strategy.org"
                          "~/org/reading.org"
                          "~/org/watching.org"
                          "~/org/calendar.org"
                          "~/org/outreach.org"
                          "~/org/calendar-inbox.org"))

  (use-package! org-super-agenda
    :after org-agenda
    :init
    (setq org-super-agenda-groups '((:name "Overdue"
                                           :deadline past)
                                    (:name "Due"
                                           :deadline today)
                                    (:name "Due soon"
                                           :deadline future
                                           :children t)
                                    (:name "Morning Ritual"
                                           :tag "morning")
                                    (:name "Today"
                                           :time-grid t
                                           :scheduled today)
                                    (:name "Shutdown Ritual"
                                           :tag "shutdown")
                                    (:name "Overdue Start"
                                           :discard (:habit t)
                                           :scheduled past)
                                    (:name "Start soon"
                                           :scheduled future)))
    :config
    (org-super-agenda-mode))

  ;; Org-Archive
  (setq org-archive-location "~/org/archive/%s_archive.org::")

  ;; Capture Templates
  (setq org-capture-templates
      '(("t" "TODO" entry
        (file+headline "~/org/actions.org" "Other")
        "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
        ("a" "APPOINTMENT" entry
        (file+headline "~/org/calendar.org" "Appointments")
        "* %?\nSCHEDULED: %^T\n%a\n"))
      )

  ;; Org Keywords
  (setq org-todo-keywords
  '((sequence "TODO(t)" "PROJ(p)" "|" "DONE(d)")
    (sequence "[ ](T)" "[-](P)" "[?](M)" "|" "[X](D)")
    (sequence "NEXT(n)" "WAIT(w)" "HOLD(h)" "|" "ABRT(c)")
    (sequence "TOREAD(r)" "|" "READ(R)")))

  ;; Export
  (setq org-latex-bib-compiler "biber"
      org-latex-pdf-process ; -shell-escape needed for minted
      '("latexmk -shell-escape -bibtex -pdf %f"))

  (add-to-list 'org-latex-packages-alist
              "\\usepackage[backend=biber, eprint=false, url=true,
              isbn=false, style=authoryear-icomp,
              date=year]{biblatex}" t)
  (add-to-list 'org-latex-packages-alist
              "\\addbibresource{~/library.bib}" t)

  (after! ox-latex
    (add-to-list 'org-export-smart-quotes-alist
                '("en_cs"
                    (primary-opening   :utf-8 "“" :html "&ldquo;" :latex "\\enquote{"  :texinfo "``")
                    (primary-closing   :utf-8 "”" :html "&rdquo;" :latex "}"           :texinfo "''")
                    (secondary-opening :utf-8 "‘" :html "&lsquo;" :latex "\\enquote*{" :texinfo "`")
                    (secondary-closing :utf-8 "’" :html "&rsquo;" :latex "}"           :texinfo "'")
                    (apostrophe        :utf-8 "’" :html "&rsquo;")))
    )

  (use-package! ox-word
    :load-path "~/.doom.d/load/ox-word/"
    :after ox)

  ;; Journal
  (setq org-journal-dir "~/org/journal/")
  (setq org-journal-date-format "%A, %d %B %Y")

  ;; Reference Management
  (use-package! org-ref
    :init
    (setq org-ref-completion-library 'org-ref-helm-bibtex)
    :config
    (setq org-ref-default-citation-link "autocite"
          org-ref-default-bibliography '("~/library.bib")
          org-ref-pdf-directory "~/Zotero/storage/"))

  (use-package! helm-bibtex
    :commands helm-bibtex
    :config
    (setq bibtex-completion-library-path '("~/Zotero/storage/")
          bibtex-completion-pdf-field "file"
          bibtex-completion-bibliography
          '("~/library.bib")))

  (use-package! bibtex
    :defer t
    :config
    (setq bibtex-dialect 'biblatex))

  (use-package! bibtex-completion
    :defer t
    :config
    (setq bibtex-completion-notes-path "~/org/bibnotes/"))

  (use-package! org-noter
    :config
    (setq org-noter-notes-search-path '("~/org/bibnotes" )))

  (defun my/org-ref-open-pdf-at-point ()
    "Open the pdf for bibtex key under point if it exists."
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
          (key (car results))
    (pdf-file (car (bibtex-completion-find-pdf key))))
      (if (file-exists-p pdf-file)
    (org-open-file pdf-file)
        (message "No PDF found for %s" key))))

  (setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)

  (setq org-file-apps
        '((auto-mode . emacs)
          ("\\.x?html?\\'" . "firefox %s")
          ))
)
