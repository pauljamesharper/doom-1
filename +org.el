;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

;; HOOKS
(add-hook! 'org-mode-hook 'my/org-cycle-hide-properties-everywhere)
(add-hook! 'org-mode-hook (lambda () (require 'org-ref)))


(after! org
  (add-to-list 'org-modules 'org-habit t)
  (setq org-directory "~/org"
        org-agenda-files (list org-directory))

  ;; 8 lines in which bold/emphasis is displayed
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (setcar (nthcdr 4 org-emphasis-regexp-components) 8)

  ;; Org-Archive
  (setq org-archive-location "~/org/archive/%s_archive.org::")

  (setq org-export-backends '(md ascii html icalendar latex))
  ;; Capture Templates
  (setq org-capture-templates
      '(("t" "TODO" entry
        (file+headline "~/org/actions.org" "Other")
        "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
        ("a" "APPOINTMENT" entry
        (file+headline "~/org/calendar.org" "Appointments")
        "* %?\n%(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
        ("d" "DISTRACTION" entry
        (file "~/org/distractions.org")
        "* %?\n%T\n** What was I doing\n** What was the trigger?"))
      )

  ;; Org Keywords
  (setq org-todo-keywords
  '((sequence "TODO(t)" "PROJ(p)" "|" "DONE(d)")
    (sequence "[ ](T)" "[-](P)" "[?](M)" "|" "[X](D)")
    (sequence "NEXT(n)" "WAIT(w)" "HOLD(h)" "|" "ABRT(c)")
    (sequence "TOREAD(r)" "|" "READ(R)")))

  ;; Journal
  ;; (setq org-journal-encrypt-journal t)
  ;; (setq org-journal-file-type 'monthly)
  ;; (setq org-journal-dir "~/org/journal/")
  ;; (setq org-journal-date-format "%A, %d %B %Y")

  (setq org-file-apps
        '((auto-mode . emacs)
          ("\\.x?html?\\'" . "firefox %s"))))

  ;; (pushnew! org-link-abbrev-alist
  ;;           '("autocite"      . "autocite:%s")
  ;;           '("textcite"      . "textcite:%s"))


  ;; (use-package! ivy-bibtex
  ;;   :defer t
  ;;   :config
  ;;   (setq bibtex-completion-notes-path "~/org/bibnotes")
  ;;   (setq bibtex-completion-pdf-field "file")
  ;;   (setq ivy-re-builders-alist
  ;;         '((ivy-bibtex . ivy--regex-ignore-order)
  ;;           (t . ivy--regex-plus))
  ;;         bibtex-completion-bibliography '("~/library.bib")))

  ;; ;; Reference Management
  ;; ;;
  ;; (use-package! org-ref
  ;;   :init
  ;;   ;; Tell org-ref to let helm-bibtex find notes for it
  ;;   (setq org-ref-notes-function
  ;;         (lambda (thekey)
  ;;     (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
  ;;       (bibtex-completion-edit-notes
	;;    (list (car (org-ref-get-bibtex-key-and-file thekey)))))))
  ;;   (setq org-ref-completion-library 'org-ref-ivy-cite)
  ;;   :config
  ;;   (setq org-ref-default-citation-link "autocite"
  ;;         org-ref-default-bibliography '("~/Library/.bib/library.bib" "~/Library/.bib/platform_state_surveillance.bib")
  ;;         org-ref-pdf-directory "~/Library"))

  ;; (use-package! bibtex
  ;;   :defer t
  ;;   :config
  ;;   (setq bibtex-dialect 'biblatex))

  ;; (defun my/org-ref-open-pdf-at-point ()
  ;;   "Open the pdf for bibtex key under point if it exists."
  ;;   (interactive)
  ;;   (let* ((results (org-ref-get-bibtex-key-and-file))
  ;;         (key (car results))
  ;;   (pdf-file (car (bibtex-completion-find-pdf key))))
  ;;     (if (file-exists-p pdf-file)
  ;;   (org-open-file pdf-file)
  ;;       (message "No PDF found for %s" key))))

  ;; (setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)


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
