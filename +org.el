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


;;WIHTIN ORG-MODE
(after! org

  (add-to-list 'org-modules 'org-habit t)
  (setq org-directory "~/org/"
      org-agenda-files (list org-directory))

  ;; Org-Agenda
  (setq org-agenda-window-setup 'only-window)
  (setq org-agenda-files '("~/org/actions.org"
                           "~/org/strategy.org"
                           "~/org/calendar.org"
                           "~/org/calendar-inbox.org"))

  ;; Org-Archive
  (setq org-archive-location "~/Archive/org-archive.org::* From %s")

  ;;Capture Templates
  (setq org-capture-templates
        '(("t" "TODO" entry
         (file+headline "~/org/actions.org" "Other")
          "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
         ("a" "APPOINTMENT" entry
         (file+headline "~/org/calendar.org" "Appointments")
          "* %?\nSCHEDULED: %^T\n%a\n"))
        )

  (setq org-todo-keywords
   '((sequence "TODO(t)" "PROJ(p)" "|" "DONE(d)")
     (sequence "[ ](T)" "[-](P)" "[?](M)" "|" "[X](D)")
     (sequence "NEXT(n)" "WAIT(w)" "HOLD(h)" "|" "ABRT(c)")
     (sequence "TOREAD(r)" "|" "READ(R)")))

  ;; Latex-Export
  (setq org-latex-bib-compiler "biber"
        org-latex-pdf-process ; -shell-escape needed for minted
        '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "biber %b"
          "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; Bibliography
  (add-to-list 'org-latex-packages-alist
               "\\usepackage[backend=biber, eprint=false, url=true,
               isbn=false, style=oxyear,
               date=year]{biblatex}" t)
  (add-to-list 'org-latex-packages-alist
               "\\addbibresource{~/library.bib}" t)
  )

;; ORG-MODE EXTERNAL PACKAGES
(setq org-journal-dir "~/org/.journal/")
(setq org-journal-date-format "%A, %d %B %Y")


(use-package! org-noter
  :config
  (setq org-noter-notes-search-path '("~/org/bibnotes" )))
