;;; ~/.doom.d/+funcs.el -*- lexical-binding: t; -*-

(defun my/flyspell-save-word ()
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

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
