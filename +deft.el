;;; ~/.doom.d/+deft.el -*- lexical-binding: t; -*-

(after! deft
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

  (defun show-org-notes ()
    (interactive)
    (setq-default deft-directory "~/org")
    (deft))

  (defun show-course-notes ()
    (interactive)
    (setq-default deft-directory "~/org/archive/courses")
    (deft)))
