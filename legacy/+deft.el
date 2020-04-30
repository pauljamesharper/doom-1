;;; ~/.doom.d/+deft.el -*- lexical-binding: t; -*-

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

(defun kill-buffer-regexp (regexp)
  "Kill buffers matching REGEXP without asking for permission."
  (interactive "sKill buffers matching this regexp: ")
  (cl-letf (((symbol-function 'kill-buffer-ask) #'kill-buffer))
    (kill-matching-buffers regexp)))

(defun show-org-notes ()
  (interactive)
  (kill-buffer-regexp "*Deft*")
  (setq-default deft-directory "~/org")
  (deft))

(defun show-course-notes ()
  (interactive)
  (kill-buffer-regexp "*Deft*")
  (setq-default deft-directory "~/org/archive/courses")
  (deft))
