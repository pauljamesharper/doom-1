;;; ~/.doom.d/+flyspell.el -*- lexical-binding: t; -*-

(after! flyspell
  (defun my/flyspell-save-word ()
    (interactive)
    (let ((current-location (point))
          (word (flyspell-get-word)))
      (when (consp word)
        (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location)))))
