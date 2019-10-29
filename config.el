;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(setq auth-sources '("~/.authinfo.gpg"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; UI
(display-time-mode 1)
(setq doom-font (font-spec :family "Roboto Mono for Powerline" :size 14))
(setq doom-theme 'doom-one
      treemacs-width 45
      dired-dwim-target t)

(display-time-mode 1)
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)

;; Bookmark save directory
(setq bookmark-default-file "~/.doom.d/bookmarks")

;; flyspell
(defun my/flyspell-save-word ()
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

;; pdf-tools
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
(add-hook 'pdf-view-mode-hook #'pdf-tools-enable-minor-modes)

(setq +latex-viewers '(pdf-tools))
(setq pdf-annot-color-history '("LightGoldenrod1" "salmon" "pale green" "cornflower blue")
      pdf-annot-default-annotation-properties
      '((t (label . "Lino on Emacs"))
        (text (icon . "Note")
              (color . "#ff0000"))
        (highlight (color . "LightGoldenrod1"))))

(load! "+bindings")
(load! "+typo")
(load! "+mu4e")
(load! "+org")
(load! "+org-ref")
(load! "+org-caldav")
(load! "+org-noter")
(load! "+org-re-reveal")
(load! "+org-super-agenda")
(load! "+ox")
(load! "+nov")
