;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(setq auth-sources '("~/.authinfo.gpg"))

;; UI
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(display-time-mode 1)
;; (setq doom-font (font-spec :family "FiraCode Nerd Font Mono Regular" :size 14))
(setq doom-theme 'doom-one
      dired-dwim-target t)

(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)
(display-time-mode 1)

(after! org
  (set-company-backend! 'text-mode 'company-math-symbols-latex 'company-dabbrev)
  (setq company-math-allow-latex-symbols-in-faces t))

;; Bookmark save directory
(setq bookmark-default-file "~/.doom.d/bookmarks")

(load! "+bindings")
(load! "+pdf-tools")
(load! "+flyspell")
(load! "+typo")
(load! "+mu4e")
(load! "+deft")
(load! "+org")
(load! "+org-ref")
(load! "+org-caldav")
(load! "+org-noter")
(load! "+org-re-reveal")
(load! "+org-super-agenda")
(load! "+anki")
(load! "+ox")
(load! "+nov")
