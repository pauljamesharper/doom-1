;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(setq auth-sources '("~/.authinfo.gpg"))

;; General UI adjustments
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq doom-theme 'doom-one
      doom-themes-enable-bold t
      dired-dwim-target t
      display-time-24hr-format t
      display-time-default-load-average nil)
(display-time-mode 1)

;; Bookmark save directory
(setq bookmark-default-file "~/.doom.d/bookmarks")

;; Modules
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
(load! "+org-clock-budget")
(load! "+anki")
(load! "+ox")
(load! "+zen")
(load! "+nov")
