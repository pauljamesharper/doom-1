;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(setq auth-sources '("~/.authinfo.gpg"))

;; UI
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(display-time-mode 1)
(setq doom-font (font-spec :family "Roboto Mono for Powerline" :size 14))
(setq doom-theme 'doom-one
      treemacs-width 45
      dired-dwim-target t)

(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)
(display-time-mode 1)

;; Bookmark save directory
(setq bookmark-default-file "~/.doom.d/bookmarks")

(load! "+bindings")
(load! "+pdf-tools")
(load! "+typo")
(load! "+mu4e")
(load! "+deft")
(load! "+org")
(load! "+org-ref")
(load! "+org-caldav")
(load! "+org-noter")
(load! "+org-re-reveal")
(load! "+org-super-agenda")
(load! "+ox")
(load! "+nov")
