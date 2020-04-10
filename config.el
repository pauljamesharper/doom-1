;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(setq auth-sources '("~/.authinfo.gpg"))

;; transparency
(add-to-list 'default-frame-alist '(alpha . (92 . 90)))
(set-frame-parameter (selected-frame) 'alpha '(92 . 90))

;; general UI adjustments
(setq doom-theme 'doom-one
      doom-themes-enable-bold t
      dired-dwim-target t
      display-time-24hr-format t
      display-time-default-load-average nil)

;; only 24 hour clock in modeline
(display-time-mode 1)

;; bookmark save directory
(setq bookmark-default-file "~/.doom.d/bookmarks")

;; latex-viewer
(setq +latex-viewers '(evince))

;; default Projectile Search Path
(setq projectile-project-search-path '("~/Projects" "/home/lino"))

;; config modules
(load! "+bindings")
(load! "+crypt")
(load! "+pdf-tools")
(load! "+flyspell")
(load! "+typo")
(load! "+mu4e")
(load! "+deft")
(load! "+org")
(load! "+org-ref")
(load! "+org-journal")
(load! "+org-caldav")
(load! "+org-noter")
(load! "+org-re-reveal")
(load! "+org-super-agenda")
(load! "+org-clock-budget")
(load! "+anki")
(load! "+ox")
(load! "+zen")
(load! "+nov")
