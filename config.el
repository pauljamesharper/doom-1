;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(load! "+funcs")
(load! "+bindings")
(load! "+org")
(load! "+refman")
(load! "+mu4e")
(load! "+reveal")
(load! "+caldav")
(load! "+flyspell")
(load! "+pdf-annot")
(load! "+ui")

(setq auth-sources '("~/.authinfo.gpg"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; UI
(setq doom-theme 'doom-one
      treemacs-width 45
      dired-dwim-target t)

;; Bookmark save directory
(setq bookmark-default-file "~/.doom.d/bookmarks")

