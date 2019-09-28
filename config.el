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

;; Bookmark save directory
(setq bookmark-default-file "~/.doom.d/bookmarks")

;; typo.el
(use-package! typo
    :config
    (typo-global-mode 1)
    (add-hook 'text-mode-hook 'typo-mode))

(load! "+funcs")
(load! "+bindings")
(load! "+org")
(load! "+latex")
(load! "+refman")
(load! "+mu4e")
(load! "+reveal")
(load! "+caldav")
(load! "+flyspell")
(load! "+pdf-annot")
(load! "+ui")
(load! "+hugo")
