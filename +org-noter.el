;;; ~/.doom.d/+org-noter.el -*- lexical-binding: t; -*-

(use-package! org-noter
  :defer t
  :config
  (setq org-noter-notes-search-path '("~/org/bibnotes" )
        org-noter-default-notes-file-names '("notes.org")
        org-noter-always-create-frame t))
