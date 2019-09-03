;;; ~/.doom.d/+reveal.el -*- lexical-binding: t; -*-

(use-package! org-re-reveal
  :after org
  :init
  ;; Fix #1127, where ox-reveal adds an errant entry to
  ;; `org-structure-template-alist'
  (setq org-re-reveal-note-key-char nil)
  :config
  (setq org-re-reveal-root "/home/lino/.local/share/reveal/"
        org-re-reveal-mathjax t))
