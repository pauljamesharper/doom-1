;; ~/.doom.d/+hugo.el -*- lexical-binding: t; -*-

(use-package! ox-hugo
  :after ox
  :config
  (setq org-hugo-default-section-directory "post"))
