;;; ~/.doom.d/+typo.el -*- lexical-binding: t; -*-

(use-package! typo
    :config
    (typo-global-mode 1)
    (add-hook 'text-mode-hook 'typo-mode))
