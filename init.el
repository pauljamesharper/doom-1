;;; init.el -*- lexical-binding: t; -*-

(doom! :input

       :completion
       (company +childframe)
       (vertico)

       :ui
       doom
       doom-dashboard
       doom-quit
       hl-todo
       indent-guides
       (modeline +light)
       nav-flash
       ophints
       (popup +all +defaults)
       treemacs
       vc-gutter
       vi-tilde-fringe
       window-select
       workspaces
       zen

       :editor
       (evil +everywhere)
       file-templates
       fold
       (format +onsave)
       snippets

       :emacs
       (dired +icons)
       electric
       (ibuffer +icons)
       undo
       vc

       :term
       vterm

       :checkers
       syntax
       (spell +flyspell +hunspell)
       grammar

       :tools
       lsp
       biblio
       ansible
       (docker +lsp)
       direnv
       eval
       (lookup +docsets +offline +dictionary)
       (magit +forge)
       pdf
       rgb
       upload

       :os
       tty

       :lang
       (cc +lsp)
       data
       emacs-lisp
       (go +lsp)
       (java +lsp)
       (javascript +lsp)
       (json +lsp)
       (latex +latexmk +cdlatex)
       (markdown +grip)
       (org +dragndrop +hugo +journal +jupyter +noter +present )
       (php +lsp)
       (python +lsp +pyright)
       (racket +xp)
       rest
       (sh +lsp)
       (web +lsp)

       :email
       mu4e

       :app
       everywhere

       :config
       literate
       (default +bindings +smartparens))
