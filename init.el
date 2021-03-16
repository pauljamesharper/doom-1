;;; init.el -*- lexical-binding: t; -*-

(doom! :input

       :completion
       (company +childframe)
       (ivy +fuzzy +prescient +childframe)

       :ui
       doom
       doom-dashboard
       doom-quit
       hl-todo
       indent-guides
       modeline
       nav-flash
       ophints
       (popup +all +defaults)
       ;; treemacs
       neotree
       vc-gutter
       vi-tilde-fringe
       tabs
       window-select
       workspaces
       zen

       :editor
       (evil +everywhere)
       file-templates
       fold
       (format +onsave)
       lispy
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
       biblio
       ansible
       (docker +lsp)
       direnv
       eval
       (lookup +docsets +offline +dictionary)
       lsp
       (magit +forge)
       pass
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
       (javascript +lsp)
       (json +lsp)
       (latex +latexmk +cdlatex)
       (markdown +grip)
       (org +dragndrop +hugo +journal +jupyter +noter +present )
       (php +lsp)
       (python +lsp)
       (racket +xp)
       rest
       ;; scheme
       (sh +lsp) web
       :email
       mu4e

       :app
       irc
       ;;calendar
       ;;(rss +org)
       ;;twitter

       :config
       literate
       (default +bindings +smartparens))
