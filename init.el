;;; init.el -*- lexical-binding: t; -*-

(doom! :input

       :completion
       (company)
       (ivy +fuzzy +prescient +childframe)

       :ui
       doom
       doom-dashboard
       doom-quit
       hl-todo
       modeline
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
       eval
       (lookup +docsets +offline +dictionary)
       (lsp +eglot)
       magit
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
       (org +dragndrop +jupyter +present +hugo +noter +roam)
       (python +lsp +pyright +pyenv +cython)
       rest
       scheme
       (sh +lsp +fish)
       web

       :email
       mu4e

       :app
       ;;calendar
       ;;irc
       ;;(rss +org)
       ;;twitter

       :config
       literate
       (default +bindings +smartparens))
