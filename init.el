;;; init.el -*- lexical-binding: t; -*-

(doom! :input

       :completion
       (company +childframe)
       (vertico)

       :ui
       doom
       doom-dashboard
       doom-quit
       (emoji +unicode)  ; 🙂
       hl-todo
       indent-guides
       (modeline +light)
       ;;ligatures
       minimap
       nav-flash
       ophints
       (popup +all +defaults)
       ;;tabs
       treemacs
       vc-gutter
       vi-tilde-fringe
       workspaces
       ;;zen

       :editor
       (evil +everywhere)
       (format +onsave)
       file-templates
       fold
       parinfer
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
       (spell +flyspell +hunspell)
       grammar
       syntax

       :tools
       (docker +lsp)
       (lookup +offline +dictionary)
       (magit +forge)
       ansible
       biblio
       direnv
       eval
       lsp
       make
       pdf
       rgb
       terraform
       upload

       :os
       tty

       :lang
       data
       emacs-lisp
       (scheme +mit)
       (go +lsp)
       (java +lsp)
       (javascript +lsp)
       (json +lsp)
       (latex +latexmk)
       markdown
       (org +dragndrop +hugo +journal +jupyter +noter +pomodoro +present +roam2)
       (php +lsp)
       (python +lsp)
       (racket +xp)
       rest
       (sh +lsp)
       (web +lsp)

       :email
       ;; mu4e

       :app
       everywhere

       :config
       literate
       (default +bindings +smartparens))
