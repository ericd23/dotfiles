;;; init.el -*- lexical-binding: t; -*-

(doom!
       :input
       chinese

       :completion
       company
       vertico

       :ui
       doom
       doom-dashboard
       hl-todo
       indent-guides
       modeline
       ophints
       (popup +defaults)
       vc-gutter
       vi-tilde-fringe

       :editor
       (evil +everywhere)
       fold
       (format +onsave)
       snippets
       word-wrap

       :emacs
       dired
       electric
       ibuffer
       undo
       vc

       :checkers
       ; syntax
       (spell +aspell)

       :tools
       editorconfig
       (eval +overlay)
       lookup
       (lsp +eglot)
       magit
       pass
       pdf

       :os
       tty

       :lang
       (cc +lsp)
       emacs-lisp
       go
       lua
       (org +roam2 +noter +hugo)
       python
       sh
       latex

       :email
       mu4e

       :app
       irc

       :config
       (default +bindings +smartparens))
