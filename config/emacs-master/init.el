;;; init.el -*- lexical-binding: t; -*-


;;; stuff
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(set-frame-font "Fira Code 14" nil t)

(load-theme 'modus-vivendi)
(global-set-key (kbd "<f6>") 'toggle-theme)

(setq-default show-trailing-whitespace t)

(setq scroll-step 1)
(setq scroll-margin 2)

(setq uniquify-buffer-name-style 'forward)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      backup-by-copying t
      custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "C-c K") 'kill-current-buffer)

(setq org-directory "~/documents/org")

(electric-pair-mode 1)

(setq vc-follow-symlinks t)

(setq native-comp-async-report-warnings-errors 'silent)

(savehist-mode 1)


;;; packages
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(setq ericd/package-list '(elisp-slime-nav
                           meow
                           simpleclip
                           magit
                           company))

(dolist (package ericd/package-list)
  (unless (package-installed-p package)
    (package-install package)))

(eval-when-compile
  (require 'use-package))


;;; util functions
(defun ericd/open-init-el ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))
(global-set-key (kbd "<f5>") 'ericd/open-init-el)

(defun ericd/delete-buffer-and-file ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))
(global-set-key (kbd "C-c d") 'ericd/delete-buffer-and-file)

(defun ericd/org-set-hugo-draft-true ()
  (interactive)
  (org-roam-set-keyword "hugo_draft" "true"))

(defun ericd/org-set-hugo-draft-false ()
  (interactive)
  (org-roam-set-keyword "hugo_draft" "false"))


;;; keys
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))
(require 'meow)
(meow-setup)
(meow-global-mode 1)


;;; use-packages
(use-package paren
  :init (setq show-paren-delay 0)
  :config (show-paren-mode 1))

(use-package simpleclip
  :init
  (setq simpleclip-copy-keystrokes '("C-c c")
        simpleclip-cut-keystrokes '("C-c x")
        simpleclip-paste-keystrokes '("C-c v"))
  :config
  (simpleclip-mode 1))

(use-package eglot
  :bind (("C-c L" . eglot)
         ("C-c l s" . eglot-shutdown)
         ("C-c l r" . eglot-rename)
         ("C-c l f" . eglot-format)
         ("C-c k" . eldoc) ; FIXME: not working
         ("C-c l d" . xref-find-definitions)
         ("C-c l m" . imenu))
  :config
  (add-to-list 'eglot-ignored-server-capabilites :hoverProvider))

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :bind (("C-c z" . hs-hide-block)
         ("C-c Z" . hs-hide-all)
         ("C-c o" . hs-show-block)
         ("C-c O" . hs-show-all)))

(use-package org-roam
  :init
  (setq org-roam-directory (expand-file-name "box/" org-directory)
        org-roam-db-location (concat org-roam-directory "org-roam.db")
        org-id-locations-file (concat org-roam-directory ".orgids"))
  :bind
  (("C-c r f" . org-roam-node-find)
   ("C-c r b" . org-roam-buffer-toggle)
   ("C-c r i" . org-roam-node-insert)
   ("C-c r P" . ericd/org-set-hugo-draft-true)
   ("C-c r p" . ericd/org-set-hugo-draft-false))
  :config
  (setq org-id-track-globally t)
  (org-roam-db-autosync-mode 1)
  (add-hook 'org-roam-capture-new-node-hook #'ericd/org-set-hugo-draft-true)
  (setq org-roam-capture-templates
        '(("d" "dots" plain "%?"
           :target (file+head
                    "dots/%<%Y%m%d%H%M%S>.org"
                    "#+title: ${title}\n#+hugo_section: dots\n")
           :immediate-finish t
           :unnarrowed t)
          ("m" "maps" plain "%?"
           :target (file+head
                    "maps/%<%Y%m%d%H%M%S>.org"
                    "#+title: ${title}\n#+hugo_section: maps\n")
           :immediate-finish t
           :unnarrowed t)
          ("a" "articles" plain "%?"
           :target (file+head
                    "articles/%<%Y%m%d%H%M%S>.org"
                    "#+title: ${title}\n#+hugo_section: articles\n")
           :immediate-finish t
           :unnarrowed t))))

(use-package vertico
  :config (vertico-mode))

(use-package org
  :bind (("C-c p p" . org-toggle-inline-images))
  :config
  (setq org-image-actual-width 256))

(use-package company
  :hook (prog-mode . company-mode))

(use-package elisp-slime-nav
  :bind (("M-." . elisp-slime-nav-find-elisp-thing-at-point)
         ("M-," . pop-tag-mark)))


;;; requires
(require 'better-defaults)

;;; init.el ends here
