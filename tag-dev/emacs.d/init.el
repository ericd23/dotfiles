(defun ericd/make-elpa-files-read-only()
  (when (string-match-p
         (concat (regexp-quote (expand-file-name user-emacs-directory)) "elpa/.*") buffer-file-name)
    (setq buffer-read-only t)
    (message "File is now read-only.")))
(add-hook 'find-file-hook 'ericd/make-elpa-files-read-only)

(defun ericd/open-init-el ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))
(global-set-key (kbd "<f5>") 'ericd/open-init-el)

;; COPY: https://www.emacswiki.org/emacs/SmoothScrolling
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq inhibit-startup-echo-area-message "user")
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

(setq default-frame-alist '((font . "Fira Code 12")))

(setq native-comp-async-report-warnings-errors 'silent)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq make-backup-files nil
      auto-save-list-file-prefix nil)
(setq require-final-newline t)

(setq-default show-trailing-whitespace t)

(require 'package) ; needed for `package-archives'
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(setq ericd/package-list '(evil))

(dolist (package ericd/package-list)
  (unless (package-installed-p package)
    (package-install package)))

(evil-mode 1)
(electric-pair-mode 1)
