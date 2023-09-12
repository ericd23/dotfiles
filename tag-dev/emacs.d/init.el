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

;; cleaner UI
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq inhibit-startup-echo-area-message (user-login-name))
(setq frame-resize-pixelwise t)
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq default-frame-alist '((vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
			    (font . "Fira Code 12")))

(setq native-comp-async-report-warnings-errors 'silent)

;; Fix archaic defaults
(setq sentence-end-double-space nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq require-final-newline t)

(setq-default show-trailing-whitespace t)

(setq vc-follow-symlinks t)

;; Automatically reread from disk
(setq auto-revert-interval 1)
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Save minibuffer history
(savehist-mode)

;; Move through windows with Ctrl + arrow
(windmove-default-keybindings 'control)

;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setq make-backup-file-name-function 'bedrock--backup-file-name)

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil
	evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package corfu
  :ensure t
  :config
  (global-corfu-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package projectile
  :ensure t
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-project-search-path '("~/src/")))


(electric-pair-mode 1)
(display-line-numbers-mode 1)
(pixel-scroll-precision-mode 1)
(global-display-line-numbers-mode 1)
