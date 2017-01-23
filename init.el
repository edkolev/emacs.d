
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(setq gc-cons-threshold (* 10 1024 1024))

;; settings
(setq custom-file (expand-file-name "custom-file.el" user-emacs-directory))
(setq inhibit-startup-screen t)
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq custom-safe-themes t)
(fset 'yes-or-no-p 'y-or-n-p)
;; initial *scratch* buffer
(setf initial-scratch-message ""
      initial-major-mode 'emacs-lisp-mode)


(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(global-set-key "\C-ch" help-map)

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

(defvar default-tags-table-function '(lambda () (expand-file-name ".git/etags" "/usr/local/")))

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)

;; theme packages
(use-package spacemacs-theme :ensure t :defer t)
(use-package molokai-theme :ensure t :defer t)
; (use-package aurora-theme :ensure t :defer t)
(use-package color-theme-sanityinc-tomorrow :ensure t :defer t)
(use-package gruvbox-theme :ensure t :defer t)
(use-package material-theme :ensure t :defer t)
(use-package molokai-theme :ensure t :defer t)
(use-package monokai-theme :ensure t :defer t)
(use-package zenburn-theme :ensure t :defer t)
(use-package darktooth-theme :ensure t :defer t)
(use-package seoul256-theme :ensure t :defer t)

;; dark variants  Range:   233 (darkest) ~ 239 (lightest) ;; Default: 237
;; light variants Range:   252 (darkest) ~ 256 (lightest) ;; Default: 253
(setq seoul256-background 253)
(load-theme 'seoul256 t)

;; packages
(use-package general ;; https://gitlab.com/KNX32542/dotfiles/blob/master/emacs/.emacs.d/init.el
  :ensure t
  :config
  (general-evil-setup))

(setq evil-search-module 'isearch ;; 'evil-search 
      ;; don't let modes override the INSERT state
      evil-overriding-maps nil
      evil-intercept-maps nil)

(use-package evil
  :ensure t
  :config
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode)

  (general-nmap "RET" 'save-buffer)
  (general-nmap "[ Q" 'first-error)
  (general-nmap "] q" 'next-error)
  (general-nmap "[ q" 'previous-error)
  (general-nmap "C-u" 'evil-scroll-up)
  (general-nmap ", w" 'evil-window-vsplit)
   
  (general-nmap "g SPC" 'find-file-in-project)
  (general-nmap "C-c C-b" 'ido-switch-buffer)

  (general-nmap "] SPC" (lambda (count) (interactive "p") (dotimes (_ count) (save-excursion (evil-insert-newline-below)))))
  (general-nmap "[ SPC" (lambda (count) (interactive "p") (dotimes (_ count) (save-excursion (evil-insert-newline-above)))))

  (general-nmap "C-p" 'beginning-of-defun)
  (general-nmap "C-n" 'end-of-defun)
  (general-nmap "[ m" 'beginning-of-defun)
  (general-nmap "] m" 'end-of-defun)

  (general-nmap "C-W C-]" 'xref-find-definitions)
  (general-nmap "C-]" 'xref-find-definitions-other-window)

  ;; navigate b/w emacs windows and tmux panes
  (defun evgeni-window-navigate (emacs-cmd tmux-cmd)
    (condition-case nil
  (funcall emacs-cmd)
      (error (if (getenv "TMUX") (shell-command-to-string tmux-cmd)))))
  (general-nmap "C-h" (lambda () (interactive) (evgeni-window-navigate 'windmove-left "tmux select-pane -L")))
  (general-nmap "C-j" (lambda () (interactive) (evgeni-window-navigate 'windmove-down "tmux select-pane -D")))
  (general-nmap "C-k" (lambda () (interactive) (evgeni-window-navigate 'windmove-up "tmux select-pane -U")))
  (general-nmap "C-l" (lambda () (interactive) (evgeni-window-navigate 'windmove-right "tmux select-pane -R")))

  ;; insert state
  (general-imap "C-e" 'end-of-line)
  (general-imap "C-a" 'beginning-of-line-text)
  (general-imap "C-u" (lambda () (interactive) (evil-delete (point-at-bol) (point))))

  ;; function text object
  (evil-define-text-object evgeni-inner-defun (count &optional beg end type)
    (save-excursion
      (mark-defun)
      (evil-range (region-beginning) (region-end) type :expanded t)))
  (define-key evil-inner-text-objects-map "m" 'evgeni-inner-defun)
  (define-key evil-outer-text-objects-map "m" 'evgeni-inner-defun)

  ;; toggles
  (general-nmap "C-c o c" 'hl-line-mode)
  )

(use-package ace-window
  :ensure t
  :general
  (general-nmap "C-w C-w" 'ace-window))

(use-package avy
  :ensure t
  :general
  (general-nmap "C-c C-g" 'avy-goto-word-or-subword-1))

(use-package evil-surround
  :ensure t
  :config (global-evil-surround-mode))

(use-package evil-commentary
  :ensure t
  :config (evil-commentary-mode))

(use-package evil-exchange
  :ensure t
  :config (evil-exchange-cx-install))

(use-package evil-replace-with-register
  :ensure t
  :config
  (setq evil-replace-with-register-key (kbd "gr"))
  (evil-replace-with-register-install))

(use-package evil-anzu
  :config (global-anzu-mode))

(use-package recentf
  :config
  (setq recentf-max-saved-items 50)
  :init
  (recentf-mode)
  :general
  (general-nmap "C-c C-r" '(lambda ()
           (interactive)
           (find-file (ido-completing-read "Find recent file: " recentf-list)))))

(use-package evil-indent-plus ;; indent object
  :ensure t
  :config (evil-indent-plus-default-bindings))

(use-package paren
  :config (show-paren-mode))

(use-package saveplace
  :config (save-place-mode))

(use-package dired
  :general
  (general-nmap "-"   (lambda () (interactive) (dired ".")))
  :config
  (define-key dired-mode-map (kbd "-") 'dired-up-directory))

(use-package magit
  :ensure t
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1) ;; use current window fullscreen
  :general
  (general-nmap "U U" 'magit-status)
  ;; :init
  ; (with-eval-after-load 'magit
  ;   (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-unstaged-changes nil t)
  ;   (remove-hook 'magit-status-sections-hook 'magit-insert-stash-index)
  ;   )
  ; ;; TODO - 1. dont show stash; 2. show untracked files after unstaged
  ; ;; (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-status-headers nil t)
  )

(use-package flycheck
  :ensure t
  :config
  ;; (flycheck-mode t)
  (setq-default flycheck-disabled-checkers '(perl-perlcritic)))

(use-package company
  :ensure t
  :defer 1
  :init
  (setf company-idle-delay 0
        company-minimum-prefix-length 2
        company-show-numbers t
        company-selection-wrap-around t
        company-backends (list #'company-capf
                               (list #'company-dabbrev-code
                                     #'company-keywords)
                               #'company-files
                               #'company-dabbrev))
  :config
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map (kbd "RET") nil)
  (global-company-mode t))

(use-package company-dabbrev
  :init
  (setf company-dabbrev-ignore-case 'keep-prefix
        company-dabbrev-ignore-invisible t
        company-dabbrev-downcase nil))

(use-package flx-ido
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  (setq ido-use-faces nil))

(use-package idomenu
  :ensure t
  :general
  (general-nmap ", f" 'idomenu))

(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode t))

(use-package org
  :defer t
  :ensure t)

(use-package move-text
  :ensure t
  :general
  (general-nmap "] e" (lambda (arg) (interactive "*p") (move-text-down arg)))
  (general-nmap "[ e" (lambda (arg) (interactive "*p") (move-text-up arg))))

;; elisp
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (modify-syntax-entry ?- "w")
                                  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)))

(add-hook 'lisp-interaction-mode-hook (lambda ()
                                        (define-key lisp-interaction-mode-map (kbd "C-c C-c") 'eval-defun)))

;; perl
(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook (lambda ()
                             (set-face-background 'cperl-hash-face nil)
                             (set-face-foreground 'cperl-hash-face nil)
                             (set-face-background 'cperl-array-face nil)
                             (set-face-foreground 'cperl-array-face nil)
                             (setq cperl-invalid-face nil) ;; extra whitespace TODO show this in normal mode only
                             ))

;; TODO
; (setq ffip-prefer-ido-mode t)

