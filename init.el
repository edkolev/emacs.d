
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(setq gc-cons-threshold (* 10 1024 1024))

;; settings
(setq custom-file (expand-file-name "custom-file.el" user-emacs-directory))
(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(fset 'yes-or-no-p 'y-or-n-p)

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

;; packages
(use-package general ;; https://gitlab.com/KNX32542/dotfiles/blob/master/emacs/.emacs.d/init.el
  :ensure t
  :config
  (general-evil-setup))

(setq evil-search-module 'evil-search 
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
  (general-nmap "] e" (lambda (arg) (interactive "*p") (move-text-down arg)))
  (general-nmap "[ e" (lambda (arg) (interactive "*p") (move-text-up arg)))
  (general-nmap "-"   (lambda () (interactive) (dired ".")))
  (general-nmap ", w" 'evil-window-vsplit)
   
  (general-nmap "g SPC" 'find-file-in-project)
  (general-nmap "C-c C-b" 'list-buffers)

  ;; TODO
  ;; [ SPC
  ;; ] SPC

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
  (general-imap "TAB" 'hippie-expand)
  (general-imap "C-e" 'end-of-line)
  (general-imap "C-a" 'beginning-of-line-text)
  (general-imap "C-u" (lambda () (interactive) (evil-delete (point-at-bol) (point))))

  ;; toggles
  (general-nmap "C-c o c" 'hl-line-mode)
  )


(use-package imenu-anywhere
  :ensure t
  :general
  (general-nmap ", f" 'ido-imenu-anywhere))

(use-package ace-window
  :ensure t
  :general
  (general-nmap "C-w C-w" 'ace-window))

(use-package avy
  :ensure t
  :general
  (general-nmap "C-c C-c" 'avy-goto-word-or-subword-1))

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
(use-package evil-indent-plus
  :ensure t
  :config (evil-indent-plus-default-bindings))

(use-package paren
  :config (show-paren-mode))

(use-package saveplace
  :config (save-place-mode))

(use-package dired
  :config
  (define-key dired-mode-map (kbd "-") 'dired-up-directory))

(use-package magit
  :ensure t
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1) ;; use current window
  :general
  (general-nmap "U U" 'magit-status))

(use-package evil-magit
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  ;; (flycheck-mode t)
  (setq-default flycheck-disabled-checkers '(perl-perlcritic)))

(use-package company
  :ensure t
  :config
  (global-company-mode 1)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (setq company-idle-delay 0.5))
  ; (add-hook 'evil-insert-state-exit-hook 'company-abort)

(use-package flx-ido
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  (setq ido-use-faces nil))

;; elisp
(add-hook 'emacs-lisp-mode-hook #'(lambda () (modify-syntax-entry ?- "w")))

;; perl
(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook (lambda ()
			     (general-nmap "C-p" 'perl-beginning-of-function)
			     (general-nmap "C-n" 'perl-end-of-function)
			     (set-face-background 'cperl-hash-face nil)
			     (set-face-foreground 'cperl-hash-face nil)
			     (set-face-background 'cperl-array-face nil)
			     (set-face-foreground 'cperl-array-face nil)
			     (setq cperl-invalid-face nil) ;; extra whitespace TODO show this in normal mode only
			     ))

;; TODO
;; ESC to NORMAL even from emacs state
;; TODO (define-key evil-emacs-state-map [escape] 'evil-normal-state)
; (setq ffip-prefer-ido-mode t)

