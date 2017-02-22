
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq gc-cons-threshold (* 10 1024 1024))

;; settings
(when (display-graphic-p)
  (set-default-font "Source Code Pro 13"))
(setq custom-file (expand-file-name "custom-file.el" user-emacs-directory))
(setq inhibit-startup-screen t)
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq custom-safe-themes t)
(setq suggest-key-bindings nil)
(setq-default truncate-lines t)
(fset 'yes-or-no-p 'y-or-n-p)
(setf initial-scratch-message ""
      initial-major-mode 'emacs-lisp-mode)

(defun display-startup-echo-area-message ()
  (message ""))
(setq load-prefer-newer t)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(global-set-key "\C-ch" help-map)

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-all-abbrevs
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

(modify-syntax-entry ?_ "w" (standard-syntax-table))

(defvar default-tags-table-function '(lambda () (expand-file-name ".git/etags" "/usr/local/")))

(defmacro lambda! (&rest body)
  "Shortcut for interactive lambdas"
  `(lambda () (interactive) ,@body))

(defmacro ex! (cmd func)
  "Shortcut for defining ex commands"
  `(with-eval-after-load 'evil
     (evil-ex-define-cmd ,cmd ,func)))

;; use-package
(setq use-package-enable-imenu-support t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)

;; theme packages
(use-package spacemacs-theme :ensure t :defer t)
(use-package molokai-theme :ensure t :defer t)
(use-package color-theme-sanityinc-tomorrow :ensure t :defer t)
(use-package gruvbox-theme :ensure t :defer t)
(use-package darktooth-theme :ensure t :defer t)
(use-package seoul256-theme :ensure t :defer t)
(use-package doom-themes :ensure t :defer t)
(use-package ujelly-theme :ensure t :defer t)
(use-package zenburn-theme :ensure t :defer t)
(use-package eclipse-theme :ensure t :defer t)
(use-package flatui-theme :ensure t :defer t)
(use-package plan9-theme :ensure t :defer t)
(use-package twilight-bright-theme :ensure t :defer t)
(use-package espresso-theme :ensure t :defer t)
(use-package soft-stone-theme :ensure t :defer t)
(use-package flatui-theme :ensure t :defer t)
(use-package faff-theme :ensure t :defer t)

(set-face-bold-p 'bold nil) ;; disable bold
(load-theme 'leuven)

;; packages
(use-package general ;; https://gitlab.com/KNX32542/dotfiles/blob/master/emacs/.emacs.d/init.el
  :ensure t
  :config
  (general-evil-setup))

(use-package evil
  :ensure t
  :init
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  :config
  (evil-mode)

  ;; auto-clear highlight
  (defun evgeni-nohighlight-hook (&rest _)
    (remove-hook 'pre-command-hook 'evgeni-nohighlight-hook 'local)
    (evil-ex-nohighlight))
  (defun evgeni-add-nohighlight-hook (&rest _)
    (add-hook 'pre-command-hook 'evgeni-nohighlight-hook nil 'local))
  (dolist (f '(evil-ex-search-backward
               evil-ex-search-forward
               evil-ex-search-next
               evil-ex-search-previous
               evil-ex-search-word-backward
               evil-ex-search-word-forward))
    (advice-add f :after #'evgeni-add-nohighlight-hook))

  (general-nmap "RET" 'save-buffer)
  (general-nmap "[ Q" 'first-error)
  (general-nmap "] q" 'next-error)
  (general-nmap "[ q" 'previous-error)
  (general-nmap "C-u" 'evil-scroll-up)
  (general-nmap ", w" 'evil-window-vsplit)

  (general-nmap "C-c C-b" 'ido-switch-buffer)

  (general-nmap "] SPC" (lambda (count) (interactive "p") (dotimes (_ count) (save-excursion (evil-insert-newline-below)))))
  (general-nmap "[ SPC" (lambda (count) (interactive "p") (dotimes (_ count) (save-excursion (evil-insert-newline-above)))))

  (general-nmap "C-p" 'beginning-of-defun)
  (general-nmap "C-n" 'end-of-defun)
  (general-nmap "[ m" 'beginning-of-defun)
  (general-nmap "] m" 'end-of-defun)
  (general-nmap "0" 'evil-first-non-blank)

  ;; alias :On with :on
  (evil-ex-define-cmd "On" "on")

  ;; :source
  (evil-ex-define-cmd "so[urce]" 'evgeni-source)
  (evil-define-command evgeni-source (&optional file)
    (interactive "<f>")
    (if file
        (load-file file)
      (load-file (concat user-emacs-directory "init.el"))))

  ;; :colorscheme
  (evil-ex-define-cmd "colo[rscheme]" 'evgeni-colorscheme)
  (evil-define-command evgeni-colorscheme (&optional theme)
    :repeat nil
    (interactive "<a>")
    (if theme
        (load-theme (intern theme) t)
      (call-interactively 'load-theme)))

  ;; :tyank & :tput
  (when (getenv "TMUX")
    (evil-define-command evgeni-tyank (begin end)
      (interactive "<r>")
      (shell-command (concat "tmux set-buffer " (shell-quote-argument (buffer-substring begin end))))
      )
    (evil-ex-define-cmd "tput" (lambda () (interactive)
                                 (save-excursion
                                   (end-of-line)
                                   (newline)
                                   (insert (shell-command-to-string "tmux show-buffer")))))
    (evil-ex-define-cmd "tyank" 'evgeni-tyank))

  (general-nmap "Y" (lambda () (interactive) (evil-yank (point) (point-at-eol))))

  ;; navigate b/w emacs windows and tmux panes
  (defun evgeni-window-navigate (emacs-cmd tmux-cmd)
    (condition-case nil
        (funcall emacs-cmd)
      (error (if (getenv "TMUX") (shell-command-to-string tmux-cmd)))))
  (general-mmap "C-h" (lambda! (evgeni-window-navigate 'windmove-left "tmux select-pane -L")))
  (general-mmap "C-j" (lambda! (evgeni-window-navigate 'windmove-down "tmux select-pane -D")))
  (general-mmap "C-k" (lambda! (evgeni-window-navigate 'windmove-up "tmux select-pane -U")))
  (general-mmap "C-l" (lambda! (evgeni-window-navigate 'windmove-right "tmux select-pane -R")))

  ;; insert state
  (general-imap "C-e" 'end-of-line)
  (general-imap "C-a" 'beginning-of-line-text)
  (general-imap "C-u" (lambda () (interactive) (evil-delete (point-at-bol) (point))))
  (general-imap "C-x s" 'complete-symbol)

  ;; expand lines
  (general-imap "C-l" 'evil-complete-next-line)
  (general-imap "C-x C-l" 'evil-complete-next-line)

  ;; completion
  (general-imap "TAB" 'hippie-expand)
  (general-imap "<backtab>" 'completion-at-point)
  (general-imap "C-x C-x" 'completion-at-poin)
  (general-imap "C-k" 'completion-at-point)
  (general-imap "C-x C-]" 'complete-tag)
  (general-imap "C-]" 'complete-tag)

  ;; complete file paths
  (general-imap "C-x C-f" (lambda () (interactive)
                            (let ((hippie-expand-try-functions-list '(try-complete-file-name try-complete-file-name-partially)))
                              (hippie-expand nil))
                            ))

  ;; C-d to either shift line or delete char
  (general-imap "C-d" (lambda () (interactive)
                        (if (eq (point) (point-at-eol))
                            (evil-shift-left-line 1)
                          (delete-char 1))))

  ;; function text object
  (evil-define-text-object evgeni-inner-defun (count &optional beg end type)
    (save-excursion
      (mark-defun)
      (evil-range (region-beginning) (region-end) type :expanded t)))

  (define-key evil-inner-text-objects-map "m" 'evgeni-inner-defun)
  (define-key evil-outer-text-objects-map "m" 'evgeni-inner-defun)

  ;; 'entire' text object
  (evil-define-text-object evgeni-entire-text-object (count &optional beg end type)
    (save-excursion
      (mark-whole-buffer)
      (exchange-dot-and-mark)
      (evil-range (region-beginning) (region-end) type :expanded t)))

  (define-key evil-inner-text-objects-map "e" 'evgeni-entire-text-object)
  (define-key evil-outer-text-objects-map "e" 'evgeni-entire-text-object)

  ;; toggles
  (general-nmap "C-c o c" 'hl-line-mode)
  (general-nmap "C-c o w" 'toggle-truncate-lines)

  ;; clean whitespace on lines with nothing but whitespace
  (add-hook 'evil-insert-state-exit-hook 'evgeni-clean-whitespace-line)
  (defun evgeni-clean-whitespace-line ()
    (when (string-match "^\s+$" (thing-at-point 'line))
      (delete-region (line-beginning-position) (line-end-position))))

  ;; cursor in terminal
  (cond
   ((getenv "TMUX")
    (add-hook 'evil-insert-state-entry-hook (lambda () (send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=1\x7\e\\")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=0\x7\e\\"))))
   ((not (display-graphic-p))
    (add-hook 'evil-insert-state-entry-hook (lambda () (send-string-to-terminal "\e]50;CursorShape=1\x7")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (send-string-to-terminal "\e]50;CursorShape=0\x7"))))))

(use-package ace-window
  :ensure t
  :general
  (general-nmap "C-w C-w" 'ace-window))

(use-package avy
  :ensure t
  :general
  (general-nmap "C-c C-g" 'avy-goto-word-or-subword-1))

(use-package ivy-hydra
  :ensure t
  :defer t)

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

(use-package evil-visualstar
  :ensure t
  :config
  (global-evil-visualstar-mode))

(use-package evil-magit
  :ensure t
  :after magit
  :init
  (setq evil-magit-want-horizontal-movement t))

(use-package recentf
  :init
  ;; (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")
  (setq recentf-save-file (expand-file-name "recentf.el" user-emacs-directory))
  (setq recentf-max-saved-items 50)
  (setq recentf-exclude '("/tmp/" "/ssh:"))
  (recentf-mode))

(use-package evil-indent-plus ;; indent object
  :ensure t
  :config (evil-indent-plus-default-bindings))

(use-package paren
  :config (show-paren-mode))

(use-package saveplace
  :config
  (save-place-mode)
  (setq save-place-file (expand-file-name "saveplace.el" user-emacs-directory))
  )

(use-package dired
  :general
  (general-nmap "-"   (lambda () (interactive) (dired ".")))
  :config
  (setq dired-listing-switches "-alh")
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (define-key dired-mode-map (kbd "-") 'dired-up-directory))

(use-package magit
  :ensure t
  :general
  (general-nmap "U U" '(magit-status :which-key "git status"))
  (general-nmap "U w" '(magit-stage-file :which-key "git stage file"))
  (general-nmap "U d" '(magit-diff-unstaged :which-key "git diff"))
  (general-nmap "U l" '(magit-log-head :which-key "git diff"))
  (general-nmap "U r" '(magit-file-checkout :which-key "git checkout file"))
  :config
  (remove-hook 'magit-status-sections-hook 'magit-insert-stashes) ;; don't show stashes
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-untracked-files 'magit-insert-staged-changes 1) ;; lower untracked

  (define-key magit-mode-map "e" 'vdiff-magit-dwim)
  (define-key magit-mode-map "E" 'vdiff-magit-popup)
  (unbind-key "C-h" magit-mode-map)
  (unbind-key "C-j" magit-mode-map)
  (unbind-key "C-k" magit-mode-map)
  (unbind-key "C-l" magit-mode-map)
  (general-evil-define-key 'normal magit-mode-map "C-n" 'magit-section-forward)
  (general-evil-define-key 'normal magit-mode-map "C-p" 'magit-section-backward))

(use-package vdiff
  :ensure t
  :commands (vdiff-files
             vdiff-files3
             vdiff-buffers
             vdiff-buffers3
             vdiff-magit-dwim
             vdiff-magit-popup)
  :general
  (general-nmap "U D" '(vdiff-magit-popup :which-key "vdiff popup"))
  :config
  (define-key vdiff-mode-map (kbd "] c") 'vdiff-next-hunk)
  (define-key vdiff-mode-map (kbd "[ c") 'vdiff-previous-hunk)
  (define-key vdiff-mode-map (kbd "zc") 'vdiff-close-fold)
  (define-key vdiff-mode-map (kbd "zM") 'vdiff-close-all-folds)
  (define-key vdiff-mode-map (kbd "zo") 'vdiff-open-fold)
  (define-key vdiff-mode-map (kbd "zR") 'vdiff-open-all-folds)
  (define-key vdiff-mode-map (kbd "C-c o SPC") 'vdiff-toggle-whitespace)

  (define-key vdiff-mode-map (kbd "do") 'vdiff-receive-changes)
  (define-key vdiff-mode-map (kbd "dp") 'vdiff-send-changes)
  (define-key vdiff-mode-map (kbd "C-c r") 'vdiff-receive-changes)
  (define-key vdiff-mode-map (kbd "C-c s") 'vdiff-send-changes)
  (evil-define-key 'normal vdiff-mode-map "d o" 'vdiff-receive-changes)
  (evil-define-key 'normal vdiff-mode-map "d p" 'vdiff-send-changes)


  (define-key vdiff-mode-map (kbd "q" ) 'vdiff-quit)
  (evil-define-key 'normal vdiff-mode-map "q" 'vdiff-quit))

(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :general
  (general-nmap "C-c o f" 'flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (setq-default flycheck-disabled-checkers '(perl-perlcritic)))

(use-package company
  :disabled t
  :ensure t
  :commands company-mode
  :general
  (general-imap "C-x C-f" 'company-files)
  (general-imap "C-x C-]" 'company-etags)
  (general-imap "C-]" 'company-etags)
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  (setf company-idle-delay 0.3

        company-minimum-prefix-length 2
        company-show-numbers t
        company-selection-wrap-around t
        company-quickhelp-delay nil
        company-tooltip-limit 10
        company-tooltip-align-annotations t
        company-require-match 'never

        company-frontends
        '(company-pseudo-tooltip-unless-just-one-frontend
          company-preview-frontend
          company-echo-metadata-frontend)

        company-backends (list (list #'company-capf
                                     ;; #'company-dabbrev-code
                                     #'company-dabbrev
                                     #'company-keywords)
                               (list #'company-dabbrev-code
                                     #'company-keywords)
                               #'company-files
                               #'company-dabbrev)


        )
  :config

  (defadvice company-pseudo-tooltip-unless-just-one-frontend
      (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p)
      ad-do-it))

  (unbind-key "C-w" company-active-map)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (general-imap "TAB" (lambda () (interactive)
                        (if (looking-at "\\_>")
                            (company-complete-common)
                          (indent-according-to-mode))))
  (global-company-mode t))

(use-package company-dabbrev
  :disabled t
  :after company
  :init
  (setf
   company-dabbrev-ignore-case nil
   company-dabbrev-ignore-invisible t
   company-dabbrev-downcase nil))

(use-package company-dabbrev-code
  :disabled t
  :after company
  :config
  (setq company-dabbrev-code-other-buffers t)
  (setq company-dabbrev-code-ignore-case t))

(use-package company-keywords
  :disabled t
  :after company
  :config
  (add-to-list
   'company-keywords-alist
   '(haskell-mode
     "undefined" "as" "case" "ccall" "class" "contained" "data" "default"
     "deriving" "do" "else" "error" "export" "family" "foreign" "hiding"
     "if" "import" "in" "infix" "infixl" "infixr" "instance" "let"
     "module" "newtype" "of" "qualified" "safe" "stdcall" "then" "trace"
     "type" "undefined" "unsafe" "where"
     )))

(use-package idomenu
  :ensure t
  :commands (idomenu))

(use-package ido-vertical-mode
  :ensure t
  :config
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (ido-vertical-mode t))

(use-package org
  :mode (("\\.org$"   . org-mode))
  :defer t
  :ensure t)

(use-package org-bullets
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package move-text
  :ensure t
  :general
  (general-nmap "] e" (lambda (arg) (interactive "*p") (move-text-down arg)))
  (general-nmap "[ e" (lambda (arg) (interactive "*p") (move-text-up arg))))

(use-package xref
  :general
  (general-nmap "C-]" 'xref-find-definitions)
  (general-nmap "C-w C-]" 'xref-find-definitions-other-window)
  (general-evil-define-key 'normal xref--xref-buffer-mode-map "q" 'delete-window)
  (general-evil-define-key 'normal xref--xref-buffer-mode-map "C-n" 'xref-next-line)
  (general-evil-define-key 'normal xref--xref-buffer-mode-map "C-p" 'xref-prev-line))

(use-package abbrev
  :diminish 'abbrev-mode
  :config
  (setq abbrev-file-name (expand-file-name "abbrev.el" user-emacs-directory))
  (setq-default abbrev-mode t)
  (add-hook 'evil-insert-state-exit-hook 'expand-abbrev)
  )

(use-package narrow
  :commands (narrow-to-region narrow-to-defun widen)
  :init
  (evil-define-command evgeni-narrow-or-widen (begin end)
    (interactive "<r>")
    (cond ((region-active-p) (narrow-to-region begin end))
          ((buffer-narrowed-p) (widen)) ;; buffer-base-buffer
          (t (narrow-to-defun))))
  (ex! "narrow" 'evgeni-narrow-or-widen))

(use-package grep
  :commands grep-mode
  :config
  (unbind-key "h" grep-mode-map)
  (unbind-key "n" grep-mode-map)
  ;; TODO these aren't working
  ;; (define-key grep-mode-map "C-p" 'previous-error-no-select)
  ;; (define-key grep-mode-map "C-n" 'next-error-no-select)
  )

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :init
  (setq wgrep-auto-save-buffer t)
  (ex! "wgrep" 'wgrep-change-to-wgrep-mode)
  (ex! "wgrep-finish" 'wgrep-finish-edit))

(use-package linum
  :general
  (general-nmap "C-c o n" 'linum-mode)
  :config
  (setq linum-format "%d "))

(use-package compile
  :config

  (defun evgeni-project-root ()
    (if (project-current)
        (cdr (project-current))
      default-directory
      ))

  (defun evgeni-grep-command ()
    (if (executable-find "ag")
        "ag --case-sensitive --nogroup"
      "grep -R -F -e"))

  (defun evgeni-compilation-start-in-project-root (mode command &rest arguments)
    (let* ((default-directory (evgeni-project-root))
           (command-with-args (cons command arguments))
           (full-commad (mapconcat 'identity command-with-args " ")))
      (compilation-start full-commad mode)))

  (defun evgeni-grep (&rest args)
    (apply
     'evgeni-compilation-start-in-project-root
     'grep-mode
     (evgeni-grep-command)
     args))

  (evil-ex-define-cmd "grep" 'evgeni-ex-grep)
  (evil-define-command evgeni-ex-grep (&rest args)
    (interactive "<f>")
    (if args
        (apply 'evgeni-grep args)
      (message "What do you want to search for?")))

  (defun evgeni-scroll-grep-win (buffer string)
    (when (and
           (eq major-mode 'grep-mode)
           (buffer-live-p buffer)
           (string-match "grep" (buffer-name buffer))
           (string-match "finished" string))
      (with-selected-window (get-buffer-window buffer)
        (goto-char (point-min))
        (scroll-up-line 4)

        (save-excursion
          (message "*grep* %d matches found" (- (count-lines (point-min) (point-max)) 6))
          ))))

  (add-hook 'compilation-finish-functions 'evgeni-scroll-grep-win)

  (general-nmap "K" (lambda () (interactive) (evgeni-grep (thing-at-point 'word)))))

(use-package smart-compile
  :ensure t
  :commands smart-compile
  :general
  (general-nmap "g m" 'smart-compile)
  (general-nmap "g RET" 'smart-compile)
  :config
  (setq compilation-read-command nil)
  (setq compilation-ask-about-save nil)
  (setq smart-compile-check-makefile nil)
  (setq smart-compile-alist '((cperl-mode . "perl -w -Mstrict -c %f")
                              (perl-mode . "perl -w -Mstrict -c %f")
                              (haskell-mode . "stack --silent ghc -- -e :q %f")
                              (emacs-lisp-mode    . (emacs-lisp-byte-compile))
                              ))

  (defun evgeni-close-compile-win-if-successful (buffer string)
    (if (and
         (eq major-mode 'compilation-mode)
         (buffer-live-p buffer)
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "warning" nil t))))
        (delete-windows-on buffer)
      (with-selected-window (get-buffer-window buffer)
        (goto-char (point-min))
        (scroll-up-line 4))))

  (add-hook 'compilation-finish-functions 'evgeni-close-compile-win-if-successful)

  (ex! "mak[e]" 'smart-compile)
  )

(use-package aggressive-indent
  :ensure t
  :commands aggressive-indent-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'perl-mode-hook 'aggressive-indent-mode)

  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'perl-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line)))))
  )

(use-package shackle
  :ensure t
  :config
  (shackle-mode 1)
  (setq shackle-rules
        '(
          ("*xref*"            :align below :size 0.4 :noselect t)
          ("*Help*"            :align below :size 16  :select t)
          ("*Backtrace*"       :align below :size 25  :noselect t)
          ("*Flycheck error messages*" :align below :size 0.25)
          ("*compilation*" :align below :size 10)
          ("*grep*" :align below :size 10)
          )))

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 1.5)
  (which-key-mode))

(use-package ivy
  :ensure t
  :diminish 'ivy-mode
  :demand
  :general
  (general-nmap "SPC" 'ivy-switch-buffer)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'full)
  (define-key ivy-minibuffer-map (kbd "C-w") 'backward-kill-word)
  (define-key ivy-minibuffer-map (kbd "C-u") (lambda () (interactive) (kill-region (point) (point-at-bol))))

  (define-key ivy-minibuffer-map (kbd "C-c C-c") 'ivy-restrict-to-matches))

(use-package swiper
  :ensure t
  :general
  (general-nmap ", s" 'swiper)
  (general-nmap ", /" 'swiper))

(use-package counsel
  :ensure t
  :general
  (general-nmap ", g" 'counsel-git-grep)
  (general-nmap ", l" 'counsel-git)
  (general-nmap "g SPC" ' counsel-git)
  (general-nmap ", i" 'counsel-imenu)
  (general-nmap ", f" 'counsel-imenu)
  )

(use-package elisp-mode ;; emacs-lisp-mode
  :config
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)

  (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
  (modify-syntax-entry ?. "w" emacs-lisp-mode-syntax-table)
  (modify-syntax-entry ?! "w" emacs-lisp-mode-syntax-table)

  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (electric-pair-mode))))

(use-package cperl-mode
  :disabled t
  :commands cperl-mode
  :load-path "extra-packages"
  :mode (("\\.pl$"   . cperl-mode)
         ("\\.pm$"   . cperl-mode)
         ("\\.t$"    . cperl-mode)
         ("\\.html$" . cperl-mode))
  :init
  (setq cperl-under-as-char t)
  :config
  (setq cperl-invalid-face nil
        cperl-indent-subs-specially nil
        cperl-indent-parens-as-block t
        cperl-indent-subs-specially nil
        cperl-close-paren-offset -3 ;; can't be set from .dir-locals.el?
        cperl-indent-level 2
        )

  (unbind-key "{" cperl-mode-map)
  (unbind-key "[" cperl-mode-map)
  (unbind-key "(" cperl-mode-map)
  (unbind-key "<" cperl-mode-map)
  (unbind-key "}" cperl-mode-map)
  (unbind-key "]" cperl-mode-map)
  (unbind-key ")" cperl-mode-map)
  (unbind-key ";" cperl-mode-map)
  (unbind-key ":" cperl-mode-map)
  (unbind-key "\C-j" cperl-mode-map)
  (unbind-key "TAB" cperl-mode-map)

  (add-hook 'cperl-mode-hook #'(lambda ()
                                 (general-define-key :keymaps 'local :states 'normal "] d" 'evgeni-perl-dump)
                                 (set-face-background 'cperl-hash-face nil)
                                 (set-face-foreground 'cperl-hash-face nil)
                                 (set-face-background 'cperl-array-face nil)
                                 (set-face-foreground 'cperl-array-face nil)
                                 (electric-pair-mode))))

(use-package intero
  :ensure t
  :commands intero-mode)

(use-package whitespace
  :commands whitespace-mode
  :config
  (setq whitespace-style '(face tabs trailing)))

(use-package winner
  :config
  (winner-mode)
  (general-mmap "z u" 'winner-undo)
  (general-mmap "z C-r" 'winner-redo)
  (ex! "winner-undo" 'winner-undo))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)
  (ex! "undo-tree" 'undo-tree-visualize)
  (setq undo-tree-history-directory-alist `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t))

(use-package perl-mode
  :commands perl-mode
  :config
  (setq perl-indent-level 3 ;; 4
        perl-continued-statement-offset 3 ;; t 4
        perl-continued-brace-offset -3 ;; -4
        perl-brace-offset 0
        perl-brace-imaginary-offset 0
        perl-label-offset -3 ;; -2
        perl-indent-continued-arguments 0 ;; nil
        perl-indent-parens-as-block t ;; nil
        perl-tab-always-indent tab-always-indent
        perl-tab-to-comment nil
        perl-nochange "\f")


  (defun evgeni-perl-dump ()
    (interactive)
    (let ((word (thing-at-point 'word)))
      (save-excursion (end-of-line)
                      (newline-and-indent)
                      (insert (concat "use Data::Dump qw(pp); warn '" word ": ' . pp($" word ") . \"\\n\";")))))


  (add-hook 'perl-mode-hook #'(lambda ()
                                (general-define-key :keymaps 'local :states 'normal "] d" 'evgeni-perl-dump)
                                (electric-pair-mode)
                                (modify-syntax-entry ?_ "w" perl-mode-syntax-table)
                                (setq defun-prompt-regexp ;; taken from cperl-mode
                                      "^[ 	]*\\(\\(?:sub\\)\\(\\([ 	\n]\\|#[^\n]*\n\\)+\\(::[a-zA-Z_0-9:']+\\|[a-zA-Z_'][a-zA-Z_0-9:']*\\)\\)\\([ 	\n]*\\(#[^\n]*\n[ 	\n]*\\)*\\(([^()]*)\\)\\)?\\([ 	\n]*\\(#[^\n]*\n[ 	\n]*\\)*\\(:\\([ 	\n]*\\(#[^\n]*\n[ 	\n]*\\)*\\(\\sw\\|_\\)+\\((\\(\\\\.\\|[^\\\\()]\\|([^\\\\()]*)\\)*)\\)?\\([ 	\n]*\\(#[^\n]*\n[ 	\n]*\\)*:\\)?\\)+\\)\\)?\\|\\(BEGIN\\|UNITCHECK\\|CHECK\\|INIT\\|END\\|AUTOLOAD\\|DESTROY\\)\\)[ 	\n]*\\(#[^\n]*\n[ 	\n]*\\)*")
                                )


            ) 
  )

(use-package eros
  :ensure t
  :general
  (general-define-key :keymap 'emacs-lisp-mode-map [remap eval-last-sexp] 'eros-eval-last-sexp)
  (general-define-key :keymap 'emacs-lisp-mode-map [remap eval-defun] 'eros-eval-defun))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package origami
  :ensure t
  :commands origami-mode)

(use-package haskell-mode
  :commands haskell-mode
  :config
  (define-abbrev-table 'haskell-mode-abbrev-table
    '(("undef" "undefined"))))

(use-package loccur
  :ensure t
  :commands (loccur-current loccur)
  :init
  (general-nmap ", *" 'loccur-current)
  (evil-ex-define-cmd "loccur" 'loccur))

