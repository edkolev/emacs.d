
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq gc-cons-threshold (* 10 1024 1024))

;; settings
(setq custom-file (expand-file-name "custom-file.el" user-emacs-directory))
(setq inhibit-startup-screen t)
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq custom-safe-themes t)
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
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

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
  (setq evil-search-module 'isearch)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  :config
  (evil-mode)

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
  (general-nmap "C-h" (lambda! (evgeni-window-navigate 'windmove-left "tmux select-pane -L")))
  (general-nmap "C-j" (lambda! (evgeni-window-navigate 'windmove-down "tmux select-pane -D")))
  (general-nmap "C-k" (lambda! (evgeni-window-navigate 'windmove-up "tmux select-pane -U")))
  (general-nmap "C-l" (lambda! (evgeni-window-navigate 'windmove-right "tmux select-pane -R")))

  ;; insert state
  (general-imap "C-e" 'end-of-line)
  (general-imap "C-a" 'beginning-of-line-text)
  (general-imap "C-u" (lambda () (interactive) (evil-delete (point-at-bol) (point))))
  (general-imap "C-x s" 'complete-symbol)

  ;; expand lines
  (defun evgeni-hippie-expand-lines ()
    (interactive)
    (let ((hippie-expand-try-functions-list
           '(try-expand-line try-expand-line-all-buffers)))
      (end-of-line)
      (hippie-expand nil)))
  (general-imap "C-l" 'evgeni-hippie-expand-lines)
  (general-imap "C-x C-l" 'evgeni-hippie-expand-lines)

  (general-imap "C-k" 'completion-at-point)
  ;; (global-set-key (kbd "C-l") 'completion-at-point)

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
  (define-key dired-mode-map (kbd "-") 'dired-up-directory))

(use-package magit
  :ensure t
  :general
  (general-nmap "U U" '(magit-status :which-key "git status"))
  (general-nmap "U s" '(magit-stage-file :which-key "git stage"))
  (general-nmap "U d" '(magit-diff-unstaged :which-key "git diff"))
  (general-nmap "U l" '(magit-log-head :which-key "git diff"))
  :config
  (remove-hook 'magit-status-sections-hook 'magit-insert-stashes) ;; don't show stashes
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-untracked-files 'magit-insert-staged-changes 1) ;; lower untracked

  (define-key magit-mode-map "e" 'vdiff-magit-dwim)
  (define-key magit-mode-map "E" 'vdiff-magit-popup)
  )

(use-package vdiff
  :ensure t
  :commands (vdiff-files
             vdiff-files3
             vdiff-buffers
             vdiff-buffers3
             vdiff-magit-dwim
             vdiff-magit-popup))


(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :general
  (general-nmap "C-c o f" 'flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (setq-default flycheck-disabled-checkers '(perl-perlcritic)))

(use-package company
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
  :after company
  :init
  (setf
   company-dabbrev-ignore-case nil
   company-dabbrev-ignore-invisible t
   company-dabbrev-downcase nil))

(use-package company-dabbrev-code
  :after company
  :config
  (setq company-dabbrev-code-other-buffers t)
  (setq company-dabbrev-code-ignore-case t))

(use-package company-keywords
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

(use-package flx-ido
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(use-package idomenu
  :ensure t
  :commands (idomenu))

(use-package ido-vertical-mode
  :ensure t
  :config
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (ido-vertical-mode t))

(use-package org
  ;; https://github.com/gjstein/emacs.d/blob/cb126260d30246dc832d6e456b06676f517b35b0/config/init-31-doc-org.el#L50-L77
  ;; https://github.com/bbatsov/prelude/blob/e0ca7c700389e70df457177ea75b0936dbe254e0/modules/prelude-evil.el#L130
  :disabled t
  :ensure t)

(use-package move-text
  :ensure t
  :general
  (general-nmap "] e" (lambda (arg) (interactive "*p") (move-text-down arg)))
  (general-nmap "[ e" (lambda (arg) (interactive "*p") (move-text-up arg))))

(use-package xref
  :general
  (general-nmap "C-]" 'xref-find-definitions)
  (general-nmap "C-W C-]" 'xref-find-definitions-other-window)
  (general-evil-define-key 'normal xref--xref-buffer-mode-map "q" 'delete-window)
  (general-evil-define-key 'normal xref--xref-buffer-mode-map "C-n" 'xref-next-line)
  (general-evil-define-key 'normal xref--xref-buffer-mode-map "C-p" 'xref-prev-line))

(use-package abbrev
  :config
  (setq save-abbrevs 'silently)
  (setq abbrev-file-name (expand-file-name "abbrev.el" user-emacs-directory))
  (setq-default abbrev-mode t)
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

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :ensure t
  :init
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
  :init
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

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

(use-package imenu-anywhere
  :general
  (general-nmap ", i" 'ido-imenu-anywhere)
  (general-nmap ", f" 'ido-imenu-anywhere)
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.4)
  (which-key-mode))

(use-package ivy
  :ensure t
  :demand
  :general
  (general-nmap "SPC" 'ivy-switch-buffer)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
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

  (defun evgeni-perl-dump ()
    (interactive)
    (let ((word (thing-at-point 'word)))
      (save-excursion (end-of-line)
                      (newline-and-indent)
                      (insert (concat "use Data::Dump qw(pp); warn '" word ": ' . pp($" word ") . \"\\n\";")))))

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
  (general-nmap "z u" 'winner-undo)
  (general-nmap "z C-r" 'winner-redo)
  (ex! "winner-undo" 'winner-undo))

(use-package undo-tree
  :ensure t
  :config
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
        perl-indent-continued-arguments nil
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
