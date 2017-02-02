
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
(setq-default truncate-lines t)
(fset 'yes-or-no-p 'y-or-n-p)
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

(defmacro lambda! (&rest body)
  "Shortcut for interactive lambdas"
  `(lambda () (interactive) ,@body))

(defmacro ex! (cmd func)
  "Shortcut for defining ex commands"
  `(with-eval-after-load 'evil
     (evil-ex-define-cmd ,cmd ,func)))

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

;; dark variants  Range:   233 (darkest) ~ 239 (lightest) ;; Default: 237
;; light variants Range:   252 (darkest) ~ 256 (lightest) ;; Default: 253
;; (setq seoul256-background 253)
(set-face-bold-p 'bold nil) ;; disable bold
;; (load-theme 'seoul256 t)
(load-theme 'leuven)

;; (load-theme 'zenburn t)
;; (require 'color)

;; (let ((bg (face-attribute 'default :background)))
;;   (custom-set-faces
;;    `(company-preview-common ((t (:background ,(color-lighten-name bg 10)))))
;;    `(company-preview ((t (:background ,(color-lighten-name bg 5)))))))


;; (let ((bg (face-attribute 'default :background)))
;;   (custom-theme-set-faces
;;    'zenburn
;;    `(company-preview-common ((t (:background ,(color-lighten-name bg 10)))))
;;    `(company-preview ((t (:background ,(color-lighten-name bg 5)))))
;;    `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
;;    `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

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

  ;; function text object
  (evil-define-text-object evgeni-inner-defun (count &optional beg end type)
    (save-excursion
      (mark-defun)
      (evil-range (region-beginning) (region-end) type :expanded t)))
  (define-key evil-inner-text-objects-map "m" 'evgeni-inner-defun)
  (define-key evil-outer-text-objects-map "m" 'evgeni-inner-defun)

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
    (add-hook 'evil-insert-state-exit-hook  (lambda () (send-string-to-terminal "\e]50;CursorShape=0\x7")))))
  )

(use-package ace-window
  :ensure t
  :general
  (general-nmap "C-w C-w" 'ace-window))

(use-package avy
  :ensure t
  :general
  (general-nmap "C-c C-g" 'avy-goto-word-or-subword-1))

(use-package ivy
  :commands (ivy-completing-read)
  :ensure t)

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
  :defer 1
  :config
  (global-evil-visualstar-mode))

(use-package evil-magit
  :ensure t
  :after magit
  :init
  (setq evil-magit-want-horizontal-movement t))

(defun evgeni-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))
(use-package recentf
  :init
  (setq recentf-max-saved-items 50)
  (recentf-mode)

  (defun evgeni-find-buffer-or-file (file-or-buffer)
    (let* ((bufnames (mapcar #'buffer-name (buffer-list)))
           (is-buffer (member file-or-buffer bufnames)))
      (cond
       (is-buffer (switch-to-buffer file-or-buffer))
       (t (find-file file-or-buffer)))))

  :general
  (general-nmap "SPC" (lambda () (interactive) (evgeni-find-buffer-or-file
                                                (ivy-completing-read
                                                 "Find recent file: "
                                                 (append (evgeni-filter (lambda (bufname) (not (string-prefix-p " " bufname))) (mapcar #'buffer-name (buffer-list))) recentf-list)))))

  )

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
  :general
  (general-nmap "U U" 'magit-status)
  (general-nmap "U w" 'magit-stage-file)
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
  :general
  (general-nmap "C-c o f" 'flycheck-mode)
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
  (global-company-mode t)
  :general
  (general-imap "C-x C-f" 'company-files)
  (general-imap "C-x C-]" 'company-tags)
  )

(use-package company-dabbrev
  :after company
  :init
  (setf company-dabbrev-ignore-case t
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
  (setq-default abbrev-mode t))

(use-package narrow
  :commands (narrow-to-region narrow-to-defun widen)
  :init
  (evil-define-command evgeni-narrow-or-widen (begin end)
    (interactive "<r>")
    (cond ((region-active-p)
           (deactivate-mark)
           (let ((indbuf (clone-indirect-buffer nil nil)))
             (with-current-buffer indbuf
               (narrow-to-region begin end))
             (switch-to-buffer indbuf)))
          ((buffer-narrowed-p) (widen))
          (t (let ((indbuf (clone-indirect-buffer nil nil)))
               (with-current-buffer indbuf
                 (narrow-to-defun))
               (switch-to-buffer indbuf)))))
  (ex! "na[rrow]" 'evgeni-narrow-or-widen))

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
                              (haskell-mode . "stack --silent ghc -- -e :q %f")
                              (emacs-lisp-mode    . (emacs-lisp-byte-compile))
                              ))

  (defun evgeni-close-compile-win-if-successful (buffer string)
    (when (and
           (buffer-live-p buffer)
           (string-match "compilation" (buffer-name buffer))
           (string-match "finished" string)
           (not
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "warning" nil t))))
      (delete-windows-on buffer)))

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
          ("*compilation*" :align below :size 0.25)
          ("*grep*" :align below :size 0.25)
          )))

(use-package imenu-anywhere
  :commands ido-imenu-anywhere
  :ensure t)

;; elisp
(use-package elisp-mode
  :config
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (modify-syntax-entry ?- "w")
                                    (modify-syntax-entry ?! "w"))))

(use-package cperl-mode
  :commands cperl-mode
  :load-path "extra-packages"
  :mode (("\\.pl$"   . cperl-mode)
         ("\\.pm$"   . cperl-mode)
         ("\\.t$"    . cperl-mode)
         ("\\.html$" . cperl-mode))
  :config
  (setq cperl-invalid-face nil
        cperl-indent-subs-specially nil
        cperl-indent-parens-as-block t
        cperl-indent-subs-specially nil
        cperl-close-paren-offset -3 ;; can't be set from .dir-locals.el?
        cperl-indent-level 2
        ;; cperl-continued-statement-offset 4
        ;; cperl-tab-always-indent t)
        )

  (defun evgeni-perl-dump ()
    (interactive)
    (let ((word (thing-at-point 'word)))
      (save-excursion (end-of-line)
                      (newline-and-indent)
                      (insert (concat "use Data::Dump qw(pp); warn '" word ": ' . pp($" word ") . \"\\n\";")))))
  (general-define-key :keymaps 'local :states 'normal "] d" 'evgeni-perl-dump)

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

  (add-hook 'cperl-mode-hook (lambda ()
                               (modify-syntax-entry ?_ "w")
                             (set-face-background 'cperl-hash-face nil)
                             (set-face-foreground 'cperl-hash-face nil)
                             (set-face-background 'cperl-array-face nil)
                             (set-face-foreground 'cperl-array-face nil)
                               (electric-pair-mode)))


(use-package intero
  :ensure t
  :commands intero-mode)

(use-package whitespace
  :config
  :commands whitespace-mode
  (setq whitespace-style '(face tabs trailing)))
