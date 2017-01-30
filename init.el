
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
;; initial *scratch* buffer
(setf initial-scratch-message ""
      initial-major-mode 'emacs-lisp-mode)
(setq linum-format "%d ")

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
(use-package material-theme :ensure t :defer t)
(use-package molokai-theme :ensure t :defer t)
(use-package monokai-theme :ensure t :defer t)
(use-package darktooth-theme :ensure t :defer t)
(use-package seoul256-theme :ensure t :defer t)
(use-package doom-themes :ensure t :defer t)
(use-package ujelly-theme :ensure t :defer t)
(use-package zenburn-theme :ensure t :defer t)

(load-theme 'leuven t)

;; dark variants  Range:   233 (darkest) ~ 239 (lightest) ;; Default: 237
;; light variants Range:   252 (darkest) ~ 256 (lightest) ;; Default: 253
;; (setq seoul256-background 253)
;; (set-face-bold-p 'bold nil) ;; disable bold

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
  :init
  (setq evil-search-module 'isearch)
  (setq evil-ex-complete-emacs-commands nil)
  :ensure 
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
  (general-nmap "g m" 'evil-make)
  (general-nmap "g RET" 'evil-make)

  (general-nmap "g SPC" 'find-file-in-project)
  (general-nmap "C-c C-b" 'ido-switch-buffer)

  (general-nmap "] SPC" (lambda (count) (interactive "p") (dotimes (_ count) (save-excursion (evil-insert-newline-below)))))
  (general-nmap "[ SPC" (lambda (count) (interactive "p") (dotimes (_ count) (save-excursion (evil-insert-newline-above)))))

  (evil-define-command my-colorscheme (&optional theme)
    :repeat nil
    (interactive "<a>")
    (if theme
        (load-theme (intern theme) t)
      (call-interactively 'load-theme))
    )
  (evil-ex-define-cmd "colo[rscheme]" 'my-colorscheme)

  (defun my-collection-fn (str pred flag)
    (completion-table-dynamic (lambda (prefix) (list "aa" "bb")))
    ;; (str)
    ;; (completion-table-dynamic (lambda (prefix) (mapcar 'symbol-name (custom-available-themes))))
    ;; (completion-table-dynamic (lambda (arg) nil))

    )

  (evil-ex-define-argument-type thm
    "Defines an argument type which can take theme names."
    :collection (lambda () (completion-table-dynamic (lambda (prefix) (list "aa" "bb"))))
    )

  (evil-define-interactive-code "<thm>"
    "A valid evil state."
    :ex-arg thm
    (list (when (evil-ex-p) evil-ex-argument)))

  (evil-ex-define-cmd "xx" 'my-colorscheme1)

  (evil-define-command my-colorscheme1 (&optional theme)
    :repeat nil
    (interactive "<thm>")
    (if theme
        (message theme)
      )
    )

  ;; tyank & tput
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
    (evil-ex-define-cmd "tyank" 'evgeni-tyank)
    )

  (general-nmap "C-p" 'beginning-of-defun)
  (general-nmap "C-n" 'end-of-defun)
  (general-nmap "[ m" 'beginning-of-defun)
  (general-nmap "] m" 'end-of-defun)
  (general-nmap "0" 'evil-first-non-blank)

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
  (general-nmap "C-c o n" 'linum-mode)

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

(use-package evil-magit
  :ensure t
  :after magit
  :init
  (setq evil-magit-want-horizontal-movement t))

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
                                                (ido-completing-read
                                                 "Find recent file: "
                                                 (append (mapcar #'buffer-name (buffer-list)) recentf-list)))))

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
  :config
  (remove-hook 'magit-status-sections-hook 'magit-insert-stashes) ;; don't show stashes
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-untracked-files 'magit-insert-staged-changes 1) ;; lower untracked
  )

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
  :init
  (setf company-dabbrev-ignore-case 'keep-prefix
        company-dabbrev-ignore-invisible t
        company-dabbrev-downcase nil))

(use-package flx-ido
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(use-package idomenu
  :ensure t
  :general
  (general-nmap ", f" 'idomenu))

(use-package ido-vertical-mode
  :ensure t
  :config
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (ido-vertical-mode t))

(use-package org
  :defer t
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

(use-package narrow
  :init
  (with-eval-after-load 'evil
    (evil-ex-define-cmd "dn[arrow]" 'narrow-to-defun)
    (evil-ex-define-cmd "wi[den]" 'widen)))

(use-package wgrep
  :ensure t
  :init
  (ex! "wgrep[toggle]" 'wgrep-toggle-readonly-area))

(use-package shackle
  :ensure t
  :config
  (shackle-mode 1)
  (setq shackle-rules
        '(
          ("*xref*"            :align below :size 0.4 :noselect t)
          )))
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

