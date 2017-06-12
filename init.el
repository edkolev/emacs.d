(defconst emacs-start-time (current-time))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed (float-time (time-subtract (current-time)
                                                       emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed)))
          t)

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
(setq blink-cursor-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setf initial-scratch-message ""
      initial-major-mode 'emacs-lisp-mode)
(setq echo-keystrokes 0.02)
(setq scroll-step 2)
(setq default-major-mode 'text-mode)
(setq enable-local-variables :all)

(defun display-startup-echo-area-message ()
  (message ""))
(setq load-prefer-newer t)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'toggle-scroll-bar) (toggle-scroll-bar -1))

(global-set-key "\C-ch" help-map)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

(modify-syntax-entry ?_ "w" (standard-syntax-table))
(add-hook 'prog-mode-hook (lambda () (modify-syntax-entry ?_ "w")))

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

;; upgrade installed packages
(defun evgeni-upgrade-packages ()
  (interactive)
  (save-window-excursion
    (package-refresh-contents)
    (package-list-packages t)
    (package-menu-mark-upgrades)
    (condition-case nil
        (package-menu-execute t)
      (error
       (package-menu-execute)))))

;; themes
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
(use-package apropospriate-theme :ensure t :defer t)
(use-package spacegray-theme :ensure t :defer t)
(use-package minimal-theme :ensure t :defer t)

;; (set-face-bold-p 'bold nil) ;; disable bold
(load-theme (if (display-graphic-p)
                'spacemacs-light ;; emacs desktop app
              'leuven) ;; in terminal
            t)

;; packages
(use-package general ;; https://gitlab.com/KNX32542/dotfiles/blob/master/emacs/.emacs.d/init.el
  :ensure t
  :config
  (general-evil-setup))

(use-package hippie-exp
  :init
  (general-imap "TAB" 'evgeni-tab)
  (defun evgeni-tab ()
    (interactive)
    (if (looking-at "\\_>") (hippie-expand nil) (indent-for-tab-command)))

  (defun my-yas-hippie-try-expand (first-time)
    (if (not first-time)
        (let ((yas-fallback-behavior 'return-nil))
          (yas-expand))
      (undo 1)
      nil))

  (defun he-tag-beg ()
    (save-excursion
      (backward-word 1)
      (point)))

  (defun tags-complete-tag (string predicate what)
    (save-excursion
      ;; If we need to ask for the tag table, allow that.
      (if (eq what t)
          (all-completions string (tags-completion-table) predicate)
        (try-completion string (tags-completion-table) predicate))))

  (defun try-expand-tag (old)
    (when tags-table-list
      (unless old
        (he-init-string (he-tag-beg) (point))
        (setq he-expand-list
              (sort (all-completions he-search-string 'tags-complete-tag)
                    'string-lessp)))
      (while (and he-expand-list
                  (he-string-member (car he-expand-list) he-tried-table))
        (setq he-expand-list (cdr he-expand-list)))
      (if (null he-expand-list)
          (progn
            (when old (he-reset-string))
            ())
        (he-substitute-string (car he-expand-list))
        (setq he-expand-list (cdr he-expand-list))
        t)))

  (defun evgeni-hippie-expand-yasnippet ()
    (interactive)
    (let ((hippie-expand-try-functions-list '(my-yas-hippie-try-expand)))
      (hippie-expand nil)))
  (general-imap "C-x C-y" 'evgeni-hippie-expand-yasnippet)
  (general-imap "C-x TAB" 'evgeni-hippie-expand-yasnippet)

  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-all-abbrevs
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-expand-tag
                                           ;; my-yas-hippie-try-expand
                                           )
        ))

(use-package evil
  :load-path "~/dev/evil"
  :ensure t
  :init
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-maybe-remove-spaces t) ;; clean whitespace on lines with nothing but whitespace
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
               evil-ex-search-word-forward
               swiper))
    (advice-add f :after #'evgeni-add-nohighlight-hook))

  (defun evgeni-save-file ()
    (interactive)
    (if (or buffer-file-name (buffer-base-buffer))
        (call-interactively 'save-buffer)
      (message "No file")))

  (general-nmap "RET" 'evgeni-save-file)
  (general-nmap "[ Q" 'first-error)
  (general-nmap "] q" 'next-error)
  (general-nmap "[ q" 'previous-error)
  (general-nmap ", w" 'evil-window-vsplit)

  (general-nmap "C-c C-b" 'ido-switch-buffer)

  (general-nmap "] SPC" (lambda (count) (interactive "p") (dotimes (_ count) (save-excursion (evil-insert-newline-below)))))
  (general-nmap "[ SPC" (lambda (count) (interactive "p") (dotimes (_ count) (save-excursion (evil-insert-newline-above)))))

  (general-nmap "C-p" 'beginning-of-defun)
  (general-nmap "C-n" 'end-of-defun)
  (general-nmap "[ m" 'beginning-of-defun)
  (general-nmap "] m" 'end-of-defun)
  (general-nmap "] M" 'end-of-defun)
  (general-nvmap "0" 'evil-first-non-blank)
  (general-omap "0" 'evil-first-non-blank)

  (defvar evgeni-conflict-marker-regex "^[<=>|]\\{7\\}")
  (defun evgeni-next-conflict ()
    (interactive)
    (re-search-forward evgeni-conflict-marker-regex))

  (defun evgeni-prev-conflict ()
    (interactive)
    (re-search-backward evgeni-conflict-marker-regex))

  (general-nmap "] n" 'evgeni-next-conflict)
  (general-nmap "[ n" 'evgeni-prev-conflict)

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
      (error (if (getenv "TMUX") (let ((default-directory "~"))
                                   (shell-command-to-string tmux-cmd))))))

  (general-mmap "C-h" (lambda! (evgeni-window-navigate 'windmove-left "tmux select-pane -L")))
  (general-mmap "C-j" (lambda! (evgeni-window-navigate 'windmove-down "tmux select-pane -D")))
  (general-mmap "C-k" (lambda! (evgeni-window-navigate 'windmove-up "tmux select-pane -U")))
  (general-mmap "C-l" (lambda! (evgeni-window-navigate 'windmove-right "tmux select-pane -R")))

  ;; insert state
  (general-imap "C-e" 'end-of-line)
  (general-imap "C-u" (lambda () (interactive) (evil-delete (point-at-bol) (point))))
  (general-imap "C-x s" 'complete-symbol)
  (general-imap "C-a" 'evgeni-beginning-of-line)
  ;; auto-indent on RET
  (define-key global-map (kbd "RET") 'newline-and-indent)

  (defun evgeni-beginning-of-line ()
    (interactive)
    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (move-beginning-of-line 1))))

  ;; expand lines
  (general-imap "C-l" 'evil-complete-next-line)
  (general-imap "C-x C-l" 'evil-complete-next-line)

  ;; completion

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
  (general-nmap "C-c o w" 'visual-line-mode)

  ;; clean whitespace on lines with nothing but whitespace
  (add-hook 'evil-insert-state-exit-hook 'evgeni-clean-whitespace-line)
  (defun evgeni-clean-whitespace-line ()
    (when (string-match "^\s+$" (or (thing-at-point 'line) ""))
      (delete-region (line-beginning-position) (line-end-position))))

  ;; cursor in terminal
  (cond
   ((getenv "TMUX")
    (add-hook 'evil-insert-state-entry-hook (lambda () (send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=1\x7\e\\")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=0\x7\e\\"))))
   ((not (display-graphic-p))
    (add-hook 'evil-insert-state-entry-hook (lambda () (send-string-to-terminal "\e]50;CursorShape=1\x7")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (send-string-to-terminal "\e]50;CursorShape=0\x7")))))

  ;; <escape> should quit
  (define-key minibuffer-local-map (kbd "<escape>") 'keyboard-escape-quit)
  (define-key minibuffer-local-ns-map (kbd "<escape>") 'keyboard-escape-quit)
  (define-key minibuffer-local-completion-map (kbd "<escape>") 'keyboard-escape-quit)
  (define-key minibuffer-local-must-match-map (kbd "<escape>") 'keyboard-escape-quit)
  (define-key minibuffer-local-isearch-map (kbd "<escape>") 'keyboard-escape-quit)

  ;; move by visual lines with j/k
  (define-key evil-normal-state-map "j"  'evil-next-visual-line)
  (define-key evil-normal-state-map "k"  'evil-previous-visual-line)
  (define-key evil-normal-state-map "gj" 'evil-next-line)
  (define-key evil-normal-state-map "gk" 'evil-previous-line)
  (define-key evil-visual-state-map "j"  'evil-next-visual-line)
  (define-key evil-visual-state-map "k"  'evil-previous-visual-line)
  (define-key evil-visual-state-map "gj" 'evil-next-line)
  (define-key evil-visual-state-map "gk" 'evil-previous-line)

  (defun evgeni-prev-or-move-end-of-line ()
    (interactive)
    (when (not (thing-at-point 'line t))
      (call-interactively 'previous-complete-history-element))
    (call-interactively 'move-end-of-line))

  (defun evgeni-ex-delete-or-complete ()
    (interactive)
    (if (eq (point) (point-at-eol))
        (evil-ex-completion)
      (delete-char 1)))

  ;; tweak search and ex maps
  (define-key evil-ex-search-keymap "\C-e"  'evgeni-prev-or-move-end-of-line)
  (define-key evil-ex-search-keymap "\C-e"  'evgeni-prev-or-move-end-of-line)
  (define-key evil-ex-completion-map "\C-e" 'evgeni-prev-or-move-end-of-line)
  (define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
  (define-key evil-ex-completion-map "\C-b" 'backward-char)
  (define-key evil-ex-completion-map "\C-d" 'evgeni-ex-delete-or-complete)

  (define-key evil-normal-state-map "zz" 'recenter-top-bottom)

  (defun evgeni-word-count ()
    (interactive)
    ;; example output:
    ;; Selected 5 of 1734 Lines; 35 of 11721 Words; 201 of 72602 Bytes
    ;; Selected 5 of 1734 Lines; 35 of 11721 Words; 205 of 72602 Bytes
    (let ((line (line-number-at-pos))
          (max-line (line-number-at-pos (buffer-size)))) ;; invalid when narrowed
      (message "Col ? of ?; Line %s of %s; Word ? of ?; Byte ? of ?" line max-line)))
  (define-key evil-normal-state-map "g\C-g" 'evgeni-word-count)

  (define-key evil-list-view-mode-map (kbd "q") #'kill-buffer-and-window)

  ;; :remove to delete file and buffer
  (defun evgeni-remove ()
    (interactive)
    (let ((filename (buffer-file-name)))
      (when filename
        (delete-file filename)
        (kill-buffer)
        (message "Removed %s and its buffer" filename))))
  (ex! "remove" 'evgeni-remove))

(use-package ace-window
  :ensure t
  :general
  (general-nmap "C-w C-w" 'ace-window))

(use-package avy
  :ensure t
  :bind (:map evil-motion-state-map
              ("C-c C-g" . avy-goto-char-timer)
              ("gY" . avy-copy-region)
              ("gyy" . avy-copy-line)))

(use-package evil-surround
  :ensure t
  :config (global-evil-surround-mode))

(use-package evil-lion
  :load-path "src/evil-lion"
  :ensure t
  :config
  (evil-lion-mode))

(use-package evil-commentary
  :ensure t
  :diminish 'evil-commentary-mode
  :bind (:map evil-normal-state-map
              ("gc" . evil-commentary)))

(use-package evil-exchange
  :ensure t
  :config (evil-exchange-install))

(use-package evil-replace-with-register
  :ensure t
  :bind (:map evil-normal-state-map
              ("gr" . evil-replace-with-register)))

(use-package evil-visualstar
  :ensure t
  :config
  (global-evil-visualstar-mode))

(use-package evil-goggles
  :ensure t
  :load-path "src/evil-goggles"
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces)

  ;; (require 'diff-mode) ;; load diff-* faces

  ;; (setq evil-goggles-default-face 'diff-changed)

  ;; (setq evil-goggles-faces-alist `(( evil-delete . diff-removed ) ;; isearch-fail
  ;;                                  ( evil-yank . diff-changed )
  ;;                                  ( evil-paste-after . diff-added )
  ;;                                  ( evil-paste-before . diff-added )
  ;;                                  ( undo-redo-add . diff-added )
  ;;                                  ( undo-redo-remove . diff-removed )))
  )

(use-package evil-magit
  :ensure t
  :after magit
  :init
  (setq evil-magit-want-horizontal-movement t))

(use-package recentf
  :init
  (setq recentf-save-file (expand-file-name "recentf.el" user-emacs-directory))
  (setq recentf-max-saved-items 500)
  (setq recentf-exclude '("/tmp/" "/ssh:"))
  (setq recentf-auto-cleanup 'never)
  (recentf-mode))

(use-package evil-indent-plus ;; indent object
  :ensure t
  :config (evil-indent-plus-default-bindings))

(use-package saveplace
  :config
  (save-place-mode)
  (setq save-place-file (expand-file-name "saveplace.el" user-emacs-directory)))

(use-package savehist
  :init
  (setq ;; savehist-file (concat spacemacs-cache-directory "savehist")
   enable-recursive-minibuffers t ; Allow commands in minibuffers
   history-length 1000
   savehist-additional-variables '(mark-ring
                                   global-mark-ring
                                   search-ring
                                   regexp-search-ring
                                   extended-command-history)
   savehist-autosave-interval 60)
  (savehist-mode t))
(use-package dired
  :general
  (general-nmap "-"   (lambda () (interactive) (dired ".")))
  :config
  (setq dired-listing-switches "-alh")
  (setq dired-auto-revert-buffer t)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (define-key dired-mode-map (kbd "-") 'dired-up-directory))

(use-package wdired
  :commands wdired-change-to-wdired-mode
  :init
  (ex! "wdired" 'wdired-change-to-wdired-mode)
  (ex! "wdired-finish" 'wdired-finish-edit))

(use-package magit
  :ensure t
  :general
  (general-nmap "U U" '(magit-status))
  (general-nmap "U w" '(magit-stage-file))
  (general-nmap "U d" '(magit-diff-unstaged))
  (general-nmap "U l" '(magit-log-head))
  (general-nmap "U r" '(magit-file-checkout))
  (general-nmap "U c" '(magit-commit-popup))
  (general-nmap "U b" '(magit-blame))
  (general-nmap "U z" '(magit-stash-popup))
  (general-nmap "U p" '(magit-push-popup))
  (general-nmap "U f" '(magit-fetch-popup))
  (general-nmap "U F" '(magit-pull-popup))
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (setq magit-diff-refine-hunk 't)

  (remove-hook 'magit-status-sections-hook 'magit-insert-stashes) ;; don't show stashes
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-untracked-files 'magit-insert-staged-changes 1) ;; lower untracked

  (unbind-key "C-h" magit-mode-map)
  (unbind-key "C-j" magit-mode-map)
  (unbind-key "C-k" magit-mode-map)
  (unbind-key "C-l" magit-mode-map)
  (unbind-key "SPC" magit-status-mode-map)
  (general-evil-define-key 'normal magit-mode-map "C-n" 'magit-section-forward)
  (general-evil-define-key 'normal magit-mode-map "C-p" 'magit-section-backward)

  (add-hook 'git-commit-mode-hook 'flyspell-mode))

(use-package vdiff
  :ensure t
  :defer t
  :config
  (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)
  (setq vdiff-magit-stage-is-2way t))

(use-package vdiff-magit
  :ensure t
  :bind (:map evil-normal-state-map
              ("U D" . vdiff-magit-popup))
  :config
  (use-package vdiff))

(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :general
  (general-nmap "C-c o f" 'flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (setq-default flycheck-disabled-checkers '(perl-perlcritic)))

;; TODO - company through hippie exp only https://github.com/jwiegley/dot-emacs/blob/master/init.el#L1355
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

(use-package org
  :mode (("\\.org$"   . org-mode))
  :defer t
  :ensure t
  :config

  ;; https://github.com/edwtjo/evil-org-mode/blob/master/evil-org.el
  (dolist (state '(normal insert))
    (evil-define-key state org-mode-map
      (kbd "M-l") 'org-metaright
      (kbd "M-h") 'org-metaleft
      (kbd "M-k") 'org-metaup
      (kbd "M-j") 'org-metadown
      (kbd "M-L") 'org-shiftmetaright
      (kbd "M-H") 'org-shiftmetaleft
      (kbd "M-K") 'org-shiftmetaup
      (kbd "M-J") 'org-shiftmetadown
      (kbd "M-o") 'org-insert-heading
      (kbd "M-t") 'org-insert-todo-heading))

  (evil-define-key 'normal org-mode-map
    "H" 'org-shiftleft
    "J" 'org-shiftdown
    "K" 'org-shiftup
    "L" 'org-shiftright))

(use-package evil-org
  :ensure t
  :after org
  :config

  (add-hook 'org-mode-hook 'evil-org-mode)

  (add-hook 'org-mode-hook (lambda ()
                             (evil-org-mode)
                             (evil-org-set-key-theme '(operators)))))

(use-package org-bullets
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package move-text
  :ensure t
  :general
  (general-nmap "] e" (lambda (count)
                        (interactive "p")
                        (let ((count (or count 1)))
                          (dotimes (i count)
                            (move-text-line-down)))))
  (general-nmap "[ e" (lambda (count)
                        (interactive "p")
                        (let ((count (or count 1)))
                          (dotimes (i count)
                            (move-text-line-up))))))

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
  (evil-define-command evgeni-narrow-or-widen (bang begin end)
    (interactive "<!><r>")
    (let ((buf (if bang (clone-indirect-buffer nil nil) (current-buffer))))
      (with-current-buffer buf
        (cond ((region-active-p) (narrow-to-region begin end))
              ((buffer-narrowed-p) (if (buffer-base-buffer)
                                       (kill-buffer) ;; wipe out indirect buffer
                                     (widen)))
              (t (narrow-to-defun))))
      (switch-to-buffer buf)))
  (ex! "nar[row]" 'evgeni-narrow-or-widen))

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
  :ensure t
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
  (setq compilation-scroll-output 'next-error)
  (setq compilation-read-command nil)
  (setq compilation-ask-about-save nil)

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
          (message "*grep* %d matches found" (- (count-lines (point-min) (point-max)) 6))))))

  (add-hook 'compilation-finish-functions 'evgeni-scroll-grep-win))

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
                              (haskell-mode . "stack --silent ghc -- -Wall -e :q %f")
                              (emacs-lisp-mode    . (emacs-lisp-byte-compile))
                              ))

  (defun evgeni-close-compile-win-if-successful (buffer string)
    (if (and
         (eq major-mode 'compilation-mode)
         (buffer-live-p buffer)
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (= 8 (line-number-at-pos (point-max)))) ;; check lines are exactly 8
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
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  :config
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
          ("*xref*"                    :align below :size 10  :noselect t)
          ("*Help*"                    :align below :size 16  :select t)
          ("*Backtrace*"               :align below :size 25  :noselect t)
          ("*Flycheck error messages*" :align below :size 0.25)
          ("*compilation*"             :align below :size 10)
          ("*grep*"                    :align below :size 10)
          )))

(use-package which-key
  :ensure t
  :diminish 'which-key-mode
  :config
  (setq which-key-idle-delay 1.5)
  (which-key-mode))

(use-package ivy
  :ensure t
  :diminish 'ivy-mode
  :demand
  :bind (:map evil-normal-state-map
              ("SPC" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'full)
  (define-key ivy-minibuffer-map (kbd "C-w") 'backward-kill-word)
  (define-key ivy-minibuffer-map (kbd "C-u") (lambda () (interactive) (kill-region (point) (point-at-bol))))

  ;; C-r C-w to read word at point
  (unbind-key "C-r" ivy-minibuffer-map)
  (define-key ivy-minibuffer-map (kbd "C-r C-w") 'ivy-next-history-element)

  (define-key ivy-minibuffer-map (kbd "C-c C-c") 'ivy-restrict-to-matches))

(use-package swiper
  :ensure t
  :bind (:map evil-motion-state-map
              ("C-c C-r" . ivy-resume)
              ("/" . swiper))
  :config
  (setq swiper-goto-start-of-match t))

(use-package counsel
  :ensure t
  :load-path "~/dev/swiper"
  :general
  (general-nmap ", g" 'counsel-git-grep)
  (general-nmap "g SPC" 'counsel-git)
  (general-nmap "g /" 'counsel-git-grep)
  :init
  (setq counsel-git-cmd "git ls-files --cached --others --exclude-standard")

  (general-nmap ", f" (lambda ()
                        (interactive)
                        (let ((imenu-default-goto-function 'evgeni-imenu-goto))
                          (call-interactively 'counsel-imenu))))

  (defun evgeni-imenu-goto (&rest args)
    (evil-set-jump)
    (apply 'imenu-default-goto-function args))

  (define-key global-map [remap describe-function] 'counsel-describe-function)
  (define-key global-map [remap describe-variable] 'counsel-describe-variable)
  (define-key global-map [remap execute-extended-command] 'counsel-M-x)
  :config
  (setq counsel-git-grep-skip-counting-lines t)
  (use-package smex)
  ;; use C-] to go to definition
  (define-key counsel-describe-map (kbd "C-]") 'counsel-find-symbol)

  ;; use "K" to grep
  (general-nmap "K" (lambda () (interactive) (counsel-git-grep nil (thing-at-point 'word))))

  (evil-define-motion evgeni-counsel-git-grep-visual (beg end)
    (interactive "<r>")
    (evil-exit-visual-state)
    (counsel-git-grep nil (buffer-substring-no-properties beg end)))

  (general-vmap "K" 'evgeni-counsel-git-grep-visual))

(use-package imenu-anywhere
  :ensure t
  :general
  (general-nmap ", F" 'imenu-anywhere))

(use-package elisp-mode
  :config
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)

  (defun evgeni-elisp-eval ()
    (interactive)
    (if (region-active-p)
        (call-interactively 'eval-region)
      (call-interactively 'eval-buffer)))

  (evil-ex-define-cmd "eval" 'evgeni-elisp-eval)

  (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
  (modify-syntax-entry ?. "w" emacs-lisp-mode-syntax-table)
  (modify-syntax-entry ?! "w" emacs-lisp-mode-syntax-table))

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
                                 (set-face-foreground 'cperl-array-face nil))))

(use-package intero
  :ensure t
  :commands intero-mode)

(use-package whitespace
  :config
  (setq whitespace-style '(face tabs trailing))
  (defun evgeni-show-trailing-whitespace () (setq show-trailing-whitespace t))
  (add-hook 'prog-mode-hook 'evgeni-show-trailing-whitespace)
  (set-face-attribute 'trailing-whitespace nil
                      :background
                      (face-attribute 'mode-line :background)))

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
  ;; (setq undo-tree-auto-save-history t)
  )

(use-package perl-mode
  :commands perl-mode
  :load-path "extra-packages"
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

  (set-face-underline 'font-lock-variable-name-face nil)

  (defun evgeni-perl-dump ()
    (interactive)
    (let ((word (thing-at-point 'word)))
      (save-excursion (end-of-line)
                      (newline)
                      (indent-according-to-mode)
                      (insert (concat "use Data::Dump qw(pp); warn '" word ": ' . pp($" word ") . \"\\n\";")))))

  (defun evgeni-define-perl-function ()
    (interactive)
    (let ((function-name-at-point (thing-at-point 'word)))
      (end-of-defun)
      (insert "\nsub " function-name-at-point " {\n}\n")
      (evil-previous-line)
      (evil-open-above 1)))

  (general-evil-define-key 'normal 'perl-mode-map "C-c f" 'evgeni-define-perl-function)
  (general-evil-define-key 'normal 'perl-mode-map "] d" 'evgeni-perl-dump)

  (modify-syntax-entry ?_ "w" perl-mode-syntax-table)

  (add-hook 'perl-mode-hook #'(lambda ()
                                ;; taken from cperl-mode, used for beginning-of-defun / end-of-defun
                                (setq defun-prompt-regexp
                                      "^[ 	]*\\(\\(?:sub\\)\\(\\([ 	\n]\\|#[^\n]*\n\\)+\\(::[a-zA-Z_0-9:']+\\|[a-zA-Z_'][a-zA-Z_0-9:']*\\)\\)\\([ 	\n]*\\(#[^\n]*\n[ 	\n]*\\)*\\(([^()]*)\\)\\)?\\([ 	\n]*\\(#[^\n]*\n[ 	\n]*\\)*\\(:\\([ 	\n]*\\(#[^\n]*\n[ 	\n]*\\)*\\(\\sw\\|_\\)+\\((\\(\\\\.\\|[^\\\\()]\\|([^\\\\()]*)\\)*)\\)?\\([ 	\n]*\\(#[^\n]*\n[ 	\n]*\\)*:\\)?\\)+\\)\\)?\\|\\(BEGIN\\|UNITCHECK\\|CHECK\\|INIT\\|END\\|AUTOLOAD\\|DESTROY\\)\\)[ 	\n]*\\(#[^\n]*\n[ 	\n]*\\)*")
                                ))
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

(use-package s
  :ensure t
  :defer t
  :functions s-trim)

(use-package haskell-mode
  :commands haskell-mode
  :config
  (setq haskell-indent-spaces 4)
  ;; (defun evgeni-haskell-evil-auto-indent () (setq evil-auto-indent nil))
  ;; (add-hook 'haskell-mode-hook 'evgeni-haskell-evil-auto-indent)

  ;; hack to get saner "o" and "O"
  (defun evgeni-haskell-evil-open-below ()
    (interactive)
    (evil-append-line nil)
    (haskell-indentation-newline-and-indent))

  (defun evgeni-haskell-evil-open-above ()
    (interactive)
    (evil-insert-line nil)
    (save-excursion
      (haskell-indentation-newline-and-indent)))

  (evil-define-key 'normal haskell-mode-map
    "o" 'evgeni-haskell-evil-open-below)
  (evil-define-key 'normal haskell-mode-map
    "O" 'evgeni-haskell-evil-open-above)

  ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation) <- this one
  (if (fboundp 'electric-indent-local-mode)
      (electric-indent-local-mode -1))

  (defun haskell-indentation-advice ()
    (when (and (< 1 (line-number-at-pos))
               (save-excursion
                 (forward-line -1)
                 (string= "" (s-trim (buffer-substring (line-beginning-position) (line-end-position))))))
      (delete-region (line-beginning-position) (point))))

  (advice-add 'haskell-indentation-newline-and-indent
              :after 'haskell-indentation-advice)

  (define-abbrev-table 'haskell-mode-abbrev-table
    '(("undef" "undefined")))

  (with-eval-after-load 'align
    (add-to-list 'align-rules-list
                 '(haskell-types
                   (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                   (modes . haskell-modes)))
    (add-to-list 'align-rules-list
                 '(haskell-assignment
                   (regexp . "\\(\\s-+\\)=\\s-+")
                   (modes . haskell-modes)))
    (add-to-list 'align-rules-list
                 '(haskell-arrows
                   (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                   (modes . haskell-modes)))
    (add-to-list 'align-rules-list
                 '(haskell-left-arrows
                   (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                   (modes . haskell-modes))))

  ;; ;; Make "RET" behaviour in REPL saner
  ;; (evil-define-key 'insert haskell-interactive-mode-map
  ;;   (kbd "RET") 'haskell-interactive-mode-return)
  ;; (evil-define-key 'normal haskell-interactive-mode-map
  ;;   (kbd "RET") 'haskell-interactive-mode-return)

  ;; smart colon
  (defun evgeni-haskell-smart-colon ()
    (interactive)
    (let ((func-name (evgeni-haskell-recent-func-name)))
      (when func-name
        (save-excursion
          (progn
            (move-end-of-line nil)
            ;; (haskell-indentation-newline-and-indent)
            (newline)
            (insert func-name " = undefined")))
        ))
    (insert ":"))

  (defconst evgeni-haskell-func-name-regex "[0-9A-Za-z']+")

  (defun evgeni-haskell-recent-func-name ()
    (interactive)
    (when (looking-back
           ;; (format "^[\t ]*\\(%s\\)[\t *]*:" evgeni-haskell-func-name-regex))
           (format "^\\(%s\\)[\t *]*:" evgeni-haskell-func-name-regex))
      (match-string-no-properties 1)))

  (evil-define-key 'insert haskell-mode-map ":" 'evgeni-haskell-smart-colon))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config

  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)
  (setq sp-show-pair-delay 0) ;; TODO not working in insert mode?

  (require 'smartparens-config)
  (show-smartparens-global-mode)
  (add-hook 'perl-mode-hook #'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)

  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-pair "[" nil :post-handlers '(("||\n[i]" "RET")))

  (general-define-key :keymap 'emacs-lisp-mode-map "C-c <" 'sp-forward-slurp-sexp)
  (general-define-key :keymap 'emacs-lisp-mode-map "C-c >" 'sp-forward-barf-sexp)

  (defun evgeni-smartparens-lisp-hook () ;; TODO drop this
    (smartparens-mode)
    ;; (general-define-key :keymap 'emacs-lisp-mode-map "C-c >" 'sp-backward-slurp-sexp)
    (general-define-key :keymap 'emacs-lisp-mode-map "C-c <" 'sp-forward-slurp-sexp)
    ;; (general-define-key :keymap 'emacs-lisp-mode-map "todo" 'sp-backward-barf-sexp)
    ;; (general-define-key :keymap 'emacs-lisp-mode-map "todo" 'sp-forward-barf-sexp)

    ;; nmap <buffer> >(  <Plug>(sexp_emit_head_element)
    ;; nmap <buffer> <)  <Plug>(sexp_emit_tail_element)
    ;; nmap <buffer> <(  <Plug>(sexp_capture_prev_element)
    ;; nmap <buffer> >)  <Plug>(sexp_capture_next_element)

    ;; ("C-M-f" . sp-forward-sexp) ;; navigation
    ;; ("C-M-b" . sp-backward-sexp)
    ;; ("C-M-u" . sp-backward-up-sexp)
    ;; ("C-M-d" . sp-down-sexp)
    ;; ("C-M-p" . sp-backward-down-sexp)
    ;; ("C-M-n" . sp-up-sexp)
    ;; ("M-s" . sp-splice-sexp) ;; depth-changing commands
    ;; ("M-<up>" . sp-splice-sexp-killing-backward)
    ;; ("M-<down>" . sp-splice-sexp-killing-forward)
    ;; ("M-r" . sp-splice-sexp-killing-around)
    ;; ("C-)" . sp-forward-slurp-sexp) ;; barf/slurp
    ;; ("C-<right>" . sp-forward-slurp-sexp)
    ;; ("C-}" . sp-forward-barf-sexp)
    ;; ("C-<left>" . sp-forward-barf-sexp)
    ;; ("C-(" . sp-backward-slurp-sexp)
    ;; ("C-M-<left>" . sp-backward-slurp-sexp)
    ;; ("C-{" . sp-backward-barf-sexp)
    ;; ("C-M-<right>" . sp-backward-barf-sexp)
    ;; ("M-S" . sp-split-sexp)
    )
  )

(use-package loccur
  :ensure t
  :commands (loccur-current loccur)
  :init
  (setq loccur-highlight-matching-regexp nil)
  (setq loccur-jump-beginning-of-line t)
  (general-nmap ", *" 'loccur-current)
  (general-vmap ", *" 'loccur)
  (evil-ex-define-cmd "loccur" 'loccur)
  (evil-define-minor-mode-key 'normal 'loccur-mode (kbd "q") 'loccur)
  (evil-define-minor-mode-key 'normal 'loccur-mode (kbd "RET") 'loccur)
  (evil-define-minor-mode-key 'normal 'loccur-mode (kbd "<escape>") 'loccur))

(use-package ledger-mode
  :ensure t
  :commands ledger-mode)

(use-package yasnippet
  :disabled t
  :ensure t
  :demand t
  ;; :diminish yas-minor-mode
  :commands (yas-expand yas-minor-mode)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :bind (("C-c y TAB" . yas-expand)
         ("C-c y s"   . yas-insert-snippet)
         ("C-c y v"   . yas-visit-snippet-file))
  :config
  (evil-ex-define-cmd "new-snippet" 'yas-new-snippet)
  ;; from https://github.com/haskell/haskell-snippets
  ;; (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))

  ;; (yas-load-directory "~/.emacs.d/snippets/")
  (yas-global-mode 1)

  (bind-key "C-i" #'yas-next-field-or-maybe-expand yas-keymap))

(use-package haskell-snippets
  :disabled t
  :ensure t)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)     ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*")) ; don't muck with special buffers

(use-package evil-iedit-state
  :ensure t
  ;; :commands evil-iedit-state/iedit-mode
  :general
  (general-nvmap ", m" 'evil-iedit-state/iedit-mode)
  :config
  (define-key evil-iedit-state-map (kbd "TAB") 'iedit-toggle-selection)
  (define-key evil-iedit-state-map (kbd "C-c f") 'iedit-restrict-function)
  (define-key evil-iedit-state-map (kbd "C-c C-f") 'iedit-restrict-function)
  (unbind-key "TAB" iedit-mode-keymap)
  ;; (define-key evil-iedit-state-map (kbd "TAB") 'iedit-restrict-region)
  ;; (define-key evil-iedit-state-map (kbd "TAB") 'iedit-restrict-current-line)
  )

(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  ;; (setq beacon-color "#d7dfff")
  (setq beacon-color (face-background 'region))
  (setq beacon-overlay-priority 1)
  ;; (setq beacon-blink-when-window-scrolls t)
  ;; (setq beacon-blink-when-point-moves-horizontally 10)
  ;; (setq beacon-color 0.9)
  (beacon-mode))

(use-package regex-tool
  :ensure t
  :commands regex-tool)

(use-package quickrun
  :ensure t
  :disabled t
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region)
  :init
  (setq quickrun-focus-p nil)
  (ex! "quickrun" 'quickrun)
  (ex! "quickrun-region" 'quickrun-region)
  (ex! "quickrun-replace-region" 'quickrun-replace-region))

(use-package macrostep
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))

(use-package yaml-mode
  :ensure t
  :commands yaml-mode)

(use-package package-lint
  :ensure t
  :commands package-lint-current-buffer)

(use-package multiple-cursors
  :load-path "src/multiple-cursors.el"
  :commands mc/edit-lines
  :config
  (evil-ex-define-cmd "mc[cursors]" 'mc/edit-lines)

  ;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  ;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  ;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  ;; mc/mark-next-like-this-word

  )

(use-package idle-highlight-mode
  :commands idle-highlight-mode
  :ensure t
  :bind (:map evil-normal-state-map
              ("C-c o h" . idle-highlight-mode)))

(use-package repl-toggle
  :disabled t
  :ensure t
  :general
  (general-imap "C-c C-z" 'rtog/toggle-repl)
  (general-nmap "C-c C-z" 'rtog/toggle-repl)
  :config
  (setq rtog/fullscreen t)
  :init
  (setq rtog/mode-repl-alist '((emacs-lisp-mode . ielm))))

(use-package js
  :mode "\\.js\\'"
  :config
  (modify-syntax-entry ?_ "w" js-mode-syntax-table))

(use-package css-mode
  :ensure t
  :mode "\\.css\\'"
  :config
  (modify-syntax-entry ?- "w" css-mode-syntax-table))
