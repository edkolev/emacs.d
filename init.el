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
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

;; settings
(when (display-graphic-p)
  (set-default-font "Source Code Pro 13" nil t)
  ;; smooth scroll
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-follow-mouse 't)
  (setq scroll-step 1))
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
(setq require-final-newline t)

(setq initial-buffer-choice (lambda ()
                              (interactive)
                              (get-buffer "*Messages*")))

(defun display-startup-echo-area-message ()
  (message ""))
(setq load-prefer-newer t)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'toggle-scroll-bar)
  (toggle-scroll-bar -1)
  (add-to-list 'default-frame-alist '(vertical-scroll-bars . nil)))

(global-set-key "\C-ch" help-map)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

(modify-syntax-entry ?_ "w" (standard-syntax-table))
(modify-syntax-entry ?' "." (standard-syntax-table))
(add-hook 'prog-mode-hook (lambda () (modify-syntax-entry ?_ "w")))

;; project root
(defun evgeni-project-root ()
  (if (project-current)
      (cdr (project-current))
    default-directory))

;; etags
(defun evgeni-find-etags-file ()
  (let ((etags-file (expand-file-name ".git/etags" (evgeni-project-root))))
    (when (file-exists-p etags-file)
      etags-file)))
(setq default-tags-table-function 'evgeni-find-etags-file)
(setq tags-revert-without-query t)

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
(setq use-package-verbose nil)

;; upgrade installed packages
(defun evgeni-upgrade-packages ()
  (interactive)
  (save-window-excursion
    (package-refresh-contents)
    (package-list-packages t)
    (package-menu-mark-upgrades)
    (package-menu-execute t)))

(ex! "upgrade-packages" 'evgeni-upgrade-packages)
(ex! "refresh-packages" 'package-refresh-contents)

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
(use-package apropospriate-theme :ensure t :defer t) ;; ok
(use-package spacegray-theme :ensure t :defer t) ;; ok
(use-package gotham-theme :ensure t :defer t)
(use-package phoenix-dark-pink-theme :ensure t :defer t)
(use-package material-theme :ensure t :defer t)
(use-package tango-plus-theme :ensure t :defer t)

;; minimal themes
(use-package minimal-theme :ensure t :defer t)
(use-package paper-theme :ensure t :defer t)
(use-package white-theme :ensure t :defer t)
(use-package tao-theme :ensure t :defer t)
(use-package basic-theme :ensure t :defer t)

;; (set-face-bold-p 'bold nil) ;; disable bold
(load-theme (if (display-graphic-p)
                'tango-plus ;; flatui
              'leuven
              )
            t)

;; load mail.el if any
(when (file-readable-p (concat user-emacs-directory "mail.el"))
  (load-file (concat user-emacs-directory "mail.el")))

(use-package no-littering
  :ensure t
  :demand t)

(use-package desktop
  :if (display-graphic-p)
  :config
  (add-to-list 'frameset-filter-alist '(background-color . :never))
  (add-to-list 'frameset-filter-alist '(foreground-color . :never))
  (add-to-list 'frameset-filter-alist '(font . :never))
  (add-to-list 'frameset-filter-alist '(cursor-color . :never))
  (setq desktop-globals-to-save '()
        desktop-locals-to-save '()
        desktop-files-not-to-save ".*"
        desktop-buffers-not-to-save ".*"
        desktop-save t)
  (desktop-save-mode))

(use-package evil
  :load-path "~/dev/evil"
  :ensure t
  :defer .1
  :init
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-shift-width 2)
  (setq evil-ex-substitute-global t)
  :config
  (evil-mode)

  (message
   "Loading evil-mode...done (%.3fs)"
   (float-time (time-subtract (current-time) emacs-start-time)))

  ;; auto-clear highlight
  (defun evgeni-nohighlight-hook (&rest _)
    (unless (memq this-command '(evil-ex-search-next evil-ex-search-previous evil-change self-insert-command evil-normal-state evil-repeat))
      (remove-hook 'pre-command-hook 'evgeni-nohighlight-hook 'local)
      (evil-ex-nohighlight)))

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

  (defun evgeni-window-vsplit ()
    (interactive)
    (split-window-horizontally)
    (windmove-right)
    (balance-windows))

  (define-key evil-normal-state-map (kbd "RET") 'evgeni-save-file)
  (define-key evil-normal-state-map (kbd "[ Q") 'first-error)
  (define-key evil-normal-state-map (kbd "] q") 'next-error)
  (define-key evil-normal-state-map (kbd "[ q") 'previous-error)
  (define-key evil-normal-state-map (kbd ", w") 'evgeni-window-vsplit)

  (define-key evil-normal-state-map (kbd "C-c C-b")'ido-switch-buffer)

  (define-key evil-normal-state-map (kbd "] SPC")(lambda (count) (interactive "p") (dotimes (_ count) (save-excursion (evil-insert-newline-below)))))
  (define-key evil-normal-state-map (kbd "[ SPC")(lambda (count) (interactive "p") (dotimes (_ count) (save-excursion (evil-insert-newline-above)))))

  (define-key evil-normal-state-map (kbd "[ m")'beginning-of-defun)
  (define-key evil-normal-state-map (kbd "] m")'end-of-defun)
  (define-key evil-normal-state-map (kbd "] M")'end-of-defun)
  (define-key evil-motion-state-map (kbd "0") 'evil-first-non-blank)
  (define-key evil-normal-state-map (kbd "g n") (lambda () (interactive) (evil-ex "%normal ")))
  (define-key evil-visual-state-map (kbd "g n") (lambda () (interactive) (evil-ex "'<,'>normal ")))

  ;; '*' should not move point
  (evil-define-motion evgeni-star (count &optional symbol)
   :jump t
   :type exclusive
   (interactive (list (prefix-numeric-value current-prefix-arg)
                      evil-symbol-word-search))
   (save-excursion
     (evil-ex-search-word-forward count symbol)))

  (evil-define-motion evgeni-g-star (count &optional symbol)
    :jump t
    :type exclusive
    (interactive (list (prefix-numeric-value current-prefix-arg)
                       evil-symbol-word-search))
    (save-excursion
      (evil-ex-search-unbounded-word-forward count symbol)))

  (define-key evil-motion-state-map "*" 'evgeni-star)
  (define-key evil-motion-state-map "g*" 'evgeni-g-star)

  (defvar evgeni-conflict-marker-regex "^[<=>|]\\{7\\}")

  ;; alias :On with :on
  (evil-ex-define-cmd "On" "on")

  ;; :source
  (evil-ex-define-cmd "so[urce]" 'evgeni-source)
  (evil-define-command evgeni-source (&optional file)
    (interactive "<f>")
    (if file
        (load-file file)
      (load-file (concat user-emacs-directory "init.el"))))

  (define-key evil-normal-state-map "Y" (lambda () (interactive) (evil-yank (point) (point-at-eol))))

  ;; navigate b/w emacs windows and tmux panes
  (defun evgeni-window-navigate (emacs-cmd tmux-cmd)
    (condition-case nil
        (funcall emacs-cmd)
      (error (if (getenv "TMUX") (let ((default-directory "~"))
                                   (shell-command-to-string tmux-cmd))))))
  (bind-key* "C-h" (lambda () (interactive) (evgeni-window-navigate 'windmove-left  "tmux select-pane -L")))
  (bind-key* "C-j" (lambda () (interactive) (evgeni-window-navigate 'windmove-down  "tmux select-pane -D")))
  (bind-key* "C-k" (lambda () (interactive) (evgeni-window-navigate 'windmove-up    "tmux select-pane -U")))
  (bind-key* "C-l" (lambda () (interactive) (evgeni-window-navigate 'windmove-right "tmux select-pane -R")))

  ;; insert state
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-insert-state-map (kbd "C-u") (lambda () (interactive) (evil-delete (point-at-bol) (point))))
  (define-key evil-insert-state-map (kbd "C-x s") 'complete-symbol)
  (define-key evil-insert-state-map (kbd "C-a") 'evgeni-beginning-of-line)
  (define-key evil-insert-state-map (kbd "TAB") 'evgeni-tab-or-complete-previous)

  (defun evgeni-tab-or-complete-previous ()
    (interactive)
    (if (looking-at "\\_>") (call-interactively 'evil-complete-previous) (indent-for-tab-command)))

  (defun evgeni-beginning-of-line ()
    (interactive)
    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (move-beginning-of-line 1))))

  ;; expand lines
  (define-key evil-insert-state-map (kbd "C-l") 'evil-complete-next-line)
  (define-key evil-insert-state-map (kbd "C-x C-l") 'evil-complete-next-line)

  ;; completion

  (define-key evil-insert-state-map (kbd "C-x C-x")'completion-at-poin)
  (define-key evil-insert-state-map (kbd "C-k")'completion-at-point)
  (define-key evil-insert-state-map (kbd "C-x C-]")'complete-tag)
  (define-key evil-insert-state-map (kbd "C-]")'complete-tag)

  ;; complete file paths
  (define-key evil-insert-state-map (kbd "C-x C-f")(lambda ()
                                                     (interactive)
                                                     (let ((hippie-expand-try-functions-list '(try-complete-file-name try-complete-file-name-partially)))
                                                       (hippie-expand nil))))

  ;; C-d to either shift line or delete char
  (define-key evil-insert-state-map (kbd "C-d") (lambda ()
                                                  (interactive)
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
  (define-key evil-normal-state-map (kbd "C-c o c") 'hl-line-mode)
  (define-key evil-normal-state-map (kbd "C-c o w") 'visual-line-mode)
  (define-key evil-normal-state-map (kbd "C-c o k") 'toggle-input-method)
  (define-key evil-normal-state-map (kbd "C-c o h") 'auto-fill-mode)

  ;; auto-fill on dot
  (aset auto-fill-chars ?. t)

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

  (defun evgeni-prev-or-prev-and-backspace ()
    (interactive)
    (when (not (thing-at-point 'line t))
      (call-interactively 'previous-complete-history-element)
      (call-interactively 'move-end-of-line))
    (call-interactively 'evil-ex-delete-backward-char))

  (defun evgeni-ex-delete-or-complete ()
    (interactive)
    (if (eq (point) (point-at-eol))
        (evil-ex-completion)
      (delete-char 1)))

  ;; tweak search and ex maps
  (define-key evil-ex-search-keymap "\C-e"  'evgeni-prev-or-move-end-of-line)
  (define-key evil-ex-completion-map "\C-e" 'evgeni-prev-or-move-end-of-line)
  (define-key evil-ex-completion-map (kbd "DEL" ) 'evgeni-prev-or-prev-and-backspace)
  (define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
  (define-key evil-ex-completion-map "\C-b" 'backward-char)
  (define-key evil-ex-completion-map "\C-d" 'evgeni-ex-delete-or-complete)

  (define-key evil-normal-state-map "zz" 'recenter-top-bottom)

  (define-key evil-list-view-mode-map (kbd "q") #'kill-buffer-and-window)

  (evil-define-operator evgeni-run-macro (beg end &optional macro)
    :move-point nil
    (interactive "<r><a>")
    (evil-ex-normal beg end
                    (concat "@"
                            (single-key-description
                             (or macro (read-char "@-"))))))

  (define-key evil-visual-state-map (kbd "@") 'evgeni-run-macro)
  (define-key evil-normal-state-map (kbd "g@") 'evgeni-run-macro)

  ;; TODO not working
  (evil-define-operator evgeni-visual-dot-repeat (beg end)
    (interactive "<r>")
    (evil-exit-visual-state)
    (evil-ex-normal beg end "."))
  (define-key evil-visual-state-map (kbd ".") 'evgeni-visual-dot-repeat)

  ;; other packages which integrate with evil

  (use-package hippie-exp
    :disabled t
    :bind (:map evil-insert-state-map
                ("TAB" . evgeni-tab))
    :config

    (setq hippie-expand-try-functions-list '(try-expand-dabbrev-visible
                                             try-expand-dabbrev
                                             try-expand-dabbrev-all-buffers
                                             try-expand-dabbrev-from-kill
                                             try-expand-tag))

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
    (define-key evil-insert-state-map (kbd "C-x C-y") 'evgeni-hippie-expand-yasnippet)
    (define-key evil-insert-state-map (kbd "C-x TAB") 'evgeni-hippie-expand-yasnippet))

  (use-package ace-window
    :ensure t
    :bind (:map evil-normal-state-map
                ("C-w C-w" . ace-window)))

  (use-package avy
    :ensure t
    :defer 3
    :bind (:map evil-motion-state-map
                ("gh" . avy-goto-char-timer)
                ("gY" . avy-copy-region)
                ("gyy" . avy-copy-line))
    :config

    (setq avy-timeout-seconds 1.2)

    (custom-set-faces
     '(avy-lead-face-0 ((t (:inherit 'highlight))))
     '(avy-lead-face ((t (:inherit 'highlight)))))

    ;; TODO
    (evil-define-text-object evgeni--avy-line (count &optional beg end type)
      (save-excursion
        (let* ((avy-all-windows nil)
               (beg (avy--line))
               (end (save-excursion
                      (goto-char beg)
                      (move-end-of-line count)
                      (point))))
          (evil-range beg end 'line :expanded t))))

    (evil-define-text-object evgeni--avy-region (count &optional beg end type)
      (save-excursion
        (let* ((beg (save-selected-window
                      (avy--line count)))
               (end (avy--line count)))
          (evil-range beg end 'line :expanded t))))

    (define-key evil-inner-text-objects-map "l" 'evgeni--avy-line)
    (define-key evil-outer-text-objects-map "l" 'evgeni--avy-region))

  (use-package evil-surround
    :ensure t
    :commands
    (evil-surround-edit
     evil-Surround-edit
     evil-surround-region
     evil-Surround-region)
    :init
    (evil-define-key 'operator global-map "s" 'evil-surround-edit)
    (evil-define-key 'operator global-map "S" 'evil-Surround-edit)
    (evil-define-key 'visual global-map "S" 'evil-surround-region)
    (evil-define-key 'visual global-map "gS" 'evil-Surround-region))

  (use-package evil-lion
    :ensure t
    :bind (:map evil-normal-state-map
                ("g l " . evil-lion-left)
                ("g L " . evil-lion-right)
                :map evil-visual-state-map
                ("g l " . evil-lion-left)
                ("g L " . evil-lion-right)))

  (use-package evil-commentary
    :ensure t
    :diminish 'evil-commentary-mode
    :bind (:map evil-normal-state-map
                ("gc" . evil-commentary))

    :config
    ;; define `gcu' to uncomment
    (define-key evil-operator-state-map "u" 'evgeni-commentary-uncomment-adjacent)

    (defun evgeni-commentary-uncomment-adjacent ()
      (interactive)
      (message "hello3")
      (when (and (eq evil-this-operator 'evil-commentary))
        (setq evil-inhibit-operator t)

        (let ((beg (evil-commentary/ensure-in-comment-block (point) (point) nil))
              (end (evil-commentary/ensure-in-comment-block (point) (point) t)))
          (message "%s %s" beg end)
          (evil-commentary (car beg) (cadr end)))))

    ;; copied from https://github.com/linktohack/evil-commentary/issues/9
    (defun evil-commentary/ensure-in-comment-block (beg end forward)
      (save-excursion
        (beginning-of-line)
        (if (not (or (looking-at-p (concat "\\s\\*" (regexp-quote comment-start)))
                     (looking-at-p (concat "\\s\\*$"))))
            (list beg end)
          (let ((saved-beg (point))
                (saved-end (point-at-eol)))
            (catch 'bound
              (when (<= saved-beg (point-min))
                (throw 'bound (list saved-beg end)))
              (when (>= saved-end (point-max))
                (throw 'bound (list beg saved-end)))
              (if forward
                  (next-line)
                (previous-line))
              (apply #'evil-commentary/ensure-in-comment-block
                     (if forward
                         (list beg saved-end forward)
                       (list saved-beg end forward)))))))))

  (use-package evil-exchange
    :ensure t
    :bind (:map evil-normal-state-map
                ("gx" . evil-exchange)
                ("gX" . evil-exchange-cancel)))

  (use-package evil-replace-with-register
    :ensure t
    :bind (:map evil-normal-state-map
                ("gr" . evil-replace-with-register)
                :map evil-visual-state-map
                ("gr" . evil-replace-with-register))
    :config
    (setq evil-replace-with-register-indent t))

  (use-package evil-visualstar
    :ensure t
    :bind (:map evil-visual-state-map
                ("*" . evil-visualstar/begin-search-forward)
                ("#" . evil-visualstar/begin-search-backward)))

  (use-package evil-indent-plus ;; indent object
    :ensure t
    :defer 4
    :config (evil-indent-plus-default-bindings)
    ;; temprorary fix until this is addressed https://github.com/TheBB/evil-indent-plus/pull/4
    (defun evil-indent-plus--linify (range)
      (let ((nbeg (save-excursion (goto-char (cl-first range)) (point-at-bol)))
            (nend (save-excursion (goto-char (cl-second range)) (+ (point-at-eol) (if (evil-visual-state-p) 0 1)))))
        (evil-range nbeg nend 'line))))

  (use-package dired
    :bind (:map evil-normal-state-map
                ("-" . evgeni-dired-current-dir))
    :config
    (unbind-key "SPC" dired-mode-map)
    (setq dired-listing-switches "-alh"
          dired-auto-revert-buffer t
          dired-dwim-target t
          dired-use-ls-dired nil)
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)
    (evil-define-key 'normal dired-mode-map
      "n" (lookup-key evil-motion-state-map "n")
      "g" (lookup-key evil-motion-state-map "g")
      "gr" 'revert-buffer)

    (defun evgeni-dired-current-dir ()
      (interactive)
      (dired ".")))

  (use-package magit
    :ensure t
    :bind (:map evil-normal-state-map
                ("U U" . magit-status)
                ("U w" . magit-stage-file)
                ("U d" . evgeni-magit-diff-unstaged-buffer-file)
                ("U L" . magit-log-head)
                ("U l" . magit-log-buffer-file)
                ("U r" . magit-file-checkout)
                ("U r" . evgeni-magit-file-checkout)
                ("U c" . magit-commit-popup)
                ("U b" . magit-branch-popup)
                ("U z" . magit-stash-popup)
                ("U p" . magit-push-popup)
                ("U f" . magit-fetch-popup)
                ("U F" . magit-pull-popup)
                ("U B" . magit-checkout))
    :commands (magit-find-file)
    :init
    (ex! "magit" 'magit-status)
    (ex! "gedit" 'magit-find-file)
    :config

    (defun evgeni-magit-diff-unstaged-buffer-file ()
      (interactive)
      (magit-diff-unstaged nil (list (magit-file-relative-name))))

    (defun evgeni-magit-file-checkout ()
      (interactive)
      (magit-file-checkout "HEAD" (magit-file-relative-name)))

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

    (define-key magit-dispatch-popup-map "/" 'isearch-forward)
    (define-key magit-dispatch-popup-map "?" 'isearch-backward)

    (evil-define-key 'insert git-commit-mode-map
      (kbd "C-x C-x") 'evgeni-insert-ticket-number-from-git-branch
      (kbd "C-x x") 'evgeni-insert-ticket-number-from-git-branch)

    (defun evgeni-insert-ticket-number-from-git-branch ()
      (interactive)
      (let ((number-from-branch
             (let ((str (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))
               (when (string-match "\\([0-9]+\\)" str)
                 (match-string-no-properties 0 str)))))
        (when number-from-branch
          (insert number-from-branch))))

    (add-hook 'git-commit-mode-hook 'flyspell-mode))

  (use-package smerge-mode
    :defer t
    :config
    (evil-define-minor-mode-key 'normal 'smerge-mode (kbd "] n") 'smerge-next)
    (evil-define-minor-mode-key 'normal 'smerge-mode (kbd "[ n") 'smerge-prev)
    (evil-define-minor-mode-key 'normal 'smerge-mode (kbd "C-c C-c") 'smerge-keep-current)
    (evil-define-minor-mode-key 'normal 'smerge-mode (kbd "C-c C-k") 'smerge-kill-current))

  (use-package vdiff
    :ensure t
    :defer t
    :init
    (ex! "diff-visible-buffers" (lambda ()
                                  (interactive)
                                  (let ((bufs (mapcar 'window-buffer (window-list)))
                                        (vdiff-2way-layout-function (lambda (&rest _args) nil)))
                                    (cond
                                     ((eq (length bufs) 2)
                                      (apply 'vdiff-buffers bufs))
                                     ((eq (length bufs) 3)
                                      (apply 'vdiff-buffers3 bufs))
                                     (t
                                      (user-error "Expected 2 or 3 buffers, got %s" (length bufs)))))))

    (evil-define-minor-mode-key 'normal 'vdiff-mode "]c" 'vdiff-next-hunk)
    (evil-define-minor-mode-key 'normal 'vdiff-mode "[c" 'vdiff-previous-hunk)
    (evil-define-minor-mode-key 'motion 'vdiff-mode "go" 'vdiff-receive-changes)
    (evil-define-minor-mode-key 'motion 'vdiff-mode "gp" 'vdiff-send-changes)
    (evil-define-minor-mode-key 'normal 'vdiff-mode "q" 'evgeni-vdiff-close-if-readonly)

    :config
    (setq vdiff-auto-refine t)
    (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)

    (defun evgeni-vdiff-close-if-readonly ()
      (interactive)
      (if buffer-read-only
          (vdiff-quit)
        (call-interactively 'evil-record-macro))))

  (use-package vdiff-magit
    :ensure t
    :after magit
    :init
    (ex! "gdiff" 'vdiff-magit-show-unstaged)
    :commands vdiff-magit-show-unstaged
    :bind (:map evil-normal-state-map
                ("U D" . vdiff-magit-popup))
    :config
    (setq vdiff-magit-stage-is-2way nil) ;; Only use two buffers (working file and index) for vdiff-magit-stage
    (define-key magit-mode-map "e" 'vdiff-magit-dwim)
    (define-key magit-mode-map "E" 'vdiff-magit-popup)
    (setcdr (assoc ?e (plist-get magit-dispatch-popup :actions))
            '("vdiff dwim" 'vdiff-magit-dwim))
    (setcdr (assoc ?E (plist-get magit-dispatch-popup :actions))
            '("vdiff popup" 'vdiff-magit-popup)))

  (use-package org
    :mode (("\\.org$"   . org-mode))
    :defer t
    :ensure t
    :config
    (org-babel-do-load-languages
     'org-babel-load-languages '((shell . t)))

    (evil-ex-define-cmd "cap[ture]" 'org-capture)

    (setq org-startup-folded "showall")
    (setq org-confirm-babel-evaluate nil)

    (evil-define-key '(normal insert) org-mode-map
      (kbd "TAB") 'org-cycle)

    (use-package org-evil
      :ensure t)

    (use-package org-bullets
      :ensure t
      :after org
      :config
      (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

    (use-package evil-org
      :ensure t
      :after org
      :config
      (add-hook 'org-mode-hook 'evil-org-mode)
      (add-hook 'org-mode-hook (lambda ()
                                 (evil-org-mode)
                                 (evil-org-set-key-theme '(operators))))))

  (use-package move-text
    :ensure t
    :bind (:map evil-normal-state-map
                ("] e" . evgeni-move-text-line-up )
                ("[ e" . evgeni-move-text-line-down ))
    :config
    (defun evgeni-move-text-line-up (count)
      (interactive "p")
      (let ((count (or count 1)))
        (dotimes (i count)
          (move-text-line-down))))

    (defun evgeni-move-text-line-down (count)
      (interactive "p")
      (let ((count (or count 1)))
        (dotimes (i count)
          (move-text-line-up)))))

  (use-package xref
    :bind (:map evil-normal-state-map
                ("C-]" . xref-find-definitions)
                ("g C-]" . xref-find-definitions)
                ("C-w C-]" . xref-find-definitions-other-window))
    :config
    (ex! "tag" (lambda ()
                 (interactive)
                 ;; make `xref-find-definitions prompt for a tag
                 (let ((current-prefix-arg '(4)))
                   (call-interactively 'xref-find-definitions))))
    (evil-define-key 'normal xref--xref-buffer-mode-map "q" 'delete-window
      "C-n" 'xref-next-line
      "C-p" 'xref-prev-line))

  (use-package linum
    :bind (:map evil-normal-state-map
                ( "C-c o n" . linum-mode))
    :config
    (setq linum-format "%d "))

  (use-package smart-compile
    :ensure t
    :commands smart-compile
    :bind (:map evil-normal-state-map
                ("g m" . smart-compile)
                ("g RET" . smart-compile))
    :config
    (setq compilation-read-command nil)
    (setq compilation-ask-about-save nil)
    (setq smart-compile-check-makefile nil)
    (setq smart-compile-alist '((cperl-mode . "perl -w -Mstrict -c %f")
                                (perl-mode . "perl -w -Mstrict -c %f")
                                (haskell-mode . "stack --silent ghc -- -e :q %f")
                                ("docker-compose*\\.yml$" . "docker-compose -f %f config -q")
                                (emacs-lisp-mode    . (emacs-lisp-byte-compile))))

    (defun evgeni-close-compile-win-if-successful (buffer string)
      (if (and
           (eq major-mode 'compilation-mode)
           (buffer-live-p buffer)
           (string-match "compilation" (buffer-name buffer))
           (string-match "finished" string)
           (<= (line-number-at-pos (point-max)) 8))
          (delete-windows-on buffer)
        (with-selected-window (get-buffer-window buffer)
          (goto-char (point-min))
          (scroll-up-line 4))))

    (add-hook 'compilation-finish-functions 'evgeni-close-compile-win-if-successful)

    (ex! "mak[e]" 'smart-compile)
    )

  (use-package ivy
    :ensure t
    :diminish 'ivy-mode
    :defer .5
    :bind (:map evil-normal-state-map
                ("SPC" . ivy-switch-buffer))
    :config
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-virtual-abbreviate 'full)
    (define-key ivy-minibuffer-map (kbd "C-w") 'backward-kill-word)
    (define-key ivy-minibuffer-map (kbd "C-u") (lambda () (interactive) (kill-region (point) (point-at-bol))))
    (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
    (define-key ivy-minibuffer-map (kbd "C-c o") 'ivy-occur)

    ;; C-r C-w to read word at point
    (unbind-key "C-r" ivy-minibuffer-map)
    (define-key ivy-minibuffer-map (kbd "C-r C-w") 'ivy-next-history-element)

    ;; use different colors in ivy-switch-buffer
    (setq ivy-switch-buffer-faces-alist
          '((emacs-lisp-mode . ivy-minibuffer-match-face-1)
            (dired-mode . ivy-subdir)
            (org-mode . org-level-4))))

  (use-package swiper
    :ensure t
    :bind (:map evil-motion-state-map
                ("C-c C-r" . ivy-resume)
                ("/" . swiper))
    :config
    (setq swiper-goto-start-of-match t)

    (defun evgeni-swiper-to-evil-iedit ()
      (interactive)
      (unless (require 'evil-iedit-state nil t)
        (error "evil-iedit-state isn't installed"))
      (unless (window-minibuffer-p)
        (error "Call me only from `swiper'"))
      (let ((cands (nreverse ivy--old-cands)))
        (unless (string= ivy-text "")
          (ivy-exit-with-action
           (lambda (_)
             (setq mark-active nil)
             (run-hooks 'deactivate-mark-hook)
             (when iedit-mode
               (iedit-cleanup))
             (let* ((regexp (ivy--regex ivy-text))
                    (iedit-case-sensitive (not (and ivy-case-fold-search
                                                    (string= regexp (downcase regexp))))))
               (iedit-start regexp (point-min) (point-max))
               (cond ((not iedit-occurrences-overlays)
                      (message "No matches found for %s" regexp)
                      (iedit-done))
                     ((not (iedit-same-length))
                      (message "Matches are not the same length.")
                      (iedit-done))
                     (t
                      (evil-iedit-state)))))))))

    (defun evgeni-swiper-to-global-normal-command ()
      (interactive)
      (if (null (window-minibuffer-p))
          (user-error "Should only be called in the minibuffer through `swiper-map'")
        (lexical-let ((enable-recursive-minibuffers t)
                      (regex (ivy--regex ivy-text)))
          (swiper--cleanup)
          (ivy-exit-with-action
           (lambda (_)
             (with-ivy-window
               (evil-ex (format "g/%s/norm " regex))))))))

    (define-key swiper-map (kbd "C-c C-c") 'evgeni-swiper-to-evil-iedit)
    (define-key swiper-map (kbd "C-c C-e") 'evgeni-swiper-to-global-normal-command))

  (use-package counsel
    :ensure t
    :init
    (ex! "faces" 'counsel-faces)
    (ex! "find-lib" 'counsel-find-library)
    :bind (([remap find-file] . counsel-find-file)
           ([remap describe-function] . counsel-describe-function)
           ([remap describe-variable] . counsel-describe-variable)
           ([remap execute-extended-command] . counsel-M-x)
           :map evil-motion-state-map
           ("g /"   . counsel-git-grep)
           :map evil-normal-state-map
           (", f"   . evgeni-imenu)
           ("K"     . evgeni-counsel-git-grep)
           ("g P" . counsel-yank-pop)
           :map evil-visual-state-map
           ("K"     . evgeni-counsel-git-grep))
    :config
    (setq counsel-git-cmd "git ls-files --cached --others --exclude-standard")
    (setq counsel-git-grep-skip-counting-lines t)

    (define-key counsel-describe-map (kbd "C-]") 'counsel-find-symbol)
    (define-key counsel-git-grep-map (kbd "C-c C-c") 'ivy-occur)
    (define-key ivy-occur-grep-mode-map (kbd "C-c C-c") 'wgrep-change-to-wgrep-mode)

    (defun evgeni-counsel-git-grep ()
      (interactive)
      (counsel-git-grep nil (substring-no-properties
                             (if (region-active-p)
                                 (buffer-substring (region-beginning) (region-end))
                               (thing-at-point 'word)))))

    (evil-define-motion evgeni-imenu ()
      "Evil motion for `imenu'."
      :type exclusive :jump t :repeat abort
      (evil-without-repeat
        (call-interactively 'counsel-imenu))))

  (use-package imenu-anywhere
    :ensure t
    :bind (:map evil-normal-state-map
                (", F"  . imenu-anywhere)))

  (use-package winner
    :defer 5
    :bind (:map evil-normal-state-map ("z u" . hydra-winner/body))
    :init
    (ex! "winner" 'hydra-winner/body)
    :config
    (winner-mode)
    (defhydra hydra-winner ()
      "Winner"
      ("u" winner-undo "undo")
      ("r" winner-redo "redo")))

  (use-package loccur
    :ensure t
    :bind (:map evil-normal-state-map
                (", *" . loccur-current)
                :map evil-visual-state-map
                (", *" . loccur))
    :config
    (setq loccur-highlight-matching-regexp nil)
    (evil-define-minor-mode-key 'normal 'loccur-mode
      (kbd "q") 'loccur
      (kbd "RET") 'loccur
      (kbd "<escape>") 'loccur)

    )

  (use-package iedit
    :ensure t
    :bind (:map evil-normal-state-map
                (", m" . iedit-mode)
                :map evil-visual-state-map
                (", m" . iedit-mode))
    :config
    (defun evgeni-iedit-restrict-to-region (beg end)
      (interactive "r")
      (iedit-restrict-region beg end))
    (evil-define-minor-mode-key 'normal 'iedit-mode
      (kbd "C-c f") 'iedit-restrict-function
      (kbd "C-c C-f") 'iedit-restrict-function
      (kbd "<escape>") 'iedit-mode)
    (evil-define-minor-mode-key 'visual 'iedit-mode
      (kbd "C-c f") 'evgeni-iedit-restrict-to-region
      (kbd "C-c C-f") 'evgeni-iedit-restrict-to-region))

  (use-package idle-highlight-mode
    :disabled t
    :commands idle-highlight-mode
    :ensure t
    :bind (:map evil-normal-state-map
                ("C-c o h" . idle-highlight-mode)))

  (use-package projectile
    :ensure t
    :defer 3
    :bind (:map evil-motion-state-map
                ("g SPC"   . evgeni-projectile-find-file))
    :init
    (ex! "proj[ectile]" 'projectile-switch-project)
    :config
    ;; TODO switch to using SPC only, no g-SPC
    ;; should then be able to switch to different kinds of filtering
    ;; (filter buffers, projects, projects files)
    (defun evgeni-projectile-find-file ()
      (interactive)
      (if (projectile-project-p)
          (find-file-in-project)
        (projectile-switch-project)))

    (setq projectile-switch-project-action
          (lambda ()
            (dired (projectile-project-root))))
    (setq projectile-mode-line (format " [%s]" (projectile-project-name)))
    (setq projectile-completion-system 'default)
    (projectile-global-mode))

  (use-package find-file-in-project
    :ensure t
    :commands find-file-in-project
    :config
    (add-to-list 'ffip-prune-patterns "*/.elpa/*" t)))

(use-package evil-expat
  :load-path "~/dev/evil-expat"
  :defer 1)

(use-package evil-goggles
  :defer 1
  :ensure t
  :load-path "~/dev/evil-goggles"
  :config

  (setq evil-goggles-duration 0.100)
  (setq evil-goggles-pulse (display-graphic-p))
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces)
  ;; (evil-goggles-use-magit-faces)
  )

(use-package recentf
  :defer 1
  :init
  (setq recentf-max-saved-items 500)
  (setq recentf-exclude '("/tmp/" "/ssh:"))
  (setq recentf-auto-cleanup 'never)
  :config
  (recentf-mode))

(use-package saveplace
  :defer 1
  :config
  (save-place-mode))

(use-package savehist
  :defer 1
  :init
  (setq
   enable-recursive-minibuffers t ; Allow commands in minibuffers
   history-length 1000
   savehist-additional-variables '(mark-ring
                                   global-mark-ring
                                   search-ring
                                   regexp-search-ring
                                   extended-command-history)
   savehist-autosave-interval 60)
  (savehist-mode t))

(use-package dired-single
  :ensure t
  :after dired
  :config
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map (kbd "RET") 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map "-" (lambda nil (interactive) (dired-single-buffer ".."))))

(use-package wdired
  :commands wdired-change-to-wdired-mode
  :init
  (ex! "wdired" 'wdired-change-to-wdired-mode))

(use-package evil-magit
  :ensure t
  :after magit
  :init
  (setq evil-magit-want-horizontal-movement t))

(use-package git-timemachine
  :ensure t
  :defer t
  :init
  (ex! "timemachine" 'git-timemachine)
  (ex! "timemachine-switch-branch" 'git-timemachine-switch-branch)
  :config

  (evil-define-minor-mode-key 'normal 'git-timemachine-mode
    "c" 'git-timemachine-show-current-revision
    "g" 'git-timemachine-show-nth-revision
    "p" 'git-timemachine-show-previous-revision
    "n" 'git-timemachine-show-next-revision
    "N" 'git-timemachine-show-previous-revision
    "Y" 'git-timemachine-kill-revision
    "q" 'git-timemachine-quit))

(use-package flycheck
  :ensure t
  :bind (:map evil-normal-state-map
              ( "C-c o f" . flycheck-mode))
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (setq-default flycheck-disabled-checkers '(perl-perlcritic)))

(use-package abbrev
  :defer t
  :diminish 'abbrev-mode
  :init
  (setq-default abbrev-mode t))

(use-package grep
  :commands grep-mode
  :defer t
  :config
  (unbind-key "h" grep-mode-map)
  (unbind-key "n" grep-mode-map))

(use-package wgrep
  :ensure t
  :commands wgrep-change-to-wgrep-mode
  :init
  (setq wgrep-auto-save-buffer t)
  (ex! "wgrep" 'wgrep-change-to-wgrep-mode)
  :config
  (custom-set-faces
   '(wgrep-face ((t (:inherit 'underline)))) ;; *Face used for the changed text in the grep buffer.
   '(wgrep-file-face ((t (:inherit 'underline)))) ;; *Face used for the changed text in the file buffer.
   '(wgrep-delete-face ((t (:inherit 'isearch-fail)))))) ;; *Face used for the deleted whole line in the grep buffer.

(use-package compile
  :defer 3
  :config
  (setq compilation-scroll-output 'next-error)
  (setq compilation-read-command nil)
  (setq compilation-ask-about-save nil))

(use-package aggressive-indent
  :ensure t
  :disabled t
  :commands aggressive-indent-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

(use-package shackle
  :ensure t
  :defer 3
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
          (ivy-occur-grep-mode         :align below :size 10))))

(use-package smex
  :ensure t
  :after counsel)

(use-package imenu
  :defer t
  :config
  (setq imenu-auto-rescan-maxout 600000))

(use-package elisp-mode
  :defer t
  :config
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)

  (defun evgeni-elisp-eval ()
    (interactive)
    (if (region-active-p)
        (call-interactively 'eval-region)
      (call-interactively 'eval-buffer)))

  (ex! "eval" 'evgeni-elisp-eval)

  (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
  (modify-syntax-entry ?. "w" emacs-lisp-mode-syntax-table)
  (modify-syntax-entry ?! "w" emacs-lisp-mode-syntax-table))

(use-package intero
  :ensure t
  :commands intero-mode)

(use-package whitespace
  :defer t
  :init
  (ex! "clean-whitespace" 'whitespace-cleanup)
  :init
  (defun evgeni-show-trailing-whitespace () (setq show-trailing-whitespace t))
  (add-hook 'prog-mode-hook 'evgeni-show-trailing-whitespace)
  :config
  (setq whitespace-style '(face tabs trailing))
  (set-face-attribute 'trailing-whitespace nil
                      :background
                      (face-attribute 'mode-line-inactive :background)))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :disabled t
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
  :mode (("\\.pl$"   . perl-mode)
         ("\\.pm$"   . perl-mode)
         ("\\.psgi$" . perl-mode)
         ("\\.t$"    . perl-mode)
         ("\\.html$" . perl-mode))
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
                      ;; (insert (concat "use Data::Dump qw(pp); warn '" word ": ' . pp($" word ") . \"\\n\";"))
                      (insert (concat "use Data::Dumper; warn '" word ": ' . Dumper($" word ") . \"\\n\";")))))

  (defun evgeni-define-perl-function ()
    (interactive)
    (let ((function-name-at-point (thing-at-point 'word)))
      (end-of-defun)
      (insert "\nsub " function-name-at-point " {\n}\n")
      (evil-previous-line)
      (evil-open-above 1)))

  (evil-define-key 'normal perl-mode-map
    (kbd "C-c f") 'evgeni-define-perl-function
    (kbd "] d" ) 'evgeni-perl-dump)

  (modify-syntax-entry ?_ "w" perl-mode-syntax-table)

  (add-hook 'perl-mode-hook #'(lambda ()
                                ;; taken from cperl-mode, used for beginning-of-defun / end-of-defun
                                (setq defun-prompt-regexp
                                      "^[ 	]*\\(\\(?:sub\\)\\(\\([ 	\n]\\|#[^\n]*\n\\)+\\(::[a-zA-Z_0-9:']+\\|[a-zA-Z_'][a-zA-Z_0-9:']*\\)\\)\\([ 	\n]*\\(#[^\n]*\n[ 	\n]*\\)*\\(([^()]*)\\)\\)?\\([ 	\n]*\\(#[^\n]*\n[ 	\n]*\\)*\\(:\\([ 	\n]*\\(#[^\n]*\n[ 	\n]*\\)*\\(\\sw\\|_\\)+\\((\\(\\\\.\\|[^\\\\()]\\|([^\\\\()]*)\\)*)\\)?\\([ 	\n]*\\(#[^\n]*\n[ 	\n]*\\)*:\\)?\\)+\\)\\)?\\|\\(BEGIN\\|UNITCHECK\\|CHECK\\|INIT\\|END\\|AUTOLOAD\\|DESTROY\\)\\)[ 	\n]*\\(#[^\n]*\n[ 	\n]*\\)*"))))

(use-package eros
  :ensure t
  :commands (eros-eval-last-sexp eros-eval-defun)
  :init
  (define-key global-map [remap eval-last-sexp] 'eros-eval-last-sexp)
  (define-key global-map [remap eval-defun] 'eros-eval-defun))

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
  :mode "\\.hs\\'"
  :config
  (setq haskell-indent-spaces 2)

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
                 (string= "" (string-trim-right (string-trim-left (buffer-substring (line-beginning-position) (line-end-position)))))))
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

(use-package hindent
  :ensure t
  :after haskell-mode
  :if (executable-find "hindent")
  :init
  (add-hook 'haskell-mode-hook 'hindent-mode))

(use-package smartparens
  :ensure t
  :commands sp-forward-slurp-sexp sp-forward-barf-sexp
  :init
  (define-key emacs-lisp-mode-map (kbd "C-c <") 'sp-forward-slurp-sexp)
  (define-key emacs-lisp-mode-map (kbd "C-c >") 'sp-forward-barf-sexp))

(use-package paren
  :defer 1
  :config
  (setq show-paren-style 'parenthesis
        show-paren-delay 0)
  (show-paren-mode 1))

(use-package ledger-mode
  :ensure t
  :commands ledger-mode)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)     ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*")) ; don't muck with special buffers

(use-package regex-tool
  :ensure t
  :commands regex-tool)

(use-package macrostep
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand))
  :config
  (evil-define-minor-mode-key 'normal 'macrostep-mode
    "q" 'macrostep-collapse-all
    "o" 'macrostep-expand
    "c" 'macrostep-collapse))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml$" . yaml-mode))
  :config
  (setq yaml-imenu-generic-expression
        '((nil  "^[ ]\\{0,2\\}\\(:?[a-zA-Z_-]+\\):"          1)))
  (modify-syntax-entry ?_ "w" yaml-mode-syntax-table)
  (modify-syntax-entry ?- "w" yaml-mode-syntax-table)
  (modify-syntax-entry ?$ "." yaml-mode-syntax-table))

(use-package package-lint
  :ensure t
  :commands package-lint-current-buffer)

(use-package js
  :ensure t
  :mode ("\\.js\\'" . js-mode)
  :config
  (modify-syntax-entry ?_ "w" js-mode-syntax-table))

(use-package json
  :commands json-pretty-print-buffer
  :init
  (ex! "json-pretty" 'json-pretty-print-buffer))

(use-package rjsx-mode
  :ensure t
  :mode ("\\.jsx\\'" . rjsx-mode))

(use-package css-mode
  :ensure t
  :mode ("\\.css\\'" . css-mode)
  :config
  (modify-syntax-entry ?- "w" css-mode-syntax-table))

(use-package tramp-sh
  :defer t
  :config
  (setq tramp-default-method "ssh"))

(use-package docker-tramp
  :ensure t
  :after tramp
  :config
  (setq docker-tramp-use-names t))

(use-package restclient
  :ensure t
  :mode ("\\.restclient\\'" . restclient-mode))

(use-package dockerfile-mode
  :ensure t
  :mode (".*Dockerfile.*" . dockerfile-mode)
  :config
  (modify-syntax-entry ?$ "." dockerfile-mode-syntax-table))

(use-package suggest
  :ensure t
  :commands suggest)

(use-package conf-mode
  :defer t
  :config
  (modify-syntax-entry ?_ "w" conf-mode-syntax-table))

(use-package nginx-mode
  :ensure t
  :mode ("nginx.*\\.conf\\'" . nginx-mode)
  :config
  (setq nginx-indent-level 3)

  ;; Auto indent on }
  (add-hook 'nginx-mode-hook
            (lambda ()
              (setq-local
               electric-indent-chars (cons ?} (and (boundp 'electric-indent-chars)
                                                   electric-indent-chars))))))

(use-package edebug
  :defer t
  :config
  (add-hook 'edebug-mode-hook 'evil-normalize-keymaps))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :defer 1
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'")

(use-package edit-indirect
  :ensure t
  :defer t)

(use-package sh-script
  :mode (("\\.sh$" . sh-mode))
  :config

  ;; indent after }
  (add-hook 'sh-mode-hook
            (lambda ()
              (setq-local
               electric-indent-chars (cons ?} (and (boundp 'electric-indent-chars)
                                                   electric-indent-chars)))))
  ;; indent after done, fi
  (define-abbrev-table 'sh-mode-abbrev-table
    '(("done" "done" indent-according-to-mode :system t)
      ("fi"  "fi"    indent-according-to-mode :system t))))

(use-package try
  :ensure t
  :commands try)

(use-package zygospore
  :ensure t
  :commands zygospore-toggle-delete-other-windows
  :init
  (ex! "only" 'zygospore-toggle-delete-other-windows))

(use-package hl-todo
  :ensure t
  :defer 3
  :config
  (add-hook 'prog-mode-hook 'hl-todo-mode))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

(use-package diff-hl
  :ensure t
  :defer 2
  :init
  (setq diff-hl-margin-symbols-alist '((insert . " ") (delete . " ") (change . " ") (unknown . "?") (ignored . "i")))
  :config
  (global-diff-hl-mode)
  ;; (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  ;; (add-hook 'text-mode-hook 'turn-on-diff-hl-mode)
  (unless (window-system)
    (diff-hl-margin-mode))
  ;; (diff-hl-flydiff-mode)

  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

  (evil-define-minor-mode-key 'normal 'diff-hl-mode
    "]c" 'diff-hl-next-hunk
    "[c" 'diff-hl-previous-hunk
    "Ux" 'diff-hl-revert-hunk))

(use-package ibuffer
  :defer t
  :init
  (ex! "ls" 'ibuffer)
  :config
  (define-key ibuffer-mode-map "j" 'ibuffer-forward-line)
  (define-key ibuffer-mode-map "k" 'ibuffer-backward-line))

(use-package elec-pair
  :defer t
  :init
  (add-hook 'prog-mode-hook 'electric-pair-local-mode))

(use-package nameless
  :ensure t
  :defer t)

(use-package git-link
  :ensure t
  :defer t
  :config
  (setq git-link-open-in-browser (display-graphic-p)))

(use-package shell-toggle
  :ensure t
  :bind* ("C-c C-z" . shell-toggle)
  :init
  (setq shell-toggle-launch-shell 'shell-toggle-eshell))

(use-package git-commit-insert-issue
  :ensure t
  :defer t
  :config
  (setq gitlab-host "https://gitlab.nccdn.net"
        gitlab-username "edkolev"
        gitlab-password "Kurec2017"))

(use-package hydra
  :ensure t
  :defer t
  :init
  (ex! "font-size" 'hydra-zoom/body)
  :config
  (defhydra hydra-zoom nil
    "zoom"
    ("j" text-scale-decrease "out")
    ("k" text-scale-increase "in")
    ("0" (text-scale-set 0) "reset")))


(use-package clojure-mode
  :ensure t
  :defer t
  :config

  (define-key clojure-mode-map (kbd "C-c <") 'sp-forward-slurp-sexp)
  (define-key clojure-mode-map (kbd "C-c >") 'sp-forward-barf-sexp)

  (use-package monroe
    :ensure t
    :config
    (add-hook 'clojure-mode-hook 'clojure-enable-monroe)))

(use-package dash
  :ensure t
  :defer t
  :config
  (dash-enable-font-lock))

(use-package which-key
  :commands which-key-mode
  :ensure t
  :config
  (setq which-key-idle-delay 0.4))
