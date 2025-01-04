;; -*- lexical-binding: t; -*-

(defconst emacs-start-time (current-time))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed (float-time (time-subtract (current-time)
                                                       emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed)))
          t)

;; tweak GC during startup
(defvar evgeni--file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-timer 5 nil
                                 (lambda ()
                                   (setq gc-cons-threshold 16777216
                                         gc-cons-percentage 0.1
                                         file-name-handler-alist evgeni--file-name-handler-alist)))))

;; bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; graphic settings
(when (display-graphic-p)
  (let ((font-name "Droid Sans Mono"))
    (if (member font-name (font-family-list))
        (set-frame-font (font-spec :family font-name :size 14 :weight 'regular) nil t)
      (user-error "Error, font %s is not available" font-name)))
  (setq-default line-spacing 3))

 ;; emacs mac https://github.com/railwaycat/homebrew-emacsmacport
(when (eq window-system 'mac)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'hyper)
  (global-set-key [(hyper a)] 'mark-whole-buffer)
  (global-set-key [(hyper v)] 'clipboard-yank)
  (global-set-key [(hyper c)] 'clipboard-kill-ring-save)
  (global-set-key [(hyper n)] 'make-frame))

(setq custom-file (expand-file-name "custom-file.el" user-emacs-directory))
(delete-file custom-file)

;; splash
(setq fancy-startup-text '(((lambda()
                              (format "Started in %s" (emacs-init-time))))))
(setq fancy-splash-image "~/Desktop/emacs-icon.png")

(setq backup-inhibited t)
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq suggest-key-bindings nil)
(setq-default truncate-lines t)
(setq blink-cursor-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setf initial-scratch-message ""
      initial-major-mode 'emacs-lisp-mode)
(setq echo-keystrokes 0.02)
(setq scroll-step 2)
(setq-default major-mode 'text-mode)
(setq enable-local-variables :all)
(setq require-final-newline t)
(setq ring-bell-function 'ignore)
(setq visual-line-fringe-indicators '(left-arrow right-arrow))

;; (setq initial-buffer-choice (lambda ()
;;                               (interactive)
;;                               (get-buffer "*Messages*")))

(defun display-startup-echo-area-message ()
  (message ""))
(setq load-prefer-newer t)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'toggle-scroll-bar)
  (toggle-scroll-bar -1))

(setq-default default-frame-alist '((ns-transparent-titlebar . t) (ns-appearance . dark) (vertical-scroll-bars)))
(setq ns-use-proxy-icon nil)
(setq frame-title-format "emacs")

(global-set-key "\C-ch" help-map)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default sentence-end-double-space nil)

;; disable VC
(setq vc-handled-backends '(Git))
;; don't ask when following a symlink to a version-controlled file
(setq vc-follow-symlinks t)

(modify-syntax-entry ?_ "w" (standard-syntax-table))
(modify-syntax-entry ?' "." (standard-syntax-table))
(modify-syntax-entry ?' "." text-mode-syntax-table)
(add-hook 'prog-mode-hook (lambda () (modify-syntax-entry ?_ "w")))

(defun evgeni-maybe-propertize-with-face (text face)
  (if face
      (propertize text 'face face)
    text))

(defun evgeni-word-at-point-or-region ()
  "Return region's text if active, otherwise the word at point."
  (substring-no-properties
   (or
    (if (region-active-p)
        (buffer-substring (region-beginning) (region-end))
      (thing-at-point 'word))
    "")))

;; in modeline, show file path, relative to the project root
(require 'subr-x)
(defun evgeni-buffer-path (&optional path prj-path-face buffer-id-face)
  "Return current buffer path relative to project root.
Return nil if not in a project"
  (when-let* ((prj (project-current nil))
              (path (or path buffer-file-truename))
              (prj-parent (file-name-directory (directory-file-name (expand-file-name (project-root prj))))))
    (concat (evgeni-maybe-propertize-with-face (file-relative-name
                                                (file-name-directory path)
                                                prj-parent)
                                               prj-path-face)
            (evgeni-maybe-propertize-with-face
             (file-name-nondirectory path)
             (or buffer-id-face 'mode-line-buffer-id)))))

(setq-default mode-line-buffer-identification
              '(:eval (format-mode-line (or (evgeni-buffer-path)
                                            (propertized-buffer-identification "%b")))))

(setq tags-revert-without-query t)

;; prompt to create missing dirs
(defun evgeni-create-non-existent-directory ()
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (when (and (not (file-exists-p parent-directory))
                   (y-or-n-p (format "Directory `%s' does not exist. Create it?" parent-directory)))
          (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'evgeni-create-non-existent-directory)

(defmacro ex! (cmd func)
  "Shortcut for defining ex commands"
  `(with-eval-after-load 'evil
     (evil-ex-define-cmd ,cmd ,func)))

;; use-package
(setq use-package-enable-imenu-support t)
(unless (package-installed-p 'use-package)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package))

(if debug-on-error
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

(use-package straight
  :config
  (defalias 'try 'straight-use-package)
  (ex! "upgrade-packages" 'straight-pull-all))

(use-package no-littering
  :straight t
  :demand t)

(use-package dashboard
  :straight t
  :if (display-graphic-p)
  :preface
  (defun evgeni-dashboard-banner ()
    (setq dashboard-banner-logo-title
          (format "Started in %.2f seconds"
                  (float-time (time-subtract after-init-time before-init-time)))))
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  (add-hook 'dashboard-mode-hook 'evgeni-dashboard-banner)
  :config
  (setq dashboard-items '((projects . 5)
                          (recents . 5)
                          (bookmarks . 5)))
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-footer-messages '(""))
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-set-init-info nil)
  (dashboard-setup-startup-hook))

;;; themes

(defmacro theme! (thm &rest customizations)
  (declare (indent 1))
  `(advice-add 'load-theme :after (lambda (theme &optional no-confirm no-enable)
                                   (when (eq theme ,thm)
                                     (custom-theme-set-faces
                                      ,thm
                                      ,@customizations)))))

(defmacro with-theme! (thm code)
  (declare (indent 1))
  `(progn
     (when (memq ,thm custom-enabled-themes)
       (progn ,code))
     (advice-add 'load-theme :after (lambda (theme &optional no-confirm no-enable)
                                      (when (eq theme ,thm)
                                        ,code)))))

(setq custom-safe-themes t)
(use-package habamax-theme :straight t :defer t)
(use-package one-themes :straight t :defer t)        ;; ok
(use-package greymatters-theme :straight t :defer t
  :config
  (let ((region "#e3e4e6"))
    (custom-theme-set-faces
     'greymatters
     `(region ((t (:background ,region)))))))
(use-package chyla-theme :straight t :defer t)       ;; ok, green
(use-package doom-themes :straight t :defer t)
(use-package eclipse-theme :straight t :defer t)
(use-package flatui-theme :straight t :defer t)
(use-package twilight-bright-theme :straight t :defer t)
(use-package espresso-theme :straight t :defer t)
(use-package apropospriate-theme :straight t :defer t)
(use-package material-theme :straight t :defer t)
(use-package tango-plus-theme :straight t :defer t)
(use-package plan9-theme :straight t :defer t)
(use-package tommyh-theme :straight t :defer t)
(use-package modus-themes :straight t :defer t)
(use-package ef-themes :straight t :defer t)
(use-package paper-theme :straight t :defer t)
(use-package hydandata-light-theme :straight t :defer t)
(use-package color-theme-sanityinc-tomorrow :straight t :defer t)
(use-package leuven-theme :defer t
  :init
  (theme! 'leuven
    '(eshell-prompt ((t (:inherit 'font-lock-keyword-face))))
    '(dired-ignored ((t (:inherit 'shadow :strike-through t))))))

(use-package white-sand-theme :straight t :defer t)
(use-package silkworm-theme :straight t :defer t)
(use-package oldlace-theme :straight t :defer t
  :config
  (custom-theme-set-faces
   'oldlace
   `(region ((t (:foreground "#525252" :background "#d0c9bd"))))))

;; dark themes
(use-package dracula-theme :straight t :defer t)

(use-package emacs
  :custom
  (fill-column 95)
  :config
  ;; load theme
  (load-theme (if (display-graphic-p)
                  'leuven ;; 'modus-operandi ;; 'leuven ;; 'doom-one-light ;; 'habamax ;; 'dichromacy ;; 'doom-one-light ;; 'twilight-bright ;; 'apropospriate-dark ;;'tango-dark ;; tango-plus flatui
                'ef-light ;; 'ef-cyprus ;; 'ef-tritanopia-light ;; 'ef-duo-light ;; 'ef-kassio ;; 'ef-summer ;; 'ef-duo-light ;; 'ef-tritanopia-light ;; 'ef-trio-light ;; 'ef-duo-light
                )
              t)
  ;; bind command-option-H to hide other windows, just like every other OS X app
  (when (string-equal system-type "darwin")
    (define-key global-map (kbd "M-s-˙") 'ns-do-hide-others))

  (setq long-line-threshold nil)

  (setq read-process-output-max (* 64 1024)) ;; 64k, instead of 4K (the default)

  (defun evgeni-make-frame ()
    (interactive)
    (make-frame `((top . ,(+ (cdr (frame-position)) 15))
                  (left . ,(+ (car (frame-position)) 15))
                  (width . ,(- (frame-width) 3))
                  (height . ,(- (frame-height) 2))))
    (switch-to-buffer "*scratch*"))

  (define-key global-map (kbd "s-n") 'evgeni-make-frame)

  ;; display flymake buffer at the bottom
  (setq display-buffer-alist nil)
  (add-to-list 'display-buffer-alist
               `(,(lambda (buf act)
                    (member (with-current-buffer buf major-mode) '(flymake-diagnostics-buffer-mode)))
                 (display-buffer--maybe-same-window
                  display-buffer-reuse-window
                  display-buffer-reuse-mode-window
                  display-buffer-at-bottom)
                 (side . bottom)
                 (window-height . 0.20)
                 (quit-restore ('window 'window nil nil)))))

(use-package emacs ;; helpers / tools
  :after evil
  :config
  (ex! "today" 'evgeni-insert-date)
  (ex! "date" 'evgeni-insert-date)
  (evil-define-command evgeni-insert-date (&optional when)
    "Call `date -d today' cmd and insert the response.
If WHEN is specified, pass it like so `date -d WHEN'"
    (interactive "<a>")
    (insert
     (string-trim (shell-command-to-string
                   (concat "date -d'"
                           (or when "today")
                           "' '+%Y-%m-%dT00:00:00'")))))

  (ex! "close-all" 'evgeni-close-all-buffers)
  (defun evgeni-close-all-buffers ()
    "Kill all buffers except current one and buffers which start with *, for example *scratch*"
    (interactive)
    (switch-to-buffer "*scratch*")
    (eglot-shutdown-all)
    (mapc 'kill-buffer (cl-remove-if
                        (lambda (x)
                          (or
                           (string-prefix-p "*" (buffer-name x))))
                        (buffer-list)))))

(use-package emacs
  :config
  ;; https://patorjk.com/software/taag/#p=display&f=Graffiti&t=Type%20Something%20
  (setq-default inhibit-startup-message t)

  (setq initial-scratch-message
        "
   █████  ███▄ ▄███▓ ▄▄▄       ▄████▄    ██████
 ▓█   ▀ ▓██▒▀█▀ ██▒▒████▄    ▒██▀ ▀█  ▒██    ░
 ▒███   ▓██    ▓██░▒██  ▀█▄  ▒▓█    ▄ ░ ▓██▄▄
 ▒▓█  ▄ ▒██    ▒██ ░██▄▄▄▄██ ▒▓▓▄ ▄██▒  ▒   ██▒
 ░▒████▒▒██▒   ░██▒ ▓█   ▓██▒▒ ▓███▀ ░▒██████▒▒
 ░░ ▒░ ░░ ▒░   ░  ░ ▒▒   ▓▒█░░ ░▒ ▒  ░▒ ▒▓▒ ▒ ░
  ░ ░  ░░  ░      ░  ▒   ▒▒ ░  ░  ▒   ░ ░▒  ░ ░
    ░   ░      ░     ░   ▒   ░        ░  ░  ░
    ░  ░       ░         ░  ░░ ░            ░
"))

;; restore frame position - https://github.com/aaronjensen/restore-frame-position
(when (display-graphic-p)
  (setq restore-frame-position-file (expand-file-name "frame-position.el" no-littering-var-directory))

  ;; smooth scrolling, i.e. scroll by pixels, instead of lines
  (pixel-scroll-precision-mode)

  (defun restore-frame-position--number-or-zero (maybe-number)
    "Return 0 if MAYBE-NUMBER is a non-number."
    (if (number-or-marker-p maybe-number)
        maybe-number
      0))

  (defun restore-frame-position-save ()
    "Save the current frame's size and position to `restore-frame-position-file'."
    (when (window-system)
      (let* ((frame-geometry-left (restore-frame-position--number-or-zero (frame-parameter (selected-frame) 'left)))
             (frame-geometry-top (restore-frame-position--number-or-zero (frame-parameter (selected-frame) 'top)))
             (frame-geometry-width (restore-frame-position--number-or-zero (frame-text-width)))
             (frame-geometry-height (restore-frame-position--number-or-zero (frame-text-height))))
        (with-temp-buffer
          (insert
           (format "(add-to-list 'initial-frame-alist '(top . %d))\n" (max frame-geometry-top 0))
           (format "(add-to-list 'initial-frame-alist '(left . %d))\n" (max frame-geometry-left 0))
           (format "(add-to-list 'initial-frame-alist '(width . (text-pixels . %d)))\n" (max frame-geometry-width 0))
           (format "(add-to-list 'initial-frame-alist '(height . (text-pixels . %d)))\n" (max frame-geometry-height 0)))
          (when (file-writable-p restore-frame-position-file)
            (write-file restore-frame-position-file))))))

  (defun restore-frame-position-load ()
    "Load the current frame's size and position from `restore-frame-position-file'."
    (when (file-readable-p restore-frame-position-file)
      (load-file restore-frame-position-file)))

  (defun restore-frame-position ()
    "Install hooks to remember and restore initial frame poosition."
    (if after-init-time
        (restore-frame-position-load)
      (add-hook 'after-init-hook 'restore-frame-position-load))
    (add-hook 'kill-emacs-hook 'restore-frame-position-save))

  (restore-frame-position))

;; use M-u instaed of C-u for universal argument
(define-key global-map (kbd "C-u") 'kill-whole-line)
(define-key global-map (kbd "M-u") 'universal-argument)
(define-key universal-argument-map (kbd "C-u") nil)
(define-key universal-argument-map (kbd "M-u") 'universal-argument-more)

;; load evil early
(use-package evil
  :straight t
  :demand t
  ;; :load-path "~/dev/evil"
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-search-module 'evil-search)
  (setq evil-ex-search-vim-style-regexp t)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-shift-width 2)
  (setq evil-ex-substitute-global t)
  (setq evil-jumps-cross-buffers nil)
  (when (not (window-system))
    (setq evil-want-C-i-jump nil))
  :config
  (evil-mode)
  ;; face for %s///
  (custom-set-faces
   '(evil-ex-substitute-replacement ((t (:inherit 'query-replace)))))

  ;; auto-clear highlight
  (defun evgeni-nohighlight-hook (&rest _)
    (unless (memq this-command
                  '(evil-ex-search-next
                    evil-ex-search-previous
                    evil-change
                    self-insert-command
                    evil-normal-state
                    evil-repeat
                    evil-scroll-line-down
                    evil-scroll-line-up
                    evil-scroll-down
                    evil-scroll-up
                    evil-scroll-line-to-top
                    evil-scroll-line-to-bottom
                    evil-ex
                    recenter-top-bottom))
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
               swiper
               consult-line))
    (advice-add f :after #'evgeni-add-nohighlight-hook))

  (evil-define-command evgeni-save-file ()
    "Save file, unless there's a widget/button at point. Press it if there's one."
    :motion nil
    :move-point nil
    :repeat nil
    (interactive)
    (let* ((field  (get-char-property (point) 'field))
           (button (get-char-property (point) 'button))
           (doc    (get-char-property (point) 'widget-doc))
           (widget (or field button doc)))
      (cond
       ;; widget
       ((and widget
             (fboundp 'widget-type)
             (fboundp 'widget-button-press)
             (or (and (symbolp widget)
                      (get widget 'widget-type))
                 (and (consp widget)
                      (get (widget-type widget) 'widget-type))))
        (when (evil-operator-state-p)
          (setq evil-inhibit-operator t))
        (when (fboundp 'widget-button-press)
          (widget-button-press (point))))
       ;; button
       ((and (fboundp 'button-at)
             (fboundp 'push-button)
             (button-at (point)))
        (when (evil-operator-state-p)
          (setq evil-inhibit-operator t))
        (push-button))
       ;; save-file
       (t
        (if (or buffer-file-name (buffer-base-buffer))
            (call-interactively 'save-buffer)
          (user-error "No file"))))))

  (defun evgeni-window-vsplit ()
    (interactive)
    (split-window-horizontally)
    (windmove-right)
    (balance-windows))


  (defun evgeni-find-next-file (&optional backward)
    "Find the next file (by name) in the current directory.

With prefix arg, find the previous file."
    (interactive "P")
    (when buffer-file-name
      (let* ((file (expand-file-name buffer-file-name))
             (files (cl-remove-if (lambda (file) (cl-first (file-attributes file)))
                                  (sort (directory-files (file-name-directory file) t nil t) 'string<)))
             (pos (mod (+ (cl-position file files :test 'equal) (if backward -1 1))
                       (length files))))
        (find-file (nth pos files)))))

  (defun evgeni-find-prev-file ()
    "Find previous file in the current directory"
    (interactive)
    (evgeni-find-next-file t))

  (define-key evil-normal-state-map (kbd "RET") 'evgeni-save-file)
  (define-key evil-normal-state-map (kbd ", w") 'evgeni-window-vsplit)
  (define-key evil-normal-state-map (kbd "g C-g") 'count-words)

  (define-key evil-normal-state-map (kbd "] f") 'evgeni-find-next-file)
  (define-key evil-normal-state-map (kbd "[ f") 'evgeni-find-prev-file)
  (define-key evil-normal-state-map (kbd "] F") 'find-file)
  (define-key evil-normal-state-map (kbd "[ F") 'find-file)

  (define-key evil-normal-state-map (kbd "C-c C-b")'ido-switch-buffer)

  (define-key evil-normal-state-map (kbd "] SPC")(lambda (count) (interactive "p") (dotimes (_ count) (save-excursion (evil-insert-newline-below)))))
  (define-key evil-normal-state-map (kbd "[ SPC")(lambda (count) (interactive "p") (dotimes (_ count) (save-excursion (evil-insert-newline-above)))))

  (define-key evil-normal-state-map (kbd "[ m") 'beginning-of-defun)
  (define-key evil-normal-state-map (kbd "] m") 'end-of-defun)
  (evil-set-command-properties 'beginning-of-defun :jump t)
  (evil-set-command-properties 'end-of-defun :jump t)
  (define-key evil-normal-state-map (kbd "] M") 'end-of-defun)
  (define-key evil-motion-state-map (kbd "0") 'evil-first-non-blank)
  (define-key evil-normal-state-map (kbd "g n") (lambda () (interactive) (evil-ex "%normal ")))
  (define-key evil-visual-state-map (kbd "g n") (lambda () (interactive) (evil-ex "'<,'>normal ")))
  (define-key evil-ex-search-keymap "\C-w" #'evil-delete-backward-word)
  (define-key evil-motion-state-map (kbd "C-z") 'ignore)

  (evil-add-command-properties 'beginning-of-defun :jump t :repeat 'motion)
  (evil-add-command-properties 'end-of-defun :jump t :repeat 'motion)

  ;; RET should do nothing in operator pending state map
  (define-key evil-operator-state-map (kbd "RET") 'evil-force-normal-state)
  (define-key evil-operator-state-map [return] 'evil-force-normal-state)

  ;; `zT' to scroll beginning of defun to top
  (define-key evil-motion-state-map "zT" 'evgeni-scroll-beginning-of-defun-to-top)
  (defun evgeni-scroll-beginning-of-defun-to-top()
    (interactive)
    (save-excursion
      (beginning-of-defun)
      (evil-scroll-line-to-top nil)))

  ;; `gV' should select last pasted pasted
  (define-key evil-motion-state-map "gV" 'evgeni-evil-select-pasted)
  (defun evgeni-evil-select-pasted ()
    (interactive)
    (let ((start-marker (evil-get-marker ?\[))
          (end-marker (evil-get-marker ?\])))
      (evil-visual-select start-marker end-marker)))

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

  (define-key evil-motion-state-map "," nil) ;; , is used as a prefix
  (define-key evil-motion-state-map "*" 'evgeni-star)
  (define-key evil-motion-state-map "g*" 'evgeni-g-star)

  (defvar evgeni-conflict-marker-regex "^[<=>|]\\{7\\}")

  ;; ex aliases/shortcuts
  (evil-ex-define-cmd "Q" 'delete-frame)
  (evil-ex-define-cmd "Qa" "qa")
  (evil-ex-define-cmd "On" "on")
  (evil-ex-define-cmd "o" "on")

  ;; :source
  (evil-ex-define-cmd "so[urce]" 'evgeni-source)
  (evil-define-command evgeni-source (&optional file)
    (interactive "<f>")
    (if file
        (load-file file)
      (load-file (concat user-emacs-directory "init.el"))))

  (evil-ex-define-cmd "init.el" (lambda () (interactive) (find-file (concat user-emacs-directory "init.el"))))
  (evil-ex-define-cmd "find-library" 'find-library)

  ;; my intercept map
  (defvar evgeni-intercept-mode-map (make-sparse-keymap))
  (define-minor-mode evgeni-intercept-mode
    "Global minor mode for higher precedence evil keybindings."
    :global t)
  (evgeni-intercept-mode)
  (dolist (state '(normal insert))
    (evil-make-intercept-map
     (evil-get-auxiliary-keymap evgeni-intercept-mode-map state t t)
     state))

  ;; navigate b/w emacs windows and tmux panes
  (defun evgeni-window-navigate (emacs-cmd tmux-cmd)
    (condition-case nil
        (funcall emacs-cmd)
      (error (if (getenv "TMUX") (let ((default-directory "~"))
                                   (shell-command-to-string tmux-cmd))))))

  (evil-define-key '(normal insert) evgeni-intercept-mode-map
    (kbd "C-h") (lambda () (interactive) (evgeni-window-navigate 'windmove-left  "tmux select-pane -L"))
    (kbd "C-j") (lambda () (interactive) (evgeni-window-navigate 'windmove-down  "tmux select-pane -D"))
    (kbd "C-k") (lambda () (interactive) (evgeni-window-navigate 'windmove-up    "tmux select-pane -U"))
    (kbd "C-l") (lambda () (interactive) (evgeni-window-navigate 'windmove-right "tmux select-pane -R")))
  (evil-global-set-key 'normal (kbd "C-h") (lambda () (interactive) (evgeni-window-navigate 'windmove-left  "tmux select-pane -L")))
  (evil-global-set-key 'normal (kbd "C-j") (lambda () (interactive) (evgeni-window-navigate 'windmove-down  "tmux select-pane -L")))
  (evil-global-set-key 'normal (kbd "C-k") (lambda () (interactive) (evgeni-window-navigate 'windmove-up  "tmux select-pane -D")))
  (evil-global-set-key 'normal (kbd "C-l") (lambda () (interactive) (evgeni-window-navigate 'windmove-right    "tmux select-pane -U")))

  ;; insert state
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-insert-state-map (kbd "C-u") (lambda () (interactive) (evil-delete (point-at-bol) (point))))
  (define-key evil-insert-state-map (kbd "C-x s") 'complete-symbol)
  (define-key evil-insert-state-map (kbd "C-a") 'evgeni-beginning-of-line)
  ;; (define-key evil-insert-state-map (kbd "TAB") 'evgeni-tab-or-complete-previous)

  ;; (defun evgeni-tab-or-complete-previous ()
  ;;   (interactive)
  ;;   (if (looking-at "\\_>") (call-interactively 'evil-complete-previous) (indent-for-tab-command)))

  (defun evgeni-beginning-of-line ()
    (interactive)
    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (move-beginning-of-line 1))))

  ;; expand lines
  (define-key evil-insert-state-map (kbd "C-x C-l") 'evil-complete-next-line)

  ;; completion
  ;; (define-key evil-insert-state-map (kbd "C-x C-x") 'completion-at-point)
  ;; (define-key evil-insert-state-map (kbd "C-k") 'completion-at-point)
  (define-key evil-insert-state-map (kbd "C-x C-]") 'complete-tag)
  (define-key evil-insert-state-map (kbd "C-]") 'completion-at-point)

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
      (exchange-point-and-mark)
      (evil-range (region-beginning) (region-end) type :expanded t)))

  (define-key evil-inner-text-objects-map "e" 'evgeni-entire-text-object)
  (define-key evil-outer-text-objects-map "e" 'evgeni-entire-text-object)

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

  ;; C-r in minibuffer
  (define-key minibuffer-local-map (kbd "C-r") 'evil-paste-from-register)
  (define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)
  (define-key minibuffer-local-map (kbd "C-u") 'backward-kill-paragraph)

  ;; move by visual lines with j/k
  (define-key evil-normal-state-map "j"  'evil-next-visual-line)
  (define-key evil-normal-state-map "k"  'evil-previous-visual-line)
  ;; (define-key evil-normal-state-map "gj" 'evil-next-line)
  (define-key evil-normal-state-map "gk" 'evil-previous-line)
  (define-key evil-visual-state-map "j"  'evil-next-visual-line)
  (define-key evil-visual-state-map "k"  'evil-previous-visual-line)
  ;; (define-key evil-visual-state-map "gj" 'evil-next-line)
  (define-key evil-visual-state-map "gk" 'evil-previous-line)

  (defun evgeni-prev-or-move-end-of-line ()
    (interactive)
    (when (not (thing-at-point 'line t))
      (call-interactively 'previous-complete-history-element))
    (call-interactively 'move-end-of-line))

  (defun evgeni-prev-or-move-beginning-of-line ()
    (interactive)
    (when (not (thing-at-point 'line t))
      (call-interactively 'previous-complete-history-element))
    (call-interactively 'move-beginning-of-line))

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
  (define-key evil-ex-completion-map "\C-a" 'evgeni-prev-or-move-beginning-of-line)
  (define-key evil-ex-completion-map (kbd "DEL" ) 'evgeni-prev-or-prev-and-backspace)
  (define-key evil-ex-completion-map "\C-b" 'backward-char)
  (define-key evil-ex-completion-map "\C-f" 'forward-char)
  (define-key evil-ex-completion-map "\C-d" 'evgeni-ex-delete-or-complete)
  (define-key evil-ex-completion-map (kbd "C-u") 'backward-kill-paragraph)

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

  (evil-define-key 'visual 'global
    "." (kbd ":norm. RET"))

  ;; re-define evil-indent to save-excursion
  (evil-define-operator evil-indent (beg end)
    "Indent text."
    :move-point nil
    :type line
    (save-excursion
      (if (and (= beg (line-beginning-position))
               (= end (line-beginning-position 2)))
          ;; since some Emacs modes can only indent one line at a time,
          ;; implement "==" as a call to `indent-according-to-mode'
          (indent-according-to-mode)
        (goto-char beg)
        (indent-region beg end))
      ;; We also need to tabify or untabify the leading white characters
      (when evil-indent-convert-tabs
        (let* ((beg-line (line-number-at-pos beg))
               (end-line (line-number-at-pos end))
               (ln beg-line)
               (convert-white (if indent-tabs-mode 'tabify 'untabify)))
          (save-excursion
            (while (<= ln end-line)
              (goto-char (point-min))
              (forward-line (- ln 1))
              (back-to-indentation)
              ;; Whether tab or space should be used is determined by indent-tabs-mode
              (funcall convert-white (line-beginning-position) (point))
              (setq ln (1+ ln)))))
        (back-to-indentation))))

  (defun evgeni-start-ex-substitute ()
    "start %s/.../... with the last search pattern"
    (interactive)
    (let ((pattern (or (evil-ex-pattern-regex evil-ex-search-pattern) "")))
      (let ((replace-with (replace-regexp-in-string "\\\\[<>]" "" pattern))
            (ex-prefix (if (evil-visual-state-p) "'<,'>" "%")))
        (evil-ex (format "%ss//%s" ex-prefix replace-with)))))

  (defun evgeni-start-ex-substitute-in-defun ()
    "Mark defun and call `evgeni-start-ex-substitute'"
    (interactive)
    (mark-defun)
    (evgeni-start-ex-substitute))

  (define-key evil-normal-state-map (kbd ",m") 'evgeni-start-ex-substitute)
  (define-key evil-visual-state-map (kbd ",m") 'evgeni-start-ex-substitute)
  (define-key evil-normal-state-map (kbd ",M") 'evgeni-start-ex-substitute-in-defun)
  (define-key evil-visual-state-map (kbd ",M") 'evgeni-start-ex-substitute-in-defun)

  ;; system clipboard integration
  (when (display-graphic-p)
    (setq x-select-enable-clipboard nil)
    (define-key global-map [remap ns-copy-including-secondary] 'clipboard-kill-ring-save)
    (define-key global-map [remap yank] 'clipboard-yank)
    (with-eval-after-load 'org
      (define-key org-mode-map (kbd "s-v") 'clipboard-yank)))

  ;; don't move point on :s// and :g//
  (defun evgeni-save-excursion-advice (orig-fun &rest args)
    (save-excursion (apply orig-fun args)))
  ;; (advice-add 'evil-ex-substitute :around #'evgeni-save-excursion-advice)
  (advice-add 'evil-ex-global :around #'evgeni-save-excursion-advice)


  ;; `narrow' ex command
  (ex! "nar[row]" 'evgeni-narrow-or-widen)
  (evil-define-command evgeni-narrow-or-widen (bang begin end)
    "Narrow ex command. With BANG, clone the buffer, then narrow"
    (interactive "<!><r>")
    (cond
     (bang
      ;; TODO narrow to defun missing
      (if (region-active-p)
          (evgeni-narrow-to-region-indirect begin end)
        (switch-to-buffer (clone-indirect-buffer nil nil))
        (funcall-interactively 'evgeni-narrow-or-widen nil begin end)))
     ((region-active-p)
      (narrow-to-region begin end))
     ((buffer-narrowed-p)
      (if (buffer-base-buffer)
          (let ((cloned-buf (current-buffer)))
            (switch-to-buffer (buffer-base-buffer)) ;; return to previous buffer
            (evil-delete-buffer cloned-buf)) ;; wipe out indirect buffer
        (widen)))
     (t
      (if (eq major-mode 'org-mode)
          (org-narrow-to-subtree)
        (narrow-to-defun)))))

  (defun evgeni-narrow-to-region-indirect (start end)
    "Restrict editing in this buffer to the current region, indirectly."
    (interactive "r")
    (deactivate-mark)
    (let ((buf (clone-indirect-buffer nil nil)))
      (with-current-buffer buf
        (narrow-to-region start end))
      (switch-to-buffer buf)))

  (require 'evil-development)

  (use-package evil-collection
    :straight t
    :init
    (setq evil-collection-outline-bind-tab-p nil
          evil-collection-want-find-usages-bindings nil)
    :config
    (delete 'outline evil-collection-mode-list)
    (delete 'eglot evil-collection-mode-list)
    (delete 'markdown-mode evil-collection-mode-list)
    (evil-collection-init))

  (use-package evil-surround
    :straight t
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
    :straight t
    :bind (:map evil-normal-state-map
                ("g l " . evil-lion-left)
                ("g L " . evil-lion-right)
                :map evil-visual-state-map
                ("g l " . evil-lion-left)
                ("g L " . evil-lion-right)))

  (use-package evil-commentary
    :straight t
    :bind (:map evil-normal-state-map
                ("gc" . evil-commentary))
    :config
    (defun evil-commentary/ensure-in-comment-block (beg end forward)
      (save-excursion
        (beginning-of-line)
        (if (not (or (looking-at-p (concat "^\s*" (regexp-quote comment-start)))
                     (looking-at-p (concat "^\s*$"))))
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
                       (list saved-beg end forward))))))))

    (defun evgeni-commentary-uncomment-adjacent ()
      (interactive)
      (when (and (eq evil-this-operator 'evil-commentary))
        (setq evil-inhibit-operator t)
        (let ((beg (evil-commentary/ensure-in-comment-block (point) (point) nil))
              (end (evil-commentary/ensure-in-comment-block (point) (point) t)))
          (evil-commentary (car beg) (cadr end)))))

    (evil-define-key 'operator global-map
      "u" '(menu-item
            ""
            nil
            :filter (lambda (&optional _)
                      (when (eq evil-this-operator 'evil-commentary)
                        (evgeni-inhibit-operator #'evgeni-commentary-uncomment-adjacent))))))

  (use-package evil-exchange
    :straight t
    :commands (evil-exchange evil-exchange-cancel)
    :init
    (defmacro evgeni-inhibit-operator (command)
      "Return a command that inhibits evil operator code."
      `(lambda ()
         (interactive)
         (setq evil-inhibit-operator t)
         (call-interactively ,command)))

    (evil-define-key 'operator global-map
      "x" '(menu-item
            ""
            nil
            :filter (lambda (&optional _)
                      (when (eq evil-this-operator 'evil-change)
                        (evgeni-inhibit-operator #'evil-exchange)))))

    (evil-define-key 'operator global-map
      "X" '(menu-item
            ""
            nil
            :filter (lambda (&optional _)
                      (when (eq evil-this-operator 'evil-change)
                        (evgeni-inhibit-operator #'evil-exchange-cancel)))))

    (evil-define-key 'visual global-map "X" 'evil-exchange))

  (use-package evil-replace-with-register
    :straight t
    :bind (:map evil-normal-state-map
                ("gr" . evil-replace-with-register)
                :map evil-visual-state-map
                ("gr" . evil-replace-with-register))
    :config
    (setq evil-replace-with-register-indent t))

  (use-package evil-visualstar
    :straight t
    :bind (:map evil-visual-state-map
                ("*" . evil-visualstar/begin-search-forward)
                ("#" . evil-visualstar/begin-search-backward))
    :config
    (advice-add 'evil-visualstar/begin-search-forward :around #'evgeni-save-excursion-advice))

  (use-package evil-indent-plus
    :straight t
    :defer t
    :init
    (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
    (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
    (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
    :config
    ;; temprorary fix until this is addressed https://github.com/TheBB/evil-indent-plus/pull/4
    (defun evil-indent-plus--linify (range)
      (let ((nbeg (save-excursion (goto-char (cl-first range)) (point-at-bol)))
            (nend (save-excursion (goto-char (cl-second range)) (+ (point-at-eol) (if (evil-visual-state-p) 0 1)))))
        (evil-range nbeg nend 'line))))

  (message "Loading evil-mode...done (%.3fs)" (float-time (time-subtract (current-time) emacs-start-time))))

(use-package dired
  :demand
  :after evil
  :bind (
         :map dired-mode-map
         (" " . evgeni-find-file-recursively)
         :map evil-normal-state-map
         ("-" . evgeni-dired-current-dir))
  :config
  (setq dired-listing-switches "-alh"
        dired-auto-revert-buffer t
        dired-dwim-target t
        dired-kill-when-opening-new-dired-buffer t
        dired-use-ls-dired nil)
  (evil-define-key 'normal dired-mode-map
    "-" 'dired-up-directory
    "U" 'evgeni-dired-unmark-all-marks
    " " 'evgeni-find-file-recursively
    (kbd "g RET") 'dired-find-file-other-window)

  (add-hook 'dired-mode-hook 'dired-hide-details-mode)

  (defun evgeni-dired-unmark-all-marks()
    "call `magit-status' on double-U press `U U'"
    (interactive)
    (if (eq this-command last-command)
        (magit-status)
      (dired-unmark-all-marks)))

  (defun evgeni-dired-current-dir ()
    (interactive)
    (let ((file buffer-file-name))
      (dired ".")
      (when file
        (dired-goto-file file))))

  (defun evgeni-find-file-recursively()
    (interactive)
    (find-file
     (completing-read
      "File: "
      (directory-files-recursively
       "." "." nil
       (lambda (dir) (not (string-equal (file-name-nondirectory dir) ".git"))))))))

(use-package dired-subtree
  :straight t
  :commands dired-subtree-toggle
  :init
  (with-eval-after-load 'dired
    (define-key dired-mode-map [tab] 'dired-subtree-toggle)
    (define-key dired-mode-map (kbd "TAB") 'dired-subtree-toggle))

  (custom-set-faces
   '(dired-subtree-depth-1-face ((t (:background unspecified))))
   '(dired-subtree-depth-2-face ((t (:background unspecified))))
   '(dired-subtree-depth-3-face ((t (:background unspecified))))
   '(dired-subtree-depth-4-face ((t (:background unspecified))))
   '(dired-subtree-depth-5-face ((t (:background unspecified))))
   '(dired-subtree-depth-6-face ((t (:background unspecified))))))

(use-package autorevert
    :hook (after-init . global-auto-revert-mode)
    :config
    (setq auto-revert-verbose nil))

(use-package magit
  :straight t
  :after evil
  :bind (:map evil-normal-state-map
              ("U U" . magit-status)
              (", u" . magit-file-dispatch)
              (", U" . magit-dispatch)
              ("U u" . magit-file-dispatch)
              ("U w" . magit-stage-buffer-file)
              ("U d" . evgeni-magit-diff-unstaged-buffer-file)
              ("U L" . magit-log-head)
              ("U l" . magit-log-buffer-file)
              ("U r" . evgeni-magit-file-checkout)
              ("U c" . magit-commit)
              ("U b" . magit-branch)
              ("U z" . magit-stash)
              ("U m" . magit-merge)
              ("U p" . magit-push)
              ("U f" . magit-fetch)
              ("U F" . magit-pull)
              ("U B" . magit-checkout))
  :init
  (ex! "ma[git]" 'magit-dispatch)
  (ex! "gedit" 'magit-find-file)
  :config

  (defun evgeni-magit-diff-unstaged-buffer-file ()
    (interactive)
    (magit-diff-unstaged nil (list (magit-file-relative-name))))

  (use-package transient
    :defer
    :config
    (setq transient-save-history nil
          transient-show-popup 1))

  (defun evgeni-magit-file-checkout ()
    (interactive)
    (magit-file-checkout "HEAD" (magit-file-relative-name)))

  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (setq magit-diff-refine-hunk 't)
  (setq magit-save-repository-buffers 'dontask)

  ;; `q' should call `magit-mode-bury-buffer', for some reason it call `quit-window' with evil-mode/evil-collection
  (evil-define-key 'normal magit-status-mode-map
    "q" 'magit-mode-bury-buffer)

  ;; `gx' should work in magit revision mode map
  (evil-define-key 'normal magit-revision-mode-map (kbd "gx") 'browse-url-at-point)

  (evil-define-key 'normal magit-status-mode-map
    (kbd "g RET") 'magit-diff-visit-file-other-window)

  ;; no ediff mappings
  (define-key magit-status-mode-map "e" nil)
  (define-key magit-status-mode-map "E" nil)

  (define-key magit-diff-mode-map "e" nil) ;; no ediff

  (add-hook 'git-commit-mode-hook 'git-commit-turn-on-flyspell))

(use-package magit-libgit
  :ensure
  :disabled t
  :after magit)

(use-package smerge-mode
  :defer t
  :after evil
  :config
  (evil-define-minor-mode-key 'normal 'smerge-mode
    (kbd "] n")     'smerge-next
    (kbd "[ n")     'smerge-prev
    (kbd "C-c C-c") 'smerge-keep-current
    (kbd "C-c C-k") 'smerge-kill-current
    ;; "b" mnemonic for both, "a" for all
    (kbd "C-c C-b") 'smerge-keep-all
    (kbd "C-c C-a") 'smerge-keep-all))

(use-package vdiff
  :straight t
  :defer t
  :after evil
  :init
  (ex! "vdiff" 'evgeni-vdiff-visible-buffers)
  (defun evgeni-vdiff-visible-buffers ()
    "Execute vdiff on the 2 or 3 visible buffers. Error if there are less/more visible windows."
    (interactive)
    (let ((bufs (mapcar 'window-buffer (window-list)))
          (vdiff-2way-layout-function (lambda (&rest _args) nil)))
      (cond
       ((eq (length bufs) 2)
        (apply 'vdiff-buffers bufs))
       ((eq (length bufs) 3)
        (apply 'vdiff-buffers3 bufs))
       (t
        (user-error "Expected 2 or 3 buffers, got %s" (length bufs))))))

  ;; line diffing below
  (ex! "linediff" 'evgeni-line-diff)
  (ex! "linediff-reset" 'evgeni-line-diff-reset)

  (defvar evgeni-line-diff-buf-1 nil)
  (defvar evgeni-line-diff-buf-2 nil)

  (defun evgeni-line-diff-reset ()
    (interactive)

    (when evgeni-line-diff-buf-1
      (kill-buffer evgeni-line-diff-buf-1)
      (setq evgeni-line-diff-buf-1 nil))

    (when evgeni-line-diff-buf-2
      (kill-buffer evgeni-line-diff-buf-2)
      (setq evgeni-line-diff-buf-2 nil)))

  (evil-define-command evgeni-line-diff (beg end)
    (interactive "<r>")
    :keep-visual nil
    (when (and evgeni-line-diff-buf-1 evgeni-line-diff-buf-2)
      (evgeni-line-diff-reset))

    (let ((text (buffer-substring-no-properties beg end)))

      (cond
       ((null evgeni-line-diff-buf-1)
        (setq evgeni-line-diff-buf-1 (get-buffer-create "*line-diff-1*"))
        (with-current-buffer evgeni-line-diff-buf-1
          (insert text)))
       ((null evgeni-line-diff-buf-2)
        (setq evgeni-line-diff-buf-2 (get-buffer-create "*line-diff-2*"))
        (with-current-buffer evgeni-line-diff-buf-2
          (insert text)))))

    (when (and evgeni-line-diff-buf-1 evgeni-line-diff-buf-2)
      (vdiff-buffers evgeni-line-diff-buf-1 evgeni-line-diff-buf-2)))

  :config

  (setq vdiff-diff-algorithm 'git-diff-patience)

  ;; (add-hook 'vdiff-mode-hook (lambda () (diff-hl-mode -1)))
  ;; (add-hook 'vdiff-mode-off-hook (lambda () (diff-hl-mode +1)))

  (setq vdiff-auto-refine t)
  (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)
  (define-key vdiff-3way-mode-map (kbd "C-c") vdiff-mode-prefix-map)

  (evil-define-minor-mode-key 'normal 'vdiff-mode "q" 'vdiff-quit)
  (evil-define-minor-mode-key 'normal 'vdiff-3way-mode "q" 'vdiff-quit))

(use-package vdiff-magit
  :straight t
  :after magit
  :init
  :commands vdiff-magit-show-unstaged
  :bind (:map evil-normal-state-map
              ("U D" . vdiff-magit))
  :config
  (setq vdiff-magit-stage-is-2way t) ;; Only use two buffers (working file and index) for vdiff-magit-stage
  (define-key magit-mode-map "e" 'vdiff-magit-dwim)
  (define-key magit-mode-map "E" 'vdiff-magit))

(use-package org
  :commands evgeni-journal
  :after evil
  :init
  (ex! "journal" 'evgeni-journal)
  :config
  ;; text-mode-like parapgraph separation
  (add-hook 'org-mode-hook (lambda ()
                             (setq paragraph-start "\\|[ 	]*$"
                                   paragraph-separate "[ 	]*$")))

  (setq org-babel-default-header-args:bash '((:results . "output")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t) (dot . t) (python . t)))

  (evil-ex-define-cmd "cap[ture]" 'org-capture)

  (setq org-startup-indented t
        org-imenu-depth 3
        org-startup-folded "showall"
        org-confirm-babel-evaluate nil
        org-src-preserve-indentation t
        org-src-window-setup 'current-window)

  (evil-ex-define-cmd "tangle" 'evgeni-org-tangle)
  (evil-define-command evgeni-org-tangle (bang)
    (interactive "<!>")
    (let ((current-prefix-arg (if bang nil '(4)) ))
      (call-interactively 'org-babel-tangle)))

  (evil-define-key 'normal org-mode-map
    "o" (kbd "A RET")
    (kbd "C-]") 'org-open-at-point
    (kbd "] m") 'org-next-visible-heading
    (kbd "[ m") 'org-previous-visible-heading)

  ;; daily journal file, `:journal'
  (defun evgeni-journal()
    (interactive)
    (let ((path (evgeni-journal-file)))
      (if (file-exists-p path)
          (find-file path)
        (evgeni-journal-hydra/body))))

  (defhydra evgeni-journal-hydra (:color blue)
    "No journal file exists"
    ("n" evgeni-new-journal "New journal" :exit t)
    ("RET" evgeni-new-journal nil :exit t)
    ("l" evgeni-last-journal "Last journal" :exit t))

  (defun evgeni-journal-file()
    (let ((daily-name (format-time-string "%Y_%m_%d.org")))
      (expand-file-name (concat org-directory "/journal/"  daily-name))))

  (defun evgeni-new-journal()
    (interactive)
    (find-file (evgeni-journal-file))
    (save-restriction
      (widen)
      (unless (save-excursion
                (goto-char (point-min))
                (search-forward "#+TITLE" nil t))
        (save-excursion
          (goto-char (point-min))
          (insert "#+TITLE: Journal for " (format-time-string "%A, %b %d %Y") "\n")
          (save-buffer)))))

  (defun evgeni-last-journal()
    (interactive)
    (find-file (car (last (directory-files (concat org-directory "/journal/") 'full)))))

  ;; `:babellib' ex command to ingest my library of babel
  (ex! "babellib" 'evgeni-babellib)
  (defun evgeni-babellib ()
    (interactive)
    (org-babel-lob-ingest "~/org/snippets.org"))

  (use-package org-autolist
    :straight t
    :hook (org-mode . org-autolist-mode)
    :config
    (setq org-autolist-enable-delete nil)))

(use-package ob-async
  :straight t
  :disabled t
  :after org)

(use-package org-make-toc
  :straight t
  :defer t)

(use-package org-bullets
  :straight t
  :after org
  :config

  (setq org-bullets-bullet-list
        '("◉" "○" "•" "▸"))

  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package xref
  :after evil
  :bind (:map evil-normal-state-map
              ("C-]" . xref-find-definitions)
              ("C-t" . xref-go-back)
              ("g C-]" . xref-find-references)
              ("C-w C-]" . xref-find-definitions-other-window)
              ("C-w ]" . xref-find-definitions-other-window))
  :config
  (evil-define-key 'normal xref--xref-buffer-mode-map "q" 'delete-window)
  ;; don't use swiper in this buffer - swiper clears the highlighting in the xref buffer
  (evil-define-key 'normal xref--xref-buffer-mode-map "/" 'evil-ex-search-forward)

  ;; don't pulse, beacon already does this
  (setq xref-after-jump-hook (delete 'xref-pulse-momentarily xref-after-jump-hook))
  (setq xref-after-return-hook (delete 'xref-pulse-momentarily xref-after-return-hook)))

(use-package vertico
  :straight (:files (:defaults "extensions/*.el"))
  :init
  (vertico-mode)

  (use-package vertico-repeat
    :init
    (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
    :bind (:map evil-normal-state-map
                ("C-c r" . vertico-repeat)
                ("C-c R" . vertico-repeat-select)))

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)

  ;; Optionally use the `orderless' completion style.
  (use-package orderless
    :straight t
    :init
    ;; Configure a custom style dispatcher (see the Consult wiki)
    ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
    ;;       orderless-component-separator #'orderless-escapable-split-on-space)
    (setq completion-styles '(orderless basic)
          completion-category-defaults nil
          completion-category-overrides '((file (styles partial-completion)))))

  ;; Enable rich annotations using the Marginalia package
  (use-package marginalia
    :straight t
    :init
    (marginalia-mode))

  (use-package consult
    :straight t
    :bind (
           :map evil-motion-state-map
           ("K" . evgeni-consult-ripgrep-current-word)
           (", f" . consult-imenu)
           (", *" . evgeni-consult-line)
           :map evil-normal-state-map
           ;; ("SPC" . consult-project-buffer)
           (", f" . consult-imenu)
           ("/" . consult-line)
           ("g SPC" . consult-buffer)
           ("g /" . evgeni-consult-ripgrep)
           ("g P" . consult-yank-pop)
           ;; ("M-g e" . consult-compile-error)
           ;; ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
           ;; ("M-g g" . consult-goto-line)             ;; orig. goto-line
           ;; ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
           ;; ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
           ;; ("M-g m" . consult-mark)
           ;; ("M-g k" . consult-global-mark)
           ;; ("M-g i" . consult-imenu)
           ;; ("M-g I" . consult-imenu-multi)
           ;; ;; M-s bindings (search-map)
           ;; ("M-s d" . consult-find)
           ;; ("M-s D" . consult-locate)
           ;; ("M-s g" . consult-grep)
           ;; ("M-s G" . consult-git-grep)
           ;; ("M-s r" . consult-ripgrep)
           ;; ("M-s l" . consult-line)
           ;; ("M-s L" . consult-line-multi)
           ;; ("M-s k" . consult-keep-lines)
           ;; ("M-s u" . consult-focus-lines)
           ;; ;; Isearch integration
           ;; ("M-s e" . consult-isearch-history)
           ;; :map isearch-mode-map
           ;; ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
           ;; ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
           ;; ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
           ;; ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
           ;; Minibuffer history
           ;; :map minibuffer-local-map
           ;; ("M-s" . consult-history)
           ;; orig. next-matching-history-element
           ;; ("M-r" . consult-history)                 ;; orig. previous-matching-history-element
           )

    ;; Enable automatic preview at point in the *Completions* buffer. This is
    ;; relevant when you use the default completion UI.
    :hook (completion-list-mode . consult-preview-at-point-mode)

    ;; The :init configuration is always executed (Not lazy)
    :init

    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0.5
          consult-line-start-from-top t
          register-preview-function #'consult-register-format)

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

    :config
    (defun evgeni-consult-ripgrep (&optional initial-input)
      (interactive)
      (if (eq major-mode 'dired-mode)
          (consult-ripgrep default-directory initial-input)
        (consult-ripgrep (project-root (project-current)) initial-input)))

    (defun evgeni-consult-ripgrep-current-word ()
      (interactive)
      (evgeni-consult-ripgrep (evgeni-word-at-point-or-region)))

    (defun evgeni-consult-line ()
      (interactive)
      (consult-line (evgeni-word-at-point-or-region)))

    ;; evil n/N integration
    (defun evgeni-consult-line-evil-history (&rest _)
      "Add latest `consult-line' search pattern to the evil search history ring.
This only works with orderless and for the first component of the search."
      (when (and (bound-and-true-p evil-mode)
                 (eq evil-search-module 'evil-search))
        (let ((pattern (nth 1 (orderless-compile (car consult--line-history)))))
          (add-to-history 'evil-ex-search-history pattern)
          (setq evil-ex-search-pattern (list pattern t t))
          (setq evil-ex-search-direction 'forward)
          (when evil-ex-search-persistent-highlight
            (evil-ex-search-activate-highlight evil-ex-search-pattern)))))
    (advice-add #'consult-line :after #'evgeni-consult-line-evil-history)

    ;; (delete 'consult--source-bookmark consult-buffer-sources)
    (consult-customize
     consult--source-bookmark
     :hidden t)

    ;; configure preview
    (consult-customize
     consult-theme :preview-key '(:debounce 0.2 any)
     consult-buffer :require-match t
     consult-ripgrep consult-git-grep consult-grep
     consult-project-buffer consult-recent-file consult-xref
     evgeni-consult-ripgrep consult-buffer
     :preview-key nil)

    ;; Optionally configure the narrowing key.
    ;; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<") ;; "C-+"

    ;; https://github.com/minad/consult/wiki#orderless-style-dispatchers-ensure-that-the--regexp-works-with-consult-buffer
    (defun fix-dollar (args)
      (if (string-suffix-p "$" (car args))
          (list (format "%s[%c-%c]*$"
                        (substring (car args) 0 -1)
                        consult--tofu-char
                        (+ consult--tofu-char consult--tofu-range -1)))
        args))
    (advice-add #'orderless-regexp :filter-args #'fix-dollar)
    (advice-add #'prescient-regexp-regexp :filter-args #'fix-dollar)

    ;; Optionally make narrowing help available in the minibuffer.
    ;; You may want to use `embark-prefix-help-command' or which-key instead.
    ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

    ;; By default `consult-project-function' uses `project-root' from project.el.
    ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
    ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
    ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
    ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
    ;; (autoload 'projectile-project-root "projectile")
    ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
    ;; (setq consult-project-function nil)

    (evil-define-key '(normal) org-mode-map
      (kbd ", f") 'consult-org-heading))

  (use-package embark
    :straight t

    :bind (
           ("C-c e" . embark-act)
           :map minibuffer-local-map
           ("C-c o" . embark-export)
           ("C-c e" . embark-act))
    ;;  ("C-;" . embark-dwim)        ;; good alternative: M-.
    ;;  ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

    :init

    ;; Optionally replace the key help with a completing-read interface
    (setq prefix-help-command #'embark-prefix-help-command)

    :config

    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none)))))

  ;; Consult users will also want the embark-consult package.
  (use-package embark-consult
    :straight t ; only need to install it, embark loads it after consult if found
    :hook
    (embark-collect-mode . consult-preview-at-point-mode))

  ;; A few more useful configurations...
  (use-package emacs
    :init
    ;; Add prompt indicator to `completing-read-multiple'.
    ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
    (defun crm-indicator (args)
      (cons (format "[CRM%s] %s"
                    (replace-regexp-in-string
                     "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                     crm-separator)
                    (car args))
            (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    ;; Do not allow the cursor in the minibuffer prompt
    (setq minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

    ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
    ;; Vertico commands are hidden in normal buffers.
    (setq read-extended-command-predicate
          #'command-completion-default-include-p))

  (use-package corfu
    :straight t
    :custom
    ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
    (corfu-auto t)                 ;; Enable auto completion
    (corfu-auto-delay 0.3)                 ;; Enable auto completion
    (corfu-separator ?\s)          ;; Orderless field separator
    ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
    ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
    ;; (corfu-preview-current nil)    ;; Disable current candidate preview
    ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
    ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
    ;; (corfu-scroll-margin 5)        ;; Use scroll margin

    ;; Recommended: Enable Corfu globally.
    ;; This is recommended since Dabbrev can be used globally (M-/).
    ;; See also `corfu-excluded-modes'.
    :init
    (global-corfu-mode)

    ;; Transfer completion to the minibuffer
    ;; https://github.com/minad/corfu?tab=readme-ov-file#transfer-completion-to-the-minibuffer
    (defun corfu-move-to-minibuffer ()
      (interactive)
      (pcase completion-in-region--data
        (`(,beg ,end ,table ,pred ,extras)
         (let ((completion-extra-properties extras)
               completion-cycle-threshold completion-cycling)
           (consult-completion-in-region beg end table pred)))))
    (keymap-set corfu-map "C-c m" #'corfu-move-to-minibuffer)
    (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))

  (use-package corfu-terminal
    :straight t
    :unless (display-graphic-p)
    :config
    (corfu-terminal-mode +1))

  ;; A few more useful configurations...
  (use-package emacs
    :init
    ;; TAB cycle if there are only few candidates
    (setq completion-cycle-threshold 3)

    ;; Enable indentation+completion using the TAB key.
    ;; `completion-at-point' is often bound to M-TAB.
    (setq tab-always-indent 'complete)))

(use-package winner
  :defer .5
  :bind (:map evil-normal-state-map ("z u" . hydra-winner/winner-undo))
  :after evil
  :config
  (winner-mode)
  (defhydra hydra-winner ()
    "Winner"
    ("u" winner-undo "undo")
    ("C-r" winner-redo "redo" :exit t)
    ("r" winner-redo "redo" :exit t)))

(use-package project
  :straight (:type built-in)
  :demand
  :bind (:map evil-normal-state-map
              (", SPC" . project-switch-project)
              ("SPC" . project-find-file))
  :config
  (setq project-vc-extra-root-markers '(".projectile" ".project"))
  (setq project-switch-commands 'project-dired))

(use-package yasnippet
  :straight t
  :config
  (setq yas-verbosity 2)
  (yas-global-mode)
  (ex! "yas-new" 'yas-new-snippet))

(use-package evil-expat
  ;; :load-path "~/dev/evil-expat"
  :straight t
  :defer .5)

(use-package evil-goggles
  :defer .5
  :straight t
  ;; :load-path "~/dev/evil-goggles"
  :config
  (setq evil-goggles-pulse nil)
  (evil-goggles-use-diff-faces)

  (evil-goggles-mode))

(use-package recentf
  :init
  (setq recentf-max-saved-items 250)
  (setq recentf-exclude '("/tmp/" ;; "/ssh:"
                          "\.pyc$"
                          (expand-file-name "~/Maildir/")))
  (setq recentf-auto-cleanup 3600) ;; cleanup after an hour of idleness
  :config
  (recentf-mode))

(use-package saveplace
  :config
  (save-place-mode))

(use-package savehist
  :init
  (setq
   enable-recursive-minibuffers t
   history-length 1000
   savehist-additional-variables '(mark-ring
                                   global-mark-ring
                                   search-ring
                                   regexp-search-ring
                                   extended-command-history)
   savehist-autosave-interval 60)
  (savehist-mode t))

(use-package wdired
  :commands wdired-change-to-wdired-mode
  :init
  (evil-set-initial-state 'wdired-mode 'normal)
  (ex! "wdired" 'wdired-change-to-wdired-mode))

(use-package flymake
  :defer t
  :config
  (setq flymake-start-syntax-check-on-newline nil)

  (defun evgeni-flymake-show-buffer-diagnostics (&rest _)
    (call-interactively 'flymake-show-buffer-diagnostics))

  (advice-add 'evil-collection-unimpaired-next-error :after 'evgeni-flymake-show-buffer-diagnostics)
  (advice-add 'evil-collection-unimpaired-first-error :after 'evgeni-flymake-show-buffer-diagnostics)
  (advice-add 'evil-collection-unimpaired-previous-error :after 'evgeni-flymake-show-buffer-diagnostics))

(use-package abbrev
  :disabled
  :defer t
  :init
  (setq-default abbrev-mode t)
  (setq save-abbrevs nil))

(use-package wgrep
  :straight t
  :bind (:map grep-mode-map
              ("C-x C-q" . wgrep-change-to-wgrep-mode))
  :commands wgrep-change-to-wgrep-mode
  :init
  (setq wgrep-auto-save-buffer t)
  (ex! "wgrep" 'wgrep-change-to-wgrep-mode))

(use-package replace
  :bind (:map occur-mode-map
              ("C-x C-q" . occur-edit-mode)))

(use-package compile
  :bind (:map evil-normal-state-map
              ("g m" . compile)
              ("g RET" . compile))
  :config
  (setq compilation-scroll-output 'next-error)
  (setq compilation-read-command nil)
  (setq compilation-ask-about-save nil)

  (defun evgeni-close-compile-win-if-successful (buffer string)
    (if (and
         (eq major-mode 'compilation-mode)
         (buffer-live-p buffer)
         ;; (<= (line-number-at-pos (point-max)) 8)
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string))
        (progn
          (sit-for .8)
          (delete-windows-on buffer))
      (with-selected-window (get-buffer-window buffer)
        (goto-char (point-min))
        (scroll-up-line 4))))

  (add-hook 'compilation-finish-functions 'evgeni-close-compile-win-if-successful))

(use-package imenu
  :defer t
  :config
  (setq imenu-auto-rescan-maxout 600000))

(use-package elisp-mode
  :defer t
  :init
  (defalias 'elisp-mode 'emacs-lisp-mode)
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

(use-package whitespace
  :after evil
  :config
  (ex! "clean-whitespace" 'whitespace-cleanup)
  :init
  (add-hook 'prog-mode-hook 'evgeni-show-trailing-whitespace)
  :config
  (setq whitespace-style '(face tabs trailing))
  (set-face-attribute 'trailing-whitespace nil
                      :background
                      (face-attribute 'mode-line-inactive :background))
  (defun evgeni-show-trailing-whitespace ()
    (set (make-local-variable 'show-trailing-whitespace) t))
  (add-hook 'evil-insert-state-entry-hook (lambda () (setq-local show-trailing-whitespace nil)))
  (add-hook 'evil-insert-state-exit-hook  (lambda () (setq-local show-trailing-whitespace t))))

(use-package undo-tree
  :straight t
  :defer t
  :config
  ;; (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (ex! "undo-tree" 'undo-tree-visualize)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-enable-undo-in-region nil)

  (define-key evil-visual-state-map "u" 'undo)
  (define-key evil-visual-state-map "\C-r" 'redo)

  (define-key evil-visual-state-map "gu" 'evil-downcase)
  (define-key evil-visual-state-map "gU" 'evil-upcase))

(use-package eros
  :straight t
  :commands (eros-eval-last-sexp eros-eval-defun)
  :init
  (define-key global-map [remap eval-last-sexp] 'eros-eval-last-sexp)
  (define-key global-map [remap eval-defun] 'eros-eval-defun))

(use-package markdown-mode
  :straight t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (evil-define-key 'normal markdown-mode-map
    (kbd "<tab>") 'markdown-cycle
    (kbd "<S-tab>") 'markdown-shifttab)

  ;; text-mode-like parapgraph separation
  (add-hook 'markdown-mode-hook (lambda ()
                             (setq paragraph-start "\\|[ 	]*$"
                                   paragraph-separate "[ 	]*$"))))

(use-package origami
  :straight t
  :commands origami-mode)

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
  :straight t
  :after haskell-mode
  :if (executable-find "hindent")
  :init
  (add-hook 'haskell-mode-hook 'hindent-mode))

(use-package smartparens
  :straight t
  :commands sp-forward-slurp-sexp sp-forward-barf-sexp
  :init
  (define-key emacs-lisp-mode-map (kbd "C-c <") 'sp-forward-slurp-sexp)
  (define-key emacs-lisp-mode-map (kbd "C-c >") 'sp-forward-barf-sexp))

(use-package paren
  :defer .5
  :config
  (setq show-paren-style 'parenthesis
        show-paren-delay 0)
  (show-paren-mode 1))

(use-package ledger-mode
  :straight t
  :commands ledger-mode)

(use-package uniquify
  :defer .5
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)     ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*")) ; don't muck with special buffers

(use-package macrostep
  :straight t
  :defer t
  :init
  (ex! "macrostep-expand" 'macrostep-expand)
  :config
  (evil-define-minor-mode-key 'normal 'macrostep-mode
    "q" 'macrostep-collapse-all
    "o" 'macrostep-expand
    "c" 'macrostep-collapse))

(use-package yaml-mode
  :straight t
  :mode (("\\.yml$" . yaml-mode) ("\\.yab$" . yaml-mode) ("\\.yaml$" . yaml-mode))
  :config
  (setq yaml-imenu-generic-expression
        '((nil  "^[ ]\\{0,2\\}\\(:?[a-zA-Z_-]+\\):"          1)))
  (modify-syntax-entry ?_ "w" yaml-mode-syntax-table)
  (modify-syntax-entry ?- "w" yaml-mode-syntax-table)
  (modify-syntax-entry ?$ "." yaml-mode-syntax-table))

(use-package package-lint
  :straight t
  :commands package-lint-current-buffer)

(use-package js
  :defer t
  :config
  (modify-syntax-entry ?_ "w" js-mode-syntax-table))

(use-package json
  :commands json-pretty-print
  :init
  (ex! "json" 'evgeni-json-pretty-print-dwim)
  (defun evgeni-json-pretty-print-dwim ()
    (interactive)
    (if (region-active-p)
        (call-interactively 'json-pretty-print)
      (json-pretty-print-buffer)
      (json-ts-mode)))
  :config
  (setq json-encoding-object-sort-predicate 'string<))

(use-package rjsx-mode
  :straight t
  :mode ("\\.jsx\\'" . rjsx-mode))

(use-package css-mode
  :defer t
  :config
  (modify-syntax-entry ?- "w" css-mode-syntax-table))

(use-package tramp
  :defer t
  :config
  (setq tramp-verbose 1)
  (setq remote-file-name-inhibit-cache nil)
  (setq tramp-default-method "ssh"))

(use-package dockerfile-mode
  :straight t
  :mode (".*Dockerfile.*" . dockerfile-mode)
  :config
  (modify-syntax-entry ?$ "." dockerfile-mode-syntax-table))

(use-package restclient
  :straight t
  :mode ("\\.restclient\\'" . restclient-mode))

(use-package conf-mode
  :defer t
  :config
  (modify-syntax-entry ?_ "w" conf-mode-syntax-table))

(use-package edebug
  :defer t
  :config
  (add-hook 'edebug-mode-hook 'evil-normalize-keymaps))

(use-package exec-path-from-shell
  :if (display-graphic-p)
  :straight t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (add-to-list 'exec-path-from-shell-variables "GOPATH" t)
  (add-to-list 'exec-path-from-shell-variables "LC_ALL" t)
  (add-to-list 'exec-path-from-shell-variables "GOBIN" t)
  (add-to-list 'exec-path-from-shell-variables "MONOREPO_GOPATH_MODE" t)
  (add-to-list 'exec-path-from-shell-variables "UBER_USER_UUID" t)
  (add-to-list 'exec-path-from-shell-variables "SSH_AUTH_SOCK" t)
  (exec-path-from-shell-initialize))

(use-package edit-indirect
  :straight t
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

(use-package hl-todo
  :straight t
  :defer 1.5
  :config
  (global-hl-todo-mode))

(use-package diff
  :defer t
  :config
  (add-hook 'diff-mode-hook 'diff-auto-refine-mode))

(use-package diff-hl
  :after evil
  :straight t
  :defer 1.5
  :custom
  (diff-hl-margin-symbols-alist '((insert . "+") (delete . "-") (change . "|") (unknown . "?") (ignored . "i")))
  :config
  (global-diff-hl-mode)
  (unless (window-system)
    (diff-hl-margin-mode))

  ;; (add-hook 'diff-hl-mode-hook 'diff-hl-flydiff-mode) ;; disabled because it adds lag when opening files
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)

  (evil-add-command-properties 'diff-hl-next-hunk :jump t :repeat 'motion)
  (evil-add-command-properties 'diff-hl-previous-hunk :jump t :repeat 'motion)

  (evil-define-minor-mode-key 'normal 'diff-hl-mode
    "]c" 'diff-hl-next-hunk
    "[c" 'diff-hl-previous-hunk
    "Ux" 'diff-hl-revert-hunk)

  (setq diff-hl-highlight-revert-hunk-function 'evgeni-diff-hl-highlight-revert-hunk-function)

  ;; display only the hunk to be reverted
  (defun evgeni-diff-hl-highlight-revert-hunk-function (_end)
    (diff-restrict-view)))

(use-package ibuffer
  :defer t
  :init
  (ex! "ls" 'ibuffer)
  :config
  (setq ibuffer-expert t) ;; don't prompt for confirmation
  (define-key ibuffer-mode-map "j" 'ibuffer-forward-line)
  (define-key ibuffer-mode-map "k" 'ibuffer-backward-line))

(use-package elec-pair
  :defer t
  :init
  (add-hook 'prog-mode-hook 'electric-pair-local-mode))

(use-package git-link
  :straight (git-link :branch "v0.8.0")
  :defer t
  :load-path "~/dev/git-link"
  :init
  (ex! "gitlink" 'git-link)
  (ex! "gitlnk" 'git-link)
  (ex! "glink" 'git-link)
  (ex! "gitlink-commit" 'git-link-commit)
  :config
  (setq git-link-open-in-browser (display-graphic-p))

  (add-to-list 'git-link-remote-alist '("uber.internal" git-link-code-uber-com))
  (defun git-link-code-uber-com (hostname dirname filename branch commit start end)
    ;; (message "hostname %s\ndirname %s\nfilename %s\nbranch %s\ncommit %s\nstart %s\nend %s" hostname dirname filename branch commit start end)
    ;; (format "https://sg.uberinternal.com/%s/uber-code/%s@%s/-/blob/%s"
    (format "https://sourcegraph.uberinternal.com/%s/%s@%s/-/blob/%s"
            (replace-regexp-in-string "config.uber.internal" "code.uber.internal" hostname)
            (replace-regexp-in-string "/$" "" (replace-regexp-in-string "@" "-" dirname)) ;; replace "@" with "-", remove trailing slash
            (or branch commit)
            (concat filename
                    (when start
                      (concat "#"
                              (if end
                                  (format "L%s-%s" start end)
                                (format "L%s" start))))))))

(use-package shell-pop
  :after evil
  :straight t
  :commands shell-pop
  :unless (getenv "TMUX") ;; don't run in tmux
  :defer 1.5
  :init
  (evil-define-key '(normal insert) evgeni-intercept-mode-map
    (kbd "C-c C-z") 'shell-pop)
  (setq shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))
  :config
  (setq shell-pop-full-span t))

(use-package git-commit
  :straight t
  :disabled t
  :defer t
  :config

  (use-package git-commit-insert-issue
    :straight t
    :config

    (use-package gitlab
      :straight t
      :config
      ;; load gitlab.el if any
      (when (file-readable-p (concat user-emacs-directory "gitlab.el"))
        (load-file (concat user-emacs-directory "gitlab.el"))))

    (defun git-commit-insert-issue-gitlab-issues (&optional projectname username)
      "Return a list of the opened issues on gitlab."
      (or (gitlab--get-host)
          (error "We can't find your gitlab host. Did you set gitlab-[host, username, password] ?"))
      (when (s-blank? gitlab-token-id)
        (gitlab-login))

      ;; edkolev has assignee_id 15
      (gitlab-list-project-issues (git-commit-insert-issue-project-id) 1 100 '((state . "opened") (assignee_id . 15))))

    (evil-define-key 'insert git-commit-mode-map
      (kbd "C-x C-x") 'evgeni-insert-ticket-number-from-git-branch
      (kbd "C-x x") 'evgeni-insert-ticket-number-from-git-branch)

    (defun evgeni-insert-ticket-number-from-git-branch ()
      (interactive)
      (let ((number-from-branch (or
                                 (let ((str (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))
                                   (when (string-match "\\([0-9]+\\)" str)
                                     (match-string-no-properties 0 str)))
                                 (let ((str (completing-read "issue: " (git-commit-insert-issue-get-issues-github-or-gitlab-or-bitbucket-format))))
                                   (when (string-match "\\([0-9]+\\)" str)
                                     (match-string-no-properties 0 str))))))
        (when number-from-branch
          (insert number-from-branch))))))

(use-package hydra
  :after evil
  :straight t
  :defer t
  :functions defhydra
  :init
  (ex! "font-size" 'evgeni-hydra-zoom/body)

  ;; "c o" / "y o" for toggles
  (evil-define-key 'operator global-map
    "o" '(menu-item
          ""
          nil
          :filter (lambda (&optional _)
                    (when (member evil-this-operator '(evil-change evil-yank))
                      #'evgeni-toggles))))

  :config
  (defhydra evgeni-hydra-zoom nil
    "zoom"
    ("j" text-scale-decrease "out")
    ("k" text-scale-adjust "in")
    ("0" (text-scale-set 0) "reset"))

  (defun evgeni-toggles ()
    (interactive)
    (setq evil-inhibit-operator t)
    (evgeni-toggles-hydra/body))

  (defhydra evgeni-toggles-hydra (:exit t)
    "toggles"
    ("c" global-hl-line-mode "global-hl-line-mode")
    ("y" global-hl-line-mode "global-hl-line-mode")
    ("w" visual-line-mode "visual-line-mode") ;; visually wrap lines
    ("h" auto-fill-mode "auto-fill-mode") ;; hard wrap lines
    ("k" toggle-input-method "toggle-input-method")
    ("n" display-line-numbers-mode "display-line-numbers-mode")
    ("s" (progn
           (call-interactively 'flyspell-mode)
           (when flyspell-mode
             (flyspell-buffer))) "flyspell-mode")
    ("r" (progn
           (display-line-numbers-mode +1)
           (setq display-line-numbers-type (if (eq display-line-numbers-type 'relative) t 'relative))
           (display-line-numbers-mode +1)) "relative line numbers")))

(use-package goto-chg
  :after evil
  :bind (:map evil-normal-state-map ("g ;" . hydra-goto-last-change/goto-last-change))
  :config
  (defhydra hydra-goto-last-change ()
    "Change List"
    (";" goto-last-change "Back")
    ("." goto-last-change-reverse "Forward")))

(use-package which-key
  :commands which-key-mode
  :straight t
  :config
  (setq which-key-idle-delay 0.4))

(use-package ox-hugo
  :straight t
  :after ox)

(use-package comint
  :after evil
  :defer t
  :config
  (evil-define-key 'insert comint-mode-map
    (kbd "C-p") #'comint-previous-matching-input-from-input
    (kbd "C-n") #'comint-next-matching-input-from-input))

(use-package eshell
  :after shell-pop
  :init
  (add-hook 'eshell-mode-hook 'evgeni-eshell-setup-keys)
  :config
  (setq eshell-banner-message "\n")

  (defun evgeni-eshell-setup-keys ()
    (evil-define-key 'insert eshell-mode-map
      (kbd "C-a") 'eshell-bol
      (kbd "C-d") 'evgeni-eshell-delete-char-or-hide
      (kbd "C-u") 'evgeni-eshell-kill-to-bol
      (kbd "C-p") 'eshell-previous-matching-input-from-input
      (kbd "C-n") 'eshell-next-matching-input-from-input)

    (evil-define-key 'normal eshell-mode-map
      (kbd "q") 'shell-pop
      (kbd "<return>") 'evgeni-eshell-normal-return
      [return] 'evgeni-eshell-normal-return))

  (defun evgeni-eshell-kill-to-bol ()
    (interactive)
    (kill-region (point) (progn (eshell-bol) (point))))

  (defun evgeni-eshell-delete-char-or-hide ()
    (interactive)
    (if (string-empty-p (eshell-get-old-input))
        (call-interactively 'shell-pop)
      (delete-char 1)))

  (defun evgeni-eshell-normal-return ()
    (interactive)
    (goto-char (point-max))
    (evil-insert 1))

  (add-hook 'shell-pop-in-after-hook 'evgeni-eshell-normal-return)

  (setq ;; eshell-buffer-shorthand t ...  Can't see Bug#19391
   eshell-scroll-to-bottom-on-input 'all
   eshell-error-if-no-glob t
   eshell-hist-ignoredups t
   eshell-save-history-on-exit t
   eshell-prefer-lisp-functions nil
   eshell-destroy-buffer-when-process-dies t)

  (defun evgeni-eshell-mode-hook ()
    (add-to-list 'eshell-visual-commands "ssh")
    (add-to-list 'eshell-visual-commands "tail")
    (add-to-list 'eshell-visual-commands "top")

    (eshell/alias "e" "find-file $1")
    (eshell/alias "ff" "find-file $1")
    (eshell/alias "emacs" "find-file $1")
    (eshell/alias "ee" "find-file-other-window $1")

    (eshell/alias "gd" "magit-diff-unstaged")
    (eshell/alias "gds" "magit-diff-staged")
    (eshell/alias "d" "dired $1")

    ;; The 'ls' executable requires the Gnu version on the Mac
    (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                  "/usr/local/bin/gls"
                "/bin/ls")))
      (eshell/alias "ll" (concat ls " -AlohG --color=always")))

    (set (make-local-variable 'company-backends) '(company-capf)))

  (add-hook 'eshell-mode-hook 'evgeni-eshell-mode-hook)

  (defun eshell/clear ()
    "Clear the eshell buffer."
    (let ((inhibit-read-only t))
      (erase-buffer)))

  (defun eshell/gcd ()
    "`cd` to the project root, if in a project"
    (if-let ((prj (project-current)))
        (eshell/cd (cdr prj))
      (user-error "Not in a project"))))

(use-package evil-numbers
  :straight t
  :after evil
  :bind (:map evil-normal-state-map
              ("C-a" . evil-numbers/inc-at-pt)
              ("g C-a" . evil-numbers/dec-at-pt)))

(use-package cperl-mode
  :init
  (defalias 'perl-mode 'cperl-mode)
  :mode (("\\.pl$"   . perl-mode)
         ("\\.pm$"   . perl-mode)
         ("\\.psgi$" . perl-mode)
         ("\\.t$"    . perl-mode)

 ;; `cperl-continued-statement-offset'
 ;;    Extra indentation given to a substatement, such as the
 ;;    then-clause of an if, or body of a while, or just a statement continuation.
 ;; `cperl-continued-brace-offset'
 ;;    Extra indentation given to a brace that starts a substatement.
 ;;    This is in addition to `cperl-continued-statement-offset'.
 ;; `cperl-brace-offset'
 ;;    Extra indentation for line if it starts with an open brace.
 ;; `cperl-brace-imaginary-offset'
 ;;    An open brace following other text is treated as if it the line started
 ;;    this far to the right of the actual line indentation.
 ;; `cperl-label-offset'
 ;;    Extra indentation for line that is a label.
 ;; `cperl-min-label-indent'
 ;;    Minimal indentation for line that is a label.
         )

  :config
  ;; (define-key 'help-command "P" 'cperl-perldoc)
  (setq cperl-indent-level 4)
  (setq cperl-continued-statement-offset 8)
  (setq cperl-font-lock t)
  (setq cperl-electric-lbrace-space t)
  (setq cperl-electric-parens nil)
  (setq cperl-electric-linefeed nil)
  (setq cperl-electric-keywords nil)
  (setq cperl-info-on-command-no-prompt t)
  (setq cperl-clobber-lisp-bindings t)
  (setq cperl-lazy-help-time 3)
  (setq cperl-indent-parens-as-block t)

  ;; compile command
  (add-hook 'cperl-mode-hook (lambda ()
                               (set (make-local-variable 'compile-command)
                                    (concat "perl -w -Mstrict -c "
                                            (shell-quote-argument
                                             (buffer-file-name))))))

  ;; does this work?
  (setq cperl-highlight-variables-indiscriminately t)

  (clear-abbrev-table cperl-mode-abbrev-table)
  (define-abbrev-table 'cperl-mode-abbrev-table
    '(("wran" "warn")
      ("wanr" "warn")))

  (custom-set-faces
   '(cperl-array-face ((t (:inherit font-lock-variable-name-face))))
   '(cperl-hash-face  ((t (:inherit font-lock-variable-name-face)))))

  (set-face-attribute cperl-invalid-face nil :underline nil)

  (setq cperl-invalid-face nil)

  ;; don't treat : as symbol
  (modify-syntax-entry ?: "." cperl-mode-syntax-table)

  (defun evgeni-perl-dump ()
    (interactive)
    (let ((word (thing-at-point 'word)))
      (save-excursion (end-of-line)
                      (newline)
                      (indent-according-to-mode)
                      ;; (insert (concat "use Data::Dump qw(pp); warn '" word ": ' . pp($" word ") . \"\\n\";"))
                      (insert (concat "use Data::Dumper; warn '" word ": ' . Dumper($" word ") . \"\\n\";")))))

  (with-eval-after-load 'evil
      (evil-define-key 'normal cperl-mode-map (kbd "] d") 'evgeni-perl-dump)))

(use-package minions
  :straight t
  :config
  (setq minions-mode-line-lighter "-")
  (setq minions-prominent-modes '(flymake-mode))
  (minions-mode))

(use-package moody
  :straight t
  :if (display-graphic-p)
  :config

  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)

  (setq-default moody-mode-line-buffer-identification
    '(:eval (moody-tab (format-mode-line (or (evgeni-buffer-path nil 'shadow) (propertized-buffer-identification "%b"))) 20 'up)))

  (defun evgeni-moody-setup (&rest _)
    (let ((box (plist-get (face-attribute 'mode-line :box) :color))
          (overline (plist-get (face-attribute 'mode-line :overline) :color))
          (underline (plist-get (face-attribute 'mode-line :underline) :color))

          (box-inactive (plist-get (face-attribute 'mode-line :box) :color))
          (overline-inactive (plist-get (face-attribute 'mode-line :overline) :color))
          (underline-inactive (plist-get (face-attribute 'mode-line :underline) :color))

          (default-bg (face-attribute 'default :background))
          (buffer-id-fg (face-attribute 'mode-line-buffer-id :foreground)))

      (when (and box (not overline) (not underline))
        (set-face-attribute 'mode-line          nil :overline   box)
        (set-face-attribute 'mode-line          nil :underline  box)
        (set-face-attribute 'mode-line          nil :box        nil))

      (when (and box-inactive (not overline-inactive) (not underline-inactive))
        (set-face-attribute 'mode-line-inactive          nil :overline   box-inactive)
        (set-face-attribute 'mode-line-inactive          nil :underline  box-inactive)
        (set-face-attribute 'mode-line-inactive          nil :box        nil))

      (when (equal (color-values buffer-id-fg) (color-values default-bg))
        (set-face-attribute 'mode-line-buffer-id nil :foreground (face-attribute 'mode-line :background)))))

  (evgeni-moody-setup)
  (advice-add 'load-theme :after #'evgeni-moody-setup))

(use-package beacon
  :straight t
  :disabled t
  :defer 1.5
  :config
  (setq beacon-size 80)
  (defun evgeni-set-beacon-color (&rest _)
    (setq beacon-color (face-attribute 'region :background)))

  (evgeni-set-beacon-color)

  (advice-add 'load-theme :after 'evgeni-set-beacon-color)

  (beacon-mode))

(use-package eglot
  :straight t
  :init
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'evgeni-eglot-ensure-if-git-repo)
  (add-hook 'go-ts-mode-hook 'evgeni-eglot-ensure-if-git-repo)
  (defun evgeni-eglot-ensure-if-git-repo ()
    (when (project-current)
      (eglot-ensure)))
  :custom
  (eglot-sync-connect 30)
  ;; (eglot-events-buffer-size 0) ;; disable logs
  (eglot-confirm-server-initiated-edits nil)
  :config
  (setq eglot-stay-out-of '(imenu))

  ;; experiment - speed up eglot
  (fset #'jsonrpc--log-event #'ignore)

  (add-to-list 'eglot-ignored-server-capabilities ':documentHighlightProvider)

  (with-eval-after-load 'evil
    (evil-define-minor-mode-key 'normal 'eglot--managed-mode
      (kbd ", a") (defhydra evgeni-eglot-hydra (:color blue)
                    ("a" eglot-code-actions "Code actions")
                    ("r" eglot-rename "Rename")
                    ("f" eglot-format "Format")
                    ("d" eglot-find-declaration "Find declaration")
                    ("i" eglot-find-implementation "Find implementation")
                    ("D" eglot-find-typeDefinition "Find type definition")
                    ("R" xref-find-references "Find references")
                    ("k" eldoc-doc-buffer "Doc buffer")))

    (evil-define-minor-mode-key 'visual 'eglot--managed-mode
      (kbd ", a") 'eglot-code-actions))

  (setq-default eglot-workspace-configuration
                '((:gopls .
                          ((staticcheck . t)
                           (gofumpt . t)
                           (usePlaceholders . t))))))


(use-package dape
  :ensure t
  :defer t
  :straight t)

(use-package python
  :defer t
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  (defun evgeni-python-dump ()
    (interactive)
    (let ((word (thing-at-point 'word)))
      (save-excursion (end-of-line)
                      (newline-and-indent)
                      (insert "import pprint")
                      (newline-and-indent)
                      (insert "print \"" word ": \" + pprint.pformat(" word ")"))))
  (with-eval-after-load 'evil
    (evil-define-key 'normal python-mode-map (kbd "] d") 'evgeni-python-dump)))

(use-package jq-mode
  :straight t
  :custom
  (jq-interactive-font-lock-mode #'json-ts-mode)
  :defer t
  :init
  (ex! "jq" 'jq-interactively))

(use-package deadgrep
  :straight t
  :defer t
  :init
  (ex! "deadgrep" 'deadgrep))

(use-package direnv
  :straight t
  :custom
  (direnv-show-paths-in-summary nil)
  :config
  (direnv-mode)
  (with-eval-after-load 'eshell
    (add-hook 'eshell-directory-change-hook 'direnv-update-directory-environment)))

(use-package autoinsert
  :defer 1.5
  :init
  (add-hook 'find-file-hook 'auto-insert)
  :config

  (setq auto-insert-query nil)

  (require 'yasnippet)
  (defun evgeni-autoinsert-yas-expand ()
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))

  (setq auto-insert-alist
        '((sh-mode . [ "template.sh" evgeni-autoinsert-yas-expand])
          (bash-ts-mode . [ "template.sh" evgeni-autoinsert-yas-expand])
          (go-ts-mode . [ "template.go" evgeni-autoinsert-yas-expand ])
          (go-mode . [ "template.go" evgeni-autoinsert-yas-expand ]))))

(use-package make-mode
  :defer t
  :config
  (modify-syntax-entry ?- "w" makefile-mode-syntax-table))

(use-package hideshow
  :after evil
  :defer t
  :config

  (evil-define-minor-mode-key 'normal 'hs-minor-mode [tab] 'evil-toggle-fold)
  (evil-define-minor-mode-key 'normal 'hs-minor-mode (kbd "TAB") 'evil-toggle-fold)

  (defun evgeni-hs-fold-on-first-occurance (regex)
    (hs-life-goes-on
    (save-excursion
      (goto-char (point-min))
      (when (ignore-errors (re-search-forward regex))
        (hs-hide-block))))))

(use-package inhibit-mouse
  :straight t
  :config
  (inhibit-mouse-mode))

(use-package protobuf-mode
  :straight t
  :defer t
  :config
  (add-hook 'protobuf-mode-hook 'electric-pair-local-mode))

(use-package focus
  :straight t
  :defer t
  :init
  (ex! "focus" 'focus-mode))

(use-package total-lines
  :straight t
  :defer 1.5
  :config
  (global-total-lines-mode)
  (setq mode-line-position '(("L%l/" (:eval (format "%d "total-lines))))))

(use-package so-long
  :disabled t
  :config
  (setq so-long-target-modes '(yaml-mode json-mode css-mode))
  (setq so-long-max-lines nil)
  (global-so-long-mode 1))

(use-package undo-fu
  :straight t
  :after evil
  :custom
  (evil-undo-system 'undo-fu)
  :config
  (global-undo-tree-mode -1)

  (use-package undo-fu-session
    :straight t
    :config
    (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
    (global-undo-fu-session-mode)))

(use-package gcmh
  :straight t
  :defer .5
  :config
  (gcmh-mode))

(use-package esup
  :straight t
  :defer)

(use-package emamux
  :straight t
  :after evil
  :if (getenv "TMUX") ;; run only in tmux
  :init

  (defun evgeni-tmux-split-window ()
    (interactive)
    (emamux:ensure-ssh-and-cd
     (emamux:tmux-run-command nil "split-window" "-p" "33")))

  (evil-define-key '(normal insert) evgeni-intercept-mode-map
    (kbd "C-c C-z") 'evgeni-tmux-split-window)

  (evil-define-key 'normal evgeni-intercept-mode-map
    (kbd "!!") (lambda () (interactive) (progn
                                          (emamux:send-command "Escape")
                                          (emamux:send-command "!!"))))

  (evil-define-key 'normal evgeni-intercept-mode-map
    (kbd "g!") 'emamux:run-command)

  (ex! "emamux" 'evgeni-emamux-send-region)
  (evil-define-command evgeni-emamux-send-region (beg end)
    (interactive "<r>")
    (emamux:send-region beg end)))

(use-package dtrt-indent
  :straight t
  :custom
  (dtrt-indent-verbosity 0)
  (dtrt-indent-max-lines 100) ;; try to speed-up this mode
  :config
  (dtrt-indent-global-mode))

(use-package treesit
  :if (version<= "29" emacs-version)
  :config

  (use-package treesit-auto
    :straight t
    :config
    (setq treesit-auto-install 'prompt)
    (global-treesit-auto-mode)

    ;; https://github.com/renzmann/treesit-auto/issues/32

    (with-eval-after-load 'org
      (add-to-list 'org-src-lang-modes '("json" . json-ts))
      (add-to-list 'org-src-lang-modes '("go" . go-ts))
      (add-to-list 'org-src-lang-modes '("bash" . bash-ts)))))

(use-package go-ts-mode
  :if (version<= "29" emacs-version)
  :mode "\\.go\\'"
  :config
  (setq go-ts-mode-indent-offset 4)

  (defun evgeni-go-ts-mode--goto-imports ()
    "Jump to end of imports, return the imports treesit node"
    (interactive)
    (when-let ((node (car (treesit-query-capture (treesit-buffer-root-node) "(import_declaration) @imports" nil nil t))))
      (goto-char (treesit-node-end node))))

  (defun evgeni-go-ts-import-add (package &optional alias)
    "Add PACKAGE if not already added.
Optionally add it with ALIAS."
    ;; (interactive (list (string-clean-whitespace (read-from-minibuffer "Import: ")) nil))
    (interactive (let* ((input (string-clean-whitespace (read-from-minibuffer "Import: ")))
                        (parts (string-split input " ")))
                   (if (eq (length parts) 1)
                       (list (nth 0 parts))
                     (list (string-trim (nth 1 parts) "\"" "\"") (nth 0 parts)))))
    (unless package
      (user-error "Invalid package"))
    (save-excursion
      (when-let* ((import-declaration (car (treesit-query-capture (treesit-buffer-root-node) "(import_declaration) @import_declaration" nil nil t)))
                  (imports (treesit-query-capture import-declaration "(import_spec path: (_) @imports)" nil nil t))
                  (packages (seq-map (lambda (x) (string-trim (treesit-node-text x t) "\"" "\"")) imports)))
        (if (seq-contains-p packages package)
            (when (called-interactively-p)
              (message "Package already imported"))
          (goto-char (treesit-node-end import-declaration))
          (move-beginning-of-line nil)
          (insert
           (if alias (concat alias " ") "")
           "\"" package "\"")
          (indent-according-to-mode)
          (newline-and-indent)
          (when (called-interactively-p)
            (message "Added"))))))

  (defun evgeni-go-ts-dump ()
    (interactive)
    (let ((word (evgeni-word-at-point-or-region)))
      (save-excursion (end-of-line)
                      (newline-and-indent)
                      (insert (concat "fmt.Printf(\"" word ": %+v\\n\", " word ")"))
                      (evgeni-go-ts-import-add "fmt"))))

  (defun evgeni-go-ts-dump-alt ()
    (interactive)
    (let ((word (evgeni-word-at-point-or-region)))
      (save-excursion (end-of-line)
                      (newline-and-indent)
                      (insert (concat "fmt.Printf(\"" word ": %v\\n\", litter.Options{HidePrivateFields: false}.Sdump(" word "))"))
                      (evgeni-go-ts-import-add "github.com/sanity-io/litter")
                      (evgeni-go-ts-import-add "fmt"))))

  (defun evgeni-go-ts-dump-json ()
    (interactive)
    (let ((word (evgeni-word-at-point-or-region)))
      (save-excursion (end-of-line)
                      (newline-and-indent)
                      (insert (concat "dddd, _ := json.MarshalIndent(" word ", \"\", \"  \")"))
                      (newline-and-indent)
                      (insert (concat "fmt.Printf(\"" word ": %+v\\n\", string(dddd))"))
                      (evgeni-go-ts-import-add "encoding/json")
                      (evgeni-go-ts-import-add "fmt"))))

  (define-key go-ts-mode-map (kbd "C-c i" )'evgeni-go-ts-mode--goto-imports)

  (with-eval-after-load 'evil
    (evil-define-key 'normal go-ts-mode-map (kbd "] d") 'evgeni-go-ts-dump)
    (evil-define-key 'normal go-ts-mode-map (kbd "] D") 'evgeni-go-ts-dump-alt)))

(use-package olivetti
  :straight t
  :config
  (defun evgeni-scratch-olivetti ()
    (with-current-buffer "*scratch*"
      (olivetti-mode 1)
      (goto-char 0)))
  (add-hook 'emacs-startup-hook 'evgeni-scratch-olivetti))

(use-package breadcrumb
  :disabled t
  :load-path "~/dev/breadcrumb"
  :config
  (breadcrumb-mode))

(use-package kkp
  :straight t
  :unless (display-graphic-p)
  :config
  (global-kkp-mode))

(use-package gptel
  :straight t
  :defer t)

(use-package pulsar
  :straight t
  :defer .5
  :if (display-graphic-p)
  :init
  (setq pulsar-pulse-on-window-change t
        pulsar-pulse-functions '(evil-window-top
                                 evil-window-middle
                                 evil-window-bottom
                                 evil-window-left
                                 evil-window-right
                                 evil-window-up
                                 evil-window-down
                                 evil-forward-paragraph
                                 evil-backward-paragraph
                                 org-backward-paragraph
                                 org-forward-paragraph
                                 org-previous-visible-heading
                                 org-next-visible-heading
                                 beginning-of-defun
                                 end-of-defun))
  :config
  (pulsar-global-mode))
