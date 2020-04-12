;; -*- lexical-binding: t; -*-

(defconst emacs-start-time (current-time))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed (float-time (time-subtract (current-time)
                                                       emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed)))
          t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(unless (boundp 'package-quickstart)
  (package-initialize))

;; tweak GC during startup
(defvar evgeni--file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-timer 5 nil
                                 (lambda ()
                                   (setq gc-cons-threshold 16777216
                                         gc-cons-percentage 0.1
                                         file-name-handler-alist evgeni--file-name-handler-alist)))))


;; graphic settings
(when (display-graphic-p)
  ;; (set-frame-font (font-spec :family "DejaVu Sans Mono" :size 13 :weight 'regular) nil t)
  (set-frame-font (font-spec :family "Fira Code" :size 13 :weight 'regular) nil t)
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

;; in modeline, show file path, relative to the project root
(defun evgeni-maybe-propertize-with-face (text face)
  (if face
      (propertize text 'face face)
    text))

(require 'subr-x)
(defun evgeni-buffer-path (&optional path prj-path-face buffer-id-face)
  "Return current buffer path relative to project root.
Return nil if not in a project"
  (when-let* ((path (or path buffer-file-truename))
              (prj (cdr-safe (project-current nil (directory-file-name path))))
              (prj-parent (file-name-directory (directory-file-name (expand-file-name prj)))))
    (concat (evgeni-maybe-propertize-with-face (file-relative-name
                                                (file-name-directory path)
                                                prj-parent)
                                               prj-path-face)
            (evgeni-maybe-propertize-with-face
             (file-name-nondirectory path)
             (or buffer-id-face 'mode-line-buffer-id)))))

(with-eval-after-load 'subr-x
  (setq-default mode-line-buffer-identification
                '(:eval (format-mode-line (or (evgeni-buffer-path)
                                              (propertized-buffer-identification "%b"))))))

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

(defmacro use-package! (name &rest plist)
  (declare (indent 1))
  `(with-eval-after-load 'evil
     (use-package ,name ,@plist)))

;; imenu should recognize `use-package!`
(with-eval-after-load 'lisp-mode
  (add-to-list 'lisp-imenu-generic-expression
               (list "Packages" (concat (concat "^\\s-*(" (regexp-opt '("use-package!") t) "\\s-+\\(")
                                        (or (bound-and-true-p lisp-mode-symbol-regexp)
                                            "\\(?:\\sw\\|\\s_\\|\\\\.\\)+") "\\)") 2)))

;; use-package
(setq use-package-enable-imenu-support t)
(unless (package-installed-p 'use-package)
  ;; (package-initialize)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-verbose nil)

(use-package esup
  :ensure t
  :defer t)

(use-package no-littering
  :ensure t
  :demand t)

(use-package dashboard
  :ensure t
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
  (setq dashboard-items '((projects  . 20)))
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-footer-messages nil)
  (dashboard-setup-startup-hook))

;; upgrade installed packages
(defun evgeni-upgrade-packages ()
  (interactive)
  (save-window-excursion
    (package-refresh-contents)
    (package-list-packages t)
    (package-menu-mark-upgrades)
    (package-menu-execute t)))

(defun evgeni-refresh-packages ()
  (interactive)
  (package-refresh-contents t))

(ex! "upgrade-packages" 'evgeni-upgrade-packages)
(ex! "refresh-packages" 'evgeni-refresh-packages)

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
(use-package habamax-theme :ensure t :defer t
  :config
  (let ((color-bg-highlight "#ececef")
        (color-dim-bg "#f5f9fe"))
    (custom-theme-set-faces
     'habamax
     `(dired-directory ((t (:inherit default :background ,color-dim-bg))))
     `(company-tooltip-selection ((t (:inherit ivy-current-match))))
     `(company-tooltip ((t (:background "#f5f5f5"))))
     `(company-scrollbar-bg ((t (:background ,color-bg-highlight)))))))
(use-package one-themes :ensure t :defer t)        ;; ok
(use-package greymatters-theme :ensure t :defer t
  :config
  (let ((region "#e3e4e6"))
    (custom-theme-set-faces
     'greymatters
     `(region ((t (:background ,region)))))))
(use-package chyla-theme :ensure t :defer t)       ;; ok, green
(use-package doom-themes :ensure t :defer t)
(use-package eclipse-theme :ensure t :defer t)
(use-package flatui-theme :ensure t :defer t)
(use-package twilight-bright-theme :ensure t :defer t)
(use-package espresso-theme :ensure t :defer t)
(use-package apropospriate-theme :ensure t :defer t)
(use-package material-theme :ensure t :defer t)
(use-package tango-plus-theme :ensure t :defer t)
(use-package plan9-theme :ensure t :defer t)
(use-package doom-themes :ensure t :defer t)
(use-package espresso-theme :ensure t :defer t)
(use-package tommyh-theme :ensure t :defer t)
(use-package eziam-theme :ensure t :defer t)
(use-package modus-operandi-theme :ensure t :defer t)
(use-package hydandata-light-theme :ensure t :defer t)
(use-package color-theme-sanityinc-tomorrow :ensure t :defer t)
(use-package leuven-theme :defer t
  :init
  (theme! 'leuven
    '(eshell-prompt ((t (:inherit 'font-lock-keyword-face))))
    '(dired-ignored ((t (:inherit 'shadow :strike-through t))))))

(use-package white-sand-theme :ensure t :defer t)
(use-package silkworm-theme :ensure t :defer t)
(use-package oldlace-theme :ensure t :defer t
  :config
  (custom-theme-set-faces
   'oldlace
   `(region ((t (:foreground "#525252" :background "#d0c9bd"))))))

;; dark themes
(use-package dracula-theme :ensure t :defer t)


(use-package emacs
  :defer .1
  :config
  ;; load theme
  (load-theme (if (display-graphic-p)
                  'modus-operandi ;; 'leuven ;; 'doom-one-light ;; 'habamax ;; 'dichromacy ;; 'doom-one-light ;; 'twilight-bright ;; 'apropospriate-dark ;;'tango-dark ;; tango-plus flatui
                'doom-one-light)
              t)
  ;; bind command-option-H to hide other windows, just like every other OS X app
  (when (string-equal system-type "darwin")
    (define-key global-map (kbd "M-s-˙") 'ns-do-hide-others))

  (defun evgeni-make-frame ()
    (interactive)
    (make-frame `((top . ,(+ (cdr (frame-position)) 15))
                  (left . ,(+ (car (frame-position)) 15))
                  (width . ,(- (frame-width) 3))
                  (height . ,(- (frame-height) 2))))
    (switch-to-buffer "*scratch*"))

  (define-key global-map (kbd "s-n") 'evgeni-make-frame))

;; restore frame position - https://github.com/aaronjensen/restore-frame-position
(when (display-graphic-p)
  (setq restore-frame-position-file (expand-file-name "frame-position.el" no-littering-var-directory))

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

;; load mail.el if any
(when (file-readable-p (concat user-emacs-directory "mail.el"))
  (run-with-idle-timer 2 nil 'load-file (concat user-emacs-directory "mail.el")))

;; workaround for OSX https://emacs.stackexchange.com/questions/18045/how-can-i-retrieve-an-https-url-on-mac-os-x-without-warnings-about-an-untrusted
;; the .pem file is installed with brew install libresll
(use-package gnutls
  :defer t
  :if (file-readable-p "/usr/local/etc/libressl/cert.pem")
  :config
  (add-to-list
   'gnutls-trustfiles "/usr/local/etc/libressl/cert.pem"))

;; use M-u instaed of C-u for universal argument
(define-key global-map (kbd "C-u") 'kill-whole-line)
(define-key global-map (kbd "M-u") 'universal-argument)
(define-key universal-argument-map (kbd "C-u") nil)
(define-key universal-argument-map (kbd "M-u") 'universal-argument-more)

;; load evil early
(use-package evil
  :ensure t
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
                    evil-ex))
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

  (defun evgeni-first-error ()
    (interactive)
    (cond
     ((bound-and-true-p flycheck-mode) (flycheck-first-error) (flycheck-list-errors))
     ((bound-and-true-p flymake-mode) (goto-char (point-min)) (call-interactively 'flymake-goto-next-error) (flymake-show-diagnostics-buffer))
     (t (first-error))))

  (defun evgeni-next-error ()
    (interactive)
    (cond
     ((bound-and-true-p flycheck-mode) (flycheck-next-error) (flycheck-list-errors))
     ((bound-and-true-p flymake-mode) (call-interactively 'flymake-goto-next-error) (flymake-show-diagnostics-buffer))
     (t (next-error))))

  (defun evgeni-previous-error ()
    (interactive)
    (cond
     ((bound-and-true-p flycheck-mode) (flycheck-previous-error) (flycheck-list-errors))
     ((bound-and-true-p flymake-mode) (call-interactively 'flymake-goto-prev-error) (flymake-show-diagnostics-buffer))
     (t (previous-error))))

  (evil-add-command-properties 'evgeni-next-error :jump t :repeat 'motion)
  (evil-add-command-properties 'evgeni-previous-error :jump t :repeat 'motion)

  (define-key evil-normal-state-map (kbd "RET") 'evgeni-save-file)
  (define-key evil-normal-state-map (kbd "[ Q") 'evgeni-first-error)
  (define-key evil-normal-state-map (kbd "] Q") (lambda () (interactive) (goto-char (point-max)) (evgeni-previous-error)))
  (define-key evil-normal-state-map (kbd "] q") 'evgeni-next-error)
  (define-key evil-normal-state-map (kbd "[ q") 'evgeni-previous-error)
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

  (define-key evil-motion-state-map "*" 'evgeni-star)
  (define-key evil-motion-state-map "g*" 'evgeni-g-star)
  (define-key evil-normal-state-map (kbd ", *" )
    (lambda ()
      (interactive)
      (counsel-grep-or-swiper (substring-no-properties (thing-at-point 'word)))))

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
      (exchange-dot-and-mark)
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

  ;; start %s/.../... with the last search pattern
  (defun evgeni-start-ex-substitute ()
    (interactive)
    (let ((pattern (or (evil-ex-pattern-regex evil-ex-search-pattern) "")))
      (let ((replace-with (replace-regexp-in-string "\\\\[<>]" "" pattern))
            (ex-prefix (if (evil-visual-state-p) "'<,'>" "%")))
        (evil-ex (format "%ss//%s" ex-prefix replace-with)))))

  (define-key evil-normal-state-map (kbd ",m") 'evgeni-start-ex-substitute)
  (define-key evil-visual-state-map (kbd ",m") 'evgeni-start-ex-substitute)

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
          (evgeni-narrow-to-region-indirect beg end)
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

  ;; other packages which integrate with evil
  (use-package evil-collection
    :ensure t
    :init
    (setq evil-collection-outline-bind-tab-p nil)
    :config
    (delete 'outline evil-collection-mode-list)
    (evil-collection-init))

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
    :bind (:map evil-normal-state-map
                ("gc" . evil-commentary)))

  (use-package evil-exchange
    :ensure t
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
                        (evgeni-inhibit-operator #'evil-exchange-cancel))))))

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

  (use-package evil-indent-plus
    :ensure t
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

  (message "Loading evil-mode...done (%.3fs)" (float-time (time-subtract (current-time) emacs-start-time))))

(use-package! avy
  :ensure t
  :defer .5
  :bind (:map evil-normal-state-map
              ;; ("gj" . avy-goto-word-1-below)
              ;; ("gk" . avy-goto-word-1-above)
              ("gj" . avy-goto-char-timer)
              ("gk" . avy-goto-char-timer)
              :map evil-motion-state-map
              ;; ("gh" . avy-goto-char-timer)
              )
  :config
  (setq avy-timeout-seconds .5)
  (custom-set-faces
   '(avy-lead-face-0 ((t (:inherit 'highlight))))
   '(avy-lead-face ((t (:inherit 'highlight)))))

  (setq avy-background t)
  (setq avy-keys '(?a ?s ?d ;; ?f
                      ?g ;; ?h
                      ?j ?k
                      ?l ?\;
                      ?1 ?2 ?3 ?4 ?5 ?6))
  (setq avy-style 'at-full)

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

(use-package! dired
  :bind (:map evil-normal-state-map
              ("-" . evgeni-dired-current-dir))
  :config
  (setq dired-listing-switches "-alh"
        dired-auto-revert-buffer t
        dired-dwim-target t
        dired-use-ls-dired nil)
  (evil-define-key 'normal dired-mode-map
    "-" 'dired-up-directory
    "U" 'evgeni-dired-unmark-all-marks)

  (add-hook 'dired-mode-hook 'dired-hide-details-mode)

  (defun evgeni-dired-unmark-all-marks()
    "call `magit-status' on double-U press `U U'"
    (interactive)
    (if (eq this-command last-command)
        (progn

          (magit-status))
      (dired-unmark-all-marks)))

  (defun evgeni-dired-current-dir ()
    (interactive)
    (let ((file buffer-file-name))
      (dired ".")
      (when file
        (dired-goto-file file)))))

(use-package! dired-subtree
  :ensure t
  :commands dired-subtree-toggle
  :init
  (with-eval-after-load 'dired
    (define-key dired-mode-map [tab] 'dired-subtree-toggle))

  (custom-set-faces
   '(dired-subtree-depth-1-face ((t (:background unspecified))))
   '(dired-subtree-depth-2-face ((t (:background unspecified))))
   '(dired-subtree-depth-3-face ((t (:background unspecified))))
   '(dired-subtree-depth-4-face ((t (:background unspecified))))
   '(dired-subtree-depth-5-face ((t (:background unspecified))))
   '(dired-subtree-depth-6-face ((t (:background unspecified))))))

(use-package! magit
  :ensure t
  :defer 30
  :bind (:map evil-normal-state-map
              ("U U" . magit-status)
              ("U u" . magit-file-dispatch)
              ("U w" . magit-stage-file)
              ("U d" . evgeni-magit-diff-unstaged-buffer-file)
              ("U L" . magit-log-head)
              ("U l" . magit-log-buffer-file)
              ("U r" . magit-file-checkout)
              ("U r" . evgeni-magit-file-checkout)
              ("U c" . magit-commit)
              ("U b" . magit-branch)
              ("U z" . magit-stash)
              ("U m" . magit-merge)
              ("U p" . magit-push)
              ("U f" . magit-fetch)
              ("U F" . magit-pull)
              ("U B" . magit-checkout))
  :commands (magit-find-file)
  :init
  (ex! "ma[git]" 'magit-status)
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

  ;; lower untracked
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-untracked-files 'magit-insert-staged-changes 1)

  ;; lower stashes
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-stashes 'magit-insert-unpushed-to-pushremote 1)

  (define-key magit-diff-mode-map "e" nil) ;; no ediff

  (add-hook 'git-commit-mode-hook 'flyspell-mode))

(use-package! smerge-mode
  :defer t
  :config
  (evil-define-minor-mode-key 'normal 'smerge-mode
    (kbd "] n")     'smerge-next
    (kbd "[ n")     'smerge-prev
    (kbd "C-c C-c") 'smerge-keep-current
    (kbd "C-c C-k") 'smerge-kill-current
    ;; "b" mnemonic for both, "a" for all
    (kbd "C-c C-b") 'smerge-keep-all
    (kbd "C-c C-a") 'smerge-keep-all))

(use-package! vdiff
  :ensure t
  :defer t
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

  (evil-define-command evgeni-line-diff (begin end)
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

(use-package! vdiff-magit
  :ensure t
  :after magit
  :init
  :commands vdiff-magit-show-unstaged
  :bind (:map evil-normal-state-map
              ("U D" . vdiff-magit))
  :config
  (setq vdiff-magit-stage-is-2way t) ;; Only use two buffers (working file and index) for vdiff-magit-stage
  (define-key magit-mode-map "e" 'vdiff-magit-dwim)
  (define-key magit-mode-map "E" 'vdiff-magit))

(use-package! org
  :defer 30
  :ensure t
  :commands evgeni-journal-file-today
  :init
  (ex! "journal" 'evgeni-journal-file-today)
  :config
  ;; text-mode-like parapgraph separation
  (add-hook 'org-mode-hook (lambda ()
                             (setq paragraph-start "\\|[ 	]*$"
                                   paragraph-separate "[ 	]*$")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t) (dot . t) (python . t)))

  (evil-ex-define-cmd "cap[ture]" 'org-capture)

  (setq org-startup-indented t
        org-imenu-depth 3
        org-startup-folded "showall"
        org-confirm-babel-evaluate nil)

  (evil-define-key '(normal insert) org-mode-map
    (kbd "TAB") 'org-cycle)
  (evil-define-key 'normal org-mode-map
    "o" (kbd "A RET")
    (kbd ", f") 'counsel-org-goto
    (kbd "gx") 'org-open-at-point
    (kbd "] m") 'org-next-visible-heading
    (kbd "[ m") 'org-previous-visible-heading)

  ;; daily journal file, `:journal'
  (defun evgeni-journal-file-today ()
    "Return filename for today's journal entry."
    (interactive)
    (let ((daily-name (format-time-string "%Y_%b_%d.org")))
      (find-file (expand-file-name (concat org-directory "/journal/"  daily-name)))))

  ;; `:babellib' ex command to ingest my library of babel
  (ex! "babellib" 'evgeni-babellib)
  (defun evgeni-babellib ()
    (interactive)
    (org-babel-lob-ingest "~/org/snippets.org"))

  ;; better org RETURN https://github.com/howardabrams/dot-files/blob/master/emacs-org.org#better-org-return
  (defun evgeni-org-return (&optional ignore)
    "Add new list item, heading or table row with RET.
A double return on an empty element deletes it.
Use a prefix arg to get regular RET. "
    (interactive "P")
    (if ignore
        (org-return)
      (cond
       ;; lists end with two blank lines, so we need to make sure we are also not
       ;; at the beginning of a line to avoid a loop where a new entry gets
       ;; created with only one blank line.
       ((and (org-in-item-p) (not (bolp)))
        (if (save-excursion
              (goto-char (line-beginning-position))
              (org-element-property :contents-begin (org-element-context)))
            (org-insert-item (org-at-item-checkbox-p))
          (beginning-of-line)
          (setf (buffer-substring
                 (line-beginning-position) (line-end-position)) "")))
       ((org-at-heading-p)
        (if (not (string= "" (org-element-property :title (org-element-context))))
            (org-meta-return)
          (beginning-of-line)
          (setf (buffer-substring
                 (line-beginning-position) (line-end-position)) "")))
       ((org-at-table-p)
        (if (-any?
             (lambda (x) (not (string= "" x)))
             (nth
              (- (org-table-current-dline) 1)
              (org-table-to-lisp)))
            (org-return)
          ;; empty row
          (beginning-of-line)
          (setf (buffer-substring
                 (line-beginning-position) (line-end-position)) "")
          (org-return)))
       (t
        (org-return)))))

  (define-key org-mode-map (kbd "RET")  'evgeni-org-return))

(use-package org-make-toc
  :ensure t
  :defer t)

(use-package org-bullets
  :ensure t
  :after org
  :config

  (setq org-bullets-bullet-list
        '("◉" "○" "•" "▸"))

  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-download
  :ensure t
  :after org)

(use-package ob-restclient
  :ensure t
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(use-package! xref
  :bind (:map evil-normal-state-map
              ("C-]" . xref-find-definitions)
              ("g C-]" . xref-find-references)
              ("C-w C-]" . xref-find-definitions-other-window))
  :config
  (ex! "tag" (lambda ()
               (interactive)
               ;; make `xref-find-definitions prompt for a tag
               (let ((current-prefix-arg '(4)))
                 (call-interactively 'xref-find-definitions))))
  (evil-define-key 'normal xref--xref-buffer-mode-map "q" 'delete-window)
  ;; don't use swiper in this buffer - swiper clears the highlighting in the xref buffer
  (evil-define-key 'normal xref--xref-buffer-mode-map "/" 'evil-ex-search-forward)

  ;; don't pulse, beacon already does this
  (setq xref-after-jump-hook (delete 'xref-pulse-momentarily xref-after-jump-hook))
  (setq xref-after-return-hook (delete 'xref-pulse-momentarily xref-after-return-hook)))

(use-package ivy-xref
  :ensure t
  :defer t
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
  :config)

(use-package! ivy
  :ensure t
  :defer t
  :bind (:map evil-normal-state-map
              ("C-c C-r" . ivy-resume))
  :init
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'full)
  (define-key ivy-minibuffer-map (kbd "C-w") 'backward-kill-word)
  (define-key ivy-minibuffer-map (kbd "C-u") (lambda () (interactive) (kill-region (point) (point-at-bol))))
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "C-c o") 'ivy-occur)

  (evil-define-key 'normal ivy-occur-mode-map (kbd "RET") 'ivy-occur-press) ;; default is 'ivy-occur-press-and-switch

  (modify-syntax-entry ?$ "." ivy-occur-grep-mode-syntax-table)

  ;; unbind keys which conflict with evil
  (unbind-key "C-d" ivy-occur-grep-mode-map)
  (unbind-key "f" ivy-occur-grep-mode-map)
  (define-key ivy-occur-grep-mode-map (kbd "C-c C-d") 'ivy-occur-delete-candidate)
  (define-key ivy-occur-grep-mode-map (kbd "C-c d") 'ivy-occur-delete-candidate)

  ;; C-r C-w to read word at point
  (unbind-key "C-r" ivy-minibuffer-map)
  (define-key ivy-minibuffer-map (kbd "C-r C-w") 'ivy-next-history-element)

  ;; use different colors in ivy-switch-buffer
  (setq ivy-switch-buffer-faces-alist
        '((emacs-lisp-mode . ivy-minibuffer-match-face-1)
          (dired-mode . ivy-subdir)
          (org-mode . org-level-4)))

  ;; push/pop view
  (evil-global-set-key 'normal (kbd "C-c v") 'ivy-push-view)
  (evil-global-set-key 'normal (kbd "C-c V") 'ivy-pop-view))

(use-package! swiper
  :ensure t
  :bind (:map evil-motion-state-map
              ("/" . swiper)
              :map evil-normal-state-map
              (", /" . swiper-all))
  :config
  (setq swiper-goto-start-of-match t)
  (evil-set-command-properties 'swiper :jump t))

(use-package! counsel
  :ensure t
  :init
  (ex! "faces" 'counsel-faces)
  (ex! "find-lib" 'counsel-find-library)
  (ex! "set[-variable]" 'counsel-set-variable)

  (evil-define-key '(normal visual) evgeni-intercept-mode-map
    (kbd "K") 'evgeni-counsel-git-grep)

  :bind (([remap find-file] . counsel-find-file)
         ([remap execute-extended-command] . counsel-M-x)
         ([remap describe-function] . counsel-describe-function)
         ([remap describe-key] . describe-key)
         ([remap describe-variable] . counsel-describe-variable)
         :map evil-normal-state-map
         ("/" . counsel-grep-or-swiper)
         (", f"   . counsel-imenu)
         ("g P" . counsel-yank-pop)
         ("g p" . counsel-evil-registers))
  :config
  (evil-set-command-properties 'counsel-grep-or-swiper :jump t)
  (setq counsel-grep-base-command "rg --smart-case %s %s") ;; use rg --smart-case to mimic swiper's `auto' case search

  (setq counsel-git-cmd "git ls-files --cached --others --exclude-standard")
  (setq counsel-rg-base-command "rg --no-ignore-messages -i -M 120 --no-heading --line-number --color never %s .")
  (setq counsel-org-goto-face-style 'org)

  (setq counsel-git-grep-skip-counting-lines t)
  (setq counsel-yank-pop-after-point t)

  (define-key counsel-describe-map (kbd "C-]") 'counsel-find-symbol)
  (define-key counsel-git-grep-map (kbd "C-c C-c") 'ivy-occur)
  (define-key ivy-occur-grep-mode-map (kbd "C-c C-c") 'wgrep-change-to-wgrep-mode)

  ;; TODO maybe drop the "rg" support in here - always use git grep
  (setq evgeni-counsel-git-grep-rigpreg-found (executable-find "rg"))

  (when evgeni-counsel-git-grep-rigpreg-found
    (setq counsel-rg-base-command "rg --no-ignore-messages -S -M 500 --no-heading --hidden -g '!.git' --line-number --color never %s ."))

  (evil-define-key 'normal evgeni-intercept-mode-map
    (kbd "g /") (if evgeni-counsel-git-grep-rigpreg-found
                    'evgeni-counsel-rg
                  'counsel-git-grep))

  (defun evgeni-counsel-rg ()
    (interactive)
    (if (eq major-mode 'dired-mode)
        (counsel-rg nil default-directory nil "rg <dir>: ")
      (call-interactively 'counsel-rg)))

  (defun evgeni-counsel-git-grep ()
    (interactive)
    (let ((word (substring-no-properties
                 (if (region-active-p)
                     (buffer-substring (region-beginning) (region-end))
                   (thing-at-point 'word)))))
      (if evgeni-counsel-git-grep-rigpreg-found
          (funcall-interactively 'counsel-rg word) ;; TODO try funcall-interactively, counsel-rg hungs occasionally
        (counsel-git-grep nil word))))

  (evil-set-command-properties 'counsel-imenu :jump t)
  (evil-set-command-properties 'counsel-find-file :jump t)
  (evil-set-command-properties 'counsel-git-grep :jump t)
  (evil-set-command-properties 'evgeni-counsel-git-grep :jump t)

  ;; copy of `counsel-faces' using standard faces only
  (ex! "standard-faces" 'evgeni-standard-faces)
  (defun evgeni-standard-faces ()
    (interactive)
    (let* ((face-list '(default bold italic bold-italic underline fixed-pitch fixed-pitch-serif variable-pitch shadow highlight isearch query-replace lazy-highlight region secondary-selection trailing-whitespace escape-glyph homoglyph nobreak-space nobreak-hyphen mode-line mode-line-inactive mode-line-highlight mode-line-buffer-id header-line header-line-highlight vertical-border minibuffer-prompt fringe cursor tooltip mouse scroll-bar tool-bar menu tty-menu-enabled-face tty-menu-disabled-face tty-menu-selected-face))
           (names (mapcar #'symbol-name face-list))
           (ivy-format-function
            (counsel--faces-format-function
             (format "%%-%ds %%s"
                     (apply #'max 0 (mapcar #'string-width names))))))
      (ivy-read "Standard face: " names
                :require-match t
                :history 'face-name-history
                :preselect (counsel--face-at-point)
                :action counsel-describe-face-function
                :caller 'counsel-faces))))

(use-package! imenu-anywhere
  :ensure t
  :bind (:map evil-normal-state-map
              (", F"  . imenu-anywhere))
  :config
  (evil-set-command-properties 'imenu-anywhere :jump t))

(use-package! winner
  :defer .5
  :bind (:map evil-normal-state-map ("z u" . hydra-winner/winner-undo))
  :config
  (winner-mode)
  (defhydra hydra-winner ()
    "Winner"
    ("u" winner-undo "undo")
    ("C-r" winner-redo "redo" :exit t)
    ("r" winner-redo "redo" :exit t)))

(use-package! projectile
  :ensure t
  :defer 3
  :commands projectile-switch-project-by-name projectile-project-p projectile-relevant-known-projects evgeni-project-files
  :init
  (evil-define-key 'normal evgeni-intercept-mode-map
    (kbd "g SPC") 'ivy-switch-buffer
    (kbd ", SPC") 'projectile-switch-project
    (kbd "SPC") 'evgeni-project-files)
  (ex! "proj[ectile]" 'projectile-switch-project)
  (ex! "prj" 'projectile-switch-project)
  :config
  (require 'counsel)

  ;; TODO use projectile-project-buffers, or something else to make recent-files also include the project buffers
  (defun evgeni-project-files()
    (interactive)
    (if (projectile-project-p)
        (let* ((prj-p (projectile-project-p))
               (recent-files (projectile-recently-active-files))
               (prj-files (projectile-current-project-files))
               (rest-prj-files (projectile-difference prj-files recent-files))
               (shadowed-rest-prj-files (mapcar (lambda (f) (propertize f 'face 'ivy-virtual)) rest-prj-files))
               (candidates (funcall (if buffer-file-name 'cdr 'identity) (append recent-files shadowed-rest-prj-files))))
          (find-file
           (projectile-expand-root
            (ivy-read "Project files: " candidates))))
      (ivy-read "No project " nil)))

  (setq projectile-switch-project-action
        (lambda ()
          (dired (projectile-project-root))))
  (setq projectile-completion-system 'ivy)
  (setq projectile-git-submodule-command nil)
  (projectile-global-mode))

(use-package! company
  :ensure t
  :defer .5
  :config

  (setq company-minimum-prefix-length 2
        company-tooltip-limit 20
        company-idle-delay .3 ;; .05
        company-echo-delay 0 ;; remove annoying blinking
        company-begin-commands '(self-insert-command)
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-backends '(company-capf company-dabbrev company-ispell)
        company-transformers '(company-sort-by-occurrence))

  (with-eval-after-load 'yasnippet
          (nconc company-backends '(company-yasnippet)))

  ;; with company's tab-and-go feature, allow snippet/template expansion with RET
  (define-key company-active-map [return] 'company-complete-selection)
  (define-key company-active-map (kbd "RET") 'company-complete-selection)

  (unbind-key "\C-w" company-active-map)
  (unbind-key "\C-u" company-active-map)
  (define-key company-active-map (kbd "C-/") 'company-search-candidates)

  (global-company-mode +1))

(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (company-quickhelp-mode))

(use-package! yasnippet
  :ensure t
  :defer .5
  :config
  (setq yas-verbosity 2)
  (yas-global-mode)
  ;; (evil-define-minor-mode-key 'insert yas-minor-mode (kbd "C-c y") 'yas-expand)
  (evil-define-minor-mode-key 'insert yas-minor-mode (kbd "C-c y") 'company-yasnippet)
  (ex! "yas-new" 'yas-new-snippet))

(use-package evil-expat
  ;; :load-path "~/dev/evil-expat"
  :ensure t
  :defer .5)

(use-package evil-goggles
  :defer .5
  :ensure t
  :load-path "~/dev/evil-goggles"
  :config
  ;; (setq evil-goggles-blocking-duration 0.100)
  (setq evil-goggles-pulse nil)
  (evil-goggles-use-diff-faces)

  ;; (setq evil-goggles-duration 0.1
  ;;       evil-goggles-enable-delete nil)
  (evil-goggles-mode))

(use-package recentf
  :defer .5
  :init
  (setq recentf-max-saved-items 500)
  (setq recentf-exclude '("/tmp/" ;; "/ssh:"
                          "\.pyc$"
                          (expand-file-name "~/Maildir/")))
  (setq recentf-auto-cleanup 3600) ;; cleanup after an hour of idleness
  :config
  (recentf-mode))

(use-package saveplace
  :defer .5
  :config
  (save-place-mode))

(use-package savehist
  :defer .5
  :init
  (setq
   enable-recursive-minibuffers t ; commands in minibuffers
   history-length 1000
   savehist-additional-variables '(mark-ring
                                   global-mark-ring
                                   search-ring
                                   regexp-search-ring
                                   extended-command-history)
   savehist-autosave-interval 60)
  (savehist-mode t))

(use-package! dired-single
  :ensure t
  :commands dired-single-buffer
  :init
  (evil-define-key 'normal dired-mode-map (kbd "RET") 'dired-single-buffer))

(use-package wdired
  :commands wdired-change-to-wdired-mode
  :init
  (evil-set-initial-state 'wdired-mode 'normal)
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
  :defer t
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

(use-package flycheck-inline
  :ensure t
  :after flycheck)

(use-package! flymake
  :ensure t
  :defer t
  :config
  (setq flymake-start-syntax-check-on-newline nil))

(use-package! flyspell-correct-ivy
  :ensure t
  :defer t
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package abbrev
  :defer t
  :init
  (setq-default abbrev-mode t)
  (setq save-abbrevs nil))

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

(use-package shackle
  :ensure t
  :defer .5
  :config
  (shackle-mode 1)
  (setq shackle-rules
        '(
          ("*xref*"                        :align below :size 10 :noselect t)
          ("*Help*"                        :align below :size 16 :select t)
          ("*Backtrace*"                   :align below :size 25 :noselect t)
          ("*Flycheck error messages*"     :align below :size 0.25)
          ("*Flycheck errors*"             :align below :size 0.20)
          (flymake-diagnostics-buffer-mode :align below :size 0.20)
          ("*compilation*"                 :align below :size 10)
          ("*grep*"                        :align below :size 10)
          ("*Occur*"                       :align below :size 10)
          ("*Go Test*"                     :align below :size 15)
          ("*Gofmt Errors*"                :align below :size 6)
          ("*shell-1*"                     :same t      :align below)
          (ivy-occur-grep-mode             :align below :size 10))))

(use-package smex
  :ensure t
  :after counsel)

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

(use-package intero
  :ensure t
  :commands intero-mode)

(use-package! whitespace
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
  :ensure t
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
  :defer .5
  :config
  (setq show-paren-style 'parenthesis
        show-paren-delay 0)
  (show-paren-mode 1))

(use-package ledger-mode
  :ensure t
  :commands ledger-mode)

(use-package uniquify
  :disabled t
  :defer .5
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
  :commands json-pretty-print
  :init
  (ex! "json" 'evgeni-json-pretty-print-dwim)
  (defun evgeni-json-pretty-print-dwim ()
    (interactive)
    (if (region-active-p)
        (call-interactively 'json-pretty-print)
      (json-pretty-print-buffer)
      (json-mode)))
  :config
  (setq json-encoding-object-sort-predicate 'string<))

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
  :defer 3
  :config
  (setq docker-tramp-use-names t))

(use-package dockerfile-mode
  :ensure t
  :mode (".*Dockerfile.*" . dockerfile-mode)
  :config
  (modify-syntax-entry ?$ "." dockerfile-mode-syntax-table))

(use-package restclient
  :ensure t
  :mode ("\\.restclient\\'" . restclient-mode))

(use-package company-restclient
  :ensure t
  :after (company restclient))

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
  :if (display-graphic-p)
  :defer .5
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (add-to-list 'exec-path-from-shell-variables "GOPATH" t)
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

(use-package hl-todo
  :ensure t
  :defer 3
  :config
  (global-hl-todo-mode))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

(use-package diff
  :defer t
  :config
  (add-hook 'diff-mode-hook 'diff-auto-refine-mode))

(use-package! diff-hl
  :ensure t
  :defer 3
  :config
  (global-diff-hl-mode)
  (unless (window-system)
    (diff-hl-margin-mode))

  (setq diff-hl-margin-symbols-alist '((insert . "+") (delete . "-") (change . "|") (unknown . "?") (ignored . "i")))

  (add-hook 'diff-hl-mode-hook 'diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

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

(use-package nameless
  :ensure t
  :defer t)

(use-package git-link
  :ensure t
  :defer t
  :init
  (ex! "gitlink" 'git-link)
  (ex! "gitlink-commit" 'git-link-commit)
  :config
  (setq git-link-open-in-browser (display-graphic-p))

  (add-to-list 'git-link-remote-alist '("uber.internal" git-link-code-uber-com))
  (defun git-link-code-uber-com (hostname dirname filename branch commit start end)
    (format "https://sourcegraph.uberinternal.com/%s/%s@%s/-/blob/%s"
            (replace-regexp-in-string "config.uber.internal" "code.uber.internal" hostname)
            (replace-regexp-in-string "@" "-" dirname) ;; replace "@" with "-"
            (or branch commit)
            (concat filename
                    (when start
                      (concat "#"
                              (if end
                                  (format "L%s-%s" start end)
                                (format "L%s" start))))))))

(use-package! shell-pop
  :ensure t
  :commands shell-pop
  :init
  (evil-define-key '(normal insert) evgeni-intercept-mode-map
    (kbd "C-c C-z") 'shell-pop)
  (setq shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))
  :config
  (setq shell-pop-full-span t))

(use-package git-commit
  :ensure t
  :defer t
  :config
  (use-package git-commit-insert-issue
    :ensure t
    :config

    (use-package gitlab
      :ensure t
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

(use-package! hydra
  :ensure t
  :defer t
  :functions defhydra
  :init
  (ex! "font-size" 'evgeni-hydra-zoom/body)

  ;; "c o" for toggles
  (evil-define-key 'operator global-map
    "o" '(menu-item
          ""
          nil
          :filter (lambda (&optional _)
                    (when (eq evil-this-operator 'evil-change)
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

  ;; use linum-mode if display-line-numbers-mode isn't available
  (unless (fboundp 'display-line-numbers-mode)
    (defalias 'display-line-numbers-mode 'linum-mode)
    (use-package linum
      :config
      (setq linum-format "%d ")))

  (defun evgeni-toggle-flycheck-flymake-mode ()
    (interactive)
    (cond
     ((bound-and-true-p flymake-mode) (call-interactively 'flymake-mode))
     ((bound-and-true-p flycheck-mode) (call-interactively 'flycheck-mode))
     ((bound-and-true-p eglot--managed-mode) (call-interactively 'flymake-mode))
     ((bound-and-true-p lsp-mode) (call-interactively (if lsp-prefer-flymake 'flymake-mode 'flycheck-mode)))
     (t (call-interactively 'flymake-mode))))

  (defhydra evgeni-toggles-hydra (:exit t)
    "toggles"
    ("c" global-hl-line-mode "global-hl-line-mode")
    ("w" visual-line-mode "visual-line-mode")
    ("k" toggle-input-method "toggle-input-method")
    ("n" display-line-numbers-mode "display-line-numbers-mode")
    ("s" (progn
           (call-interactively 'flyspell-mode)
           (when flyspell-mode
             (flyspell-buffer))) "flyspell-mode")
    ("m" evgeni-toggle-flycheck-flymake-mode "flycheck/flymake")
    ("r" linum-relative-toggle "display-line-numbers-mode")
    ("h" auto-fill-mode "auto-fill-mode")))

(use-package linum-relative
  :ensure t
  :defer t)

(use-package! goto-chg
  :bind (:map evil-normal-state-map ("g ;" . hydra-goto-last-change/goto-last-change))
  :config
  (defhydra hydra-goto-last-change ()
    "Change List"
    (";" goto-last-change "undo")
    ("." goto-last-change-reverse "redo")))

(use-package clojure-mode
  :ensure t
  :defer t
  :config

  (define-key clojure-mode-map (kbd "C-c <") 'sp-forward-slurp-sexp)
  (define-key clojure-mode-map (kbd "C-c >") 'sp-forward-barf-sexp)

  (modify-syntax-entry ?> "w" clojure-mode-syntax-table)

  (use-package cider
    :ensure t
    :config
    (setq cider-repl-display-help-banner nil)
    (evil-define-key 'insert cider-repl-mode-map
      (kbd "C-p") 'cider-repl-previous-input
      (kbd "C-n") 'cider-repl-next-input
      (kbd "TAB") #'cider-repl-tab)))

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

(use-package ox-hugo
  :ensure t
  :defer t)

(use-package! comint
  :defer t
  :config
  (evil-define-key 'insert comint-mode-map
    (kbd "TAB") #'company-complete-common-or-cycle
    (kbd "C-p") #'comint-previous-matching-input-from-input
    (kbd "C-n") #'comint-next-matching-input-from-input))

(use-package! eshell
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

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :ensure t)

(use-package! evil-numbers
  :ensure t
  :bind (:map evil-normal-state-map
              ("C-a" . evil-numbers/inc-at-pt)
              ("g C-a" . evil-numbers/dec-at-pt)))

(use-package php-mode
  :ensure t
  :defer t)

(use-package cperl-mode
  :init
  (defalias 'perl-mode 'cperl-mode)
  :mode (("\\.pl$"   . perl-mode)
         ("\\.pm$"   . perl-mode)
         ("\\.psgi$" . perl-mode)
         ("\\.t$"    . perl-mode)
         ("\\.html$" . perl-mode)

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
  :defer .1
  :ensure t
  :config
  (setq minions-mode-line-lighter "-")
  (setq minions-direct '(flycheck-mode flymake-mode))
  (minions-mode))

(use-package moody
  :ensure t
  :if (display-graphic-p)
  :config
  (when (eq window-system 'ns)
    (setq moody-slant-function #'moody-slant-apple-rgb))

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
  :ensure t
  :defer 3
  :config
  (setq beacon-size 80)
  (defun evgeni-set-beacon-color (&rest _)
    (setq beacon-color (face-attribute 'region :background)))

  (evgeni-set-beacon-color)

  (advice-add 'load-theme :after 'evgeni-set-beacon-color)

  (beacon-mode))

(use-package dired-sidebar
  :ensure t
  :commands dired-sidebar-toggle-sidebar
  :init
  (ex! "sidebar" 'dired-sidebar-toggle-sidebar)
  (ex! "tree" 'dired-sidebar-toggle-sidebar))

(use-package go-mode
  :defer t
  :config

  (setq gofmt-command "goimports")

  (reformatter-define evgeni-gofmt
    :program "goimports")

  (add-hook 'go-mode-hook 'evgeni-go-mode-hook)
  (defun evgeni-go-mode-hook ()
    (evgeni-gofmt-on-save-mode)
    (hs-minor-mode)
    (evgeni-hs-fold-on-first-occurance "^import (")
    ;; compile command
    (set (make-local-variable 'compile-command)
         "go build -o /dev/null"))

  (evil-define-key 'normal go-mode-map
    (kbd "gm") (lambda () (interactive) (compile "go build -o /dev/null"))
    (kbd "gM") (lambda () (interactive) (compile "go test")))

  ;; TODO will this become unnecessary with go modules?
  (defun evgeni-go-strip-vendor-prefix (orig-fun &rest args)
    (mapcar
     (lambda (str) (replace-regexp-in-string "^.*/vendor/" "" str))
     (apply orig-fun args)))
  (advice-add 'go-packages-native :around #'evgeni-go-strip-vendor-prefix)

  (defun evgeni-go-dump ()
    (interactive)
    (let ((word (thing-at-point 'word)))
      (save-excursion (end-of-line)
                      (newline)
                      (indent-according-to-mode)
                      (insert (concat "fmt.Fprintf(os.Stderr, \"" word ": %v\\n\", litter.Options{HidePrivateFields: false}.Sdump(" word "))")))))

  (with-eval-after-load 'evil
    (evil-define-key 'normal go-mode-map (kbd "] d") 'evgeni-go-dump))

  (set (make-local-variable 'outline-regexp) "func\\|import\\|type\\|const"))

(use-package eglot
  :ensure t
  :defer t
  :init
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  :custom
  (eglot-sync-connect nil)
  :config
  (add-to-list 'eglot-ignored-server-capabilites ':documentHighlightProvider))

(use-package lsp-mode
  :ensure t
  :disabled t
  :defer t
  ;; :hook ((python-mode . lsp))
  :init
  (setq lsp-auto-guess-root t
        lsp-prefer-flymake nil)
  :config

  (setq lsp-eldoc-hook '(lsp-hover)) ;; remove documentHighlightProvider (lsp-document-highlight)
  (setq lsp-eldoc-prefer-signature-help nil)

  (use-package lsp-ui
    :ensure t
    :init
    (setq lsp-ui-peek-enable nil
          lsp-ui-sideline-enable nil
          lsp-ui-doc-enable nil
          lsp-ui-imenu-enable nil))

  (add-hook 'lsp-after-open-hook 'evgeni-lsp-after-open-hook)
  (defun evgeni-lsp-after-open-hook ()
    (flycheck-mode -1) ;; TODO this isn't working
    (setq company-backends '(company-lsp))))

(use-package company-lsp
  :ensure t
  :after lsp-mode)

(use-package! evil-multiedit
  :ensure t
  :commands evgeni-evil-multiedit
  :init
  (ex! "iedit" 'evgeni-evil-multiedit)
  :config
  (evil-define-command evgeni-evil-multiedit (&optional beg end regexp)
    :motion nil
    :move-point nil
    :repeat nil
    (interactive "<r><a>")
    (evil-multiedit-abort)
    (evil-multiedit--start-regexp (or regexp (evil-ex-pattern-regex evil-ex-search-pattern)) beg end)))

(use-package! rainbow-mode
  :ensure t
  :commands rainbow-mode
  :config
  (setq rainbow-r-colors-alast nil))

(use-package company-anaconda
  :ensure t
  :after anaconda-mode
  :config
  (add-to-list 'company-backends 'company-anaconda))

;; (use-package! gotest
;;   :ensure t
;;   :after go-mode
;;   :config
;;   (evil-define-key 'normal go-mode-map (kbd "g m") 'go-test-current-file)

;;   (add-hook 'go-mode-hook (lambda ()
;;                             (set (make-local-variable 'compilation-error-regexp-alist-alist)
;;                                  go-test-compilation-error-regexp-alist-alist)
;;                             (set (make-local-variable 'compilation-error-regexp-alist)
;;                                  go-test-compilation-error-regexp-alist))))

(use-package with-editor
  :ensure t
  :defer t
  :config
  (add-hook 'eshell-mode-hook 'with-editor-export-editor))

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

(use-package thrift
  :ensure t
  :defer t)

(use-package json-mode
  :ensure t
  :defer t)

(use-package deadgrep
  :ensure t
  :defer t
  :init
  (ex! "deadgrep" 'deadgrep))

(use-package replace
  :defer
  :init
  (ex! "occur" 'evgeni-occur)
  (evil-define-command evgeni-occur (count)
    (interactive "<c>")
    (message "--- %s" count)
    (setq list-matching-lines-default-context-lines (or count 0))
    (occur (evil-ex-pattern-regex evil-ex-search-pattern))
    (select-window (display-buffer "*Occur*")))

  (defun evgeni-occur-increase-context()
    (interactive)
    (unless (eq major-mode 'occur-mode)
      (user-error "Buffer is not in be occur-mode"))
    (setq list-matching-lines-default-context-lines
          (1+ (or list-matching-lines-default-context-lines 0)))
    (revert-buffer))

  (defun evgeni-occur-decrease-context()
    (interactive)
    (unless (eq major-mode 'occur-mode)
      (user-error "Buffer is not in be occur-mode"))
    (when (<= list-matching-lines-default-context-lines 0)
      (user-error "Can't go below 0"))
    (setq list-matching-lines-default-context-lines
          (1- (or list-matching-lines-default-context-lines 0)))
    (revert-buffer))

  (evil-define-key 'normal occur-mode-map
    "+" 'evgeni-occur-increase-context
    "-" 'evgeni-occur-decrease-context))

(use-package direnv
  :ensure t
  :defer 3
  :config
  (direnv-mode))

(use-package autoinsert
  :defer 3
  :init
  (add-hook 'find-file-hook 'auto-insert)
  :config

  (setq auto-insert-query nil)

  (require 'yasnippet)
  (defun evgeni-autoinsert-yas-expand ()
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))

  (setq auto-insert-alist
        '((sh-mode . [ "template.sh" evgeni-autoinsert-yas-expand])
          (go-mode . [ "template.go" evgeni-autoinsert-yas-expand ]))))

(use-package make-mode
  :defer t
  :config
  (modify-syntax-entry ?- "w" makefile-mode-syntax-table))

(use-package prodigy
  :ensure t
  :defer t
  :init
  (ex! "prodigy" 'prodigy)
  (ex! "services" 'prodigy)
  :config
  (setq prodigy-kill-process-buffer-on-stop t
        prodigy-completion-system 'default)

  (evil-define-key 'normal prodigy-mode-map
    "$" 'prodigy-display-process
    "gr" 'prodigy-refresh))

(use-package! hideshow
  :defer t
  :config

  (evil-define-minor-mode-key 'normal 'hs-minor-mode [tab] 'evil-toggle-fold)

  (defun evgeni-hs-fold-on-first-occurance (regex)
    (hs-life-goes-on
    (save-excursion
      (goto-char (point-min))
      (when (ignore-errors (re-search-forward regex))
        (hs-hide-block))))))

(use-package disable-mouse
  :ensure t
  :defer 3
  :config
  ;; no mouse in evil insert state
  (disable-mouse-in-keymap evil-insert-state-map))

(use-package outline
  :defer t
  :config
  (define-key outline-minor-mode-map (kbd "TAB")
    '(menu-item "" nil :filter
                (lambda
                  (&optional _)
                  (when (outline-on-heading-p)
                    'outline-cycle)))))

(use-package protobuf-mode
  :ensure t
  :defer t
  :config
  (add-hook 'protobuf-mode-hook 'electric-pair-local-mode))

(use-package reformatter
  :ensure t
  :defer t)

(use-package focus
  :ensure t
  :defer t
  :init
  (ex! "focus" 'focus-mode))

(use-package fancy-narrow
  :ensure t
  :defer t)

(use-package total-lines
  :ensure t
  :defer 3
  :config
  (global-total-lines-mode)
  (setq mode-line-position '(("L%l/" (:eval (format "%d "total-lines))))))

(use-package so-long
  :load-path "~/dev/so-long"
  :defer 3
  :config
  (add-to-list 'so-long-target-modes 'yaml-mode)
  (add-to-list 'so-long-target-modes 'json-mode)
  (global-so-long-mode 1))

(use-package vterm
  :ensure t
  :defer t)

(use-package undo-fu
  :ensure t
  :defer .5
  :config
  (global-undo-tree-mode -1)
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)

  (use-package undo-fu-session
    :ensure t
    :config
    (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
    (global-undo-fu-session-mode)))

(use-package gcmh
  :ensure t
  :defer .5
  :config
  (gcmh-mode))
