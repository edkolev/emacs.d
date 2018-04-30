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
(setq-default sentence-end-double-space nil)

;; disable VC
(setq vc-handled-backends '(Git))

(modify-syntax-entry ?_ "w" (standard-syntax-table))
(modify-syntax-entry ?' "." (standard-syntax-table))
(modify-syntax-entry ?' "." text-mode-syntax-table)
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

(defmacro jump! (fun)
  "Register the given function as an evil jump"
  (let ((advice (intern (concat "evgeni-evil-jump-advice--" (symbol-name fun)))))
    `(progn
       (defun ,advice (&rest args)
         (evil-set-jump))

       (advice-add (quote ,fun) :before (quote ,advice)))))

(defmacro use-package! (name &rest plist)
  (declare (indent 1))
  `(with-eval-after-load 'evil
     (use-package ,name ,@plist)))

;; imenu should recognize use-package!
(with-eval-after-load 'lisp-mode
  (add-to-list 'lisp-imenu-generic-expression
               (list "Packages" (concat (concat "^\\s-*(" (regexp-opt '("use-package!") t) "\\s-+\\(")
                                        (or (bound-and-true-p lisp-mode-symbol-regexp)
                                            "\\(?:\\sw\\|\\s_\\|\\\\.\\)+") "\\)") 2)))




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
(use-package twilight-bright-theme :ensure t :defer t
  :config
  (require 'color)
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     ;; `(company-tooltip ((t (:inherit default :background ,(color-darken-name bg 5)))))
     ;; `(company-scrollbar-bg ((t (:background ,(color-darken-name bg 10)))))
     ;; `(company-scrollbar-fg ((t (:background ,(color-darken-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face)))))))
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

(use-package emacs
  :defer .15
  :config
  ;; load theme
  (load-theme (if (display-graphic-p)
                  'twilight-bright ;; 'apropospriate-dark ;;'tango-dark ;; tango-plus flatui
                'twilight-bright
                )
              t)
  ;; bind command-option-H to hide other windows, just like every other OS X app
  (when (string-equal system-type "darwin")
    (define-key global-map (kbd "M-s-˙") 'ns-do-hide-others)))

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

;; load mail.el if any
(when (file-readable-p (concat user-emacs-directory "mail.el"))
  (load-file (concat user-emacs-directory "mail.el")))

;; workaround for OSX https://emacs.stackexchange.com/questions/18045/how-can-i-retrieve-an-https-url-on-mac-os-x-without-warnings-about-an-untrusted
;; the .pem file is installed with brew install libresll
(use-package gnutls
  :defer t
  :if (file-readable-p "/usr/local/etc/libressl/cert.pem")
  :config
  (add-to-list
   'gnutls-trustfiles "/usr/local/etc/libressl/cert.pem"))

;; load evil early
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil)
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
  (message "Loading evil-mode...done (%.3fs)" (float-time (time-subtract (current-time) emacs-start-time))))

;; configure evil after startup
(use-package evil
  :defer .1
  :config

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
    :motion nil
    :move-point nil
    :repeat nil
    (interactive)
    (if (or buffer-file-name (buffer-base-buffer))
        (call-interactively 'save-buffer)
      (user-error "No file")))

  (defun evgeni-window-vsplit ()
    (interactive)
    (split-window-horizontally)
    (windmove-right)
    (balance-windows))

  (define-key evil-normal-state-map (kbd "RET") 'evgeni-save-file)
  (define-key evil-normal-state-map (kbd "[ Q") 'first-error)
  (define-key evil-normal-state-map (kbd "] Q") (lambda () (interactive) (goto-char (point-max)) (previous-error)))
  (define-key evil-normal-state-map (kbd "] q") 'next-error)
  (define-key evil-normal-state-map (kbd "[ q") 'previous-error)
  (define-key evil-normal-state-map (kbd ", w") 'evgeni-window-vsplit)

  (define-key evil-normal-state-map (kbd "C-c C-b")'ido-switch-buffer)

  (define-key evil-normal-state-map (kbd "] SPC")(lambda (count) (interactive "p") (dotimes (_ count) (save-excursion (evil-insert-newline-below)))))
  (define-key evil-normal-state-map (kbd "[ SPC")(lambda (count) (interactive "p") (dotimes (_ count) (save-excursion (evil-insert-newline-above)))))

  (define-key evil-normal-state-map (kbd "[ m")'beginning-of-defun)
  (define-key evil-normal-state-map (kbd "] m")'end-of-defun)
  (jump! beginning-of-defun)
  (jump! end-of-defun)
  (define-key evil-normal-state-map (kbd "] M")'end-of-defun)
  (define-key evil-motion-state-map (kbd "0") 'evil-first-non-blank)
  (define-key evil-normal-state-map (kbd "g n") (lambda () (interactive) (evil-ex "%normal ")))
  (define-key evil-visual-state-map (kbd "g n") (lambda () (interactive) (evil-ex "'<,'>normal ")))
  (define-key evil-ex-search-keymap "\C-w" #'evil-delete-backward-word)

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
  ;; run `occur' with the evil's search pattern
  (define-key evil-normal-state-map (kbd ", *" )
    (lambda ()
      (interactive)
      (occur (evil-ex-pattern-regex evil-ex-search-pattern))
      (pop-to-buffer-same-window "*Occur*")))

  (defvar evgeni-conflict-marker-regex "^[<=>|]\\{7\\}")

  ;; :source
  (evil-ex-define-cmd "so[urce]" 'evgeni-source)
  (evil-define-command evgeni-source (&optional file)
    (interactive "<f>")
    (if file
        (load-file file)
      (load-file (concat user-emacs-directory "init.el"))))

  (define-key evil-normal-state-map "Y" (lambda () (interactive) (evil-yank (point) (point-at-eol))))

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
  (define-key evil-insert-state-map (kbd "C-x C-x") 'completion-at-point)
  (define-key evil-insert-state-map (kbd "C-k") 'completion-at-point)
  (define-key evil-insert-state-map (kbd "C-x C-]") 'complete-tag)
  (define-key evil-insert-state-map (kbd "C-]") 'complete-tag)

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

  ;; ;; toggles
  ;; (define-key evil-normal-state-map (kbd "C-c o c") 'hl-line-mode)
  ;; (define-key evil-normal-state-map (kbd "C-c o w") 'visual-line-mode)
  ;; (define-key evil-normal-state-map (kbd "C-c o k") 'toggle-input-method)
  ;; (define-key evil-normal-state-map (kbd "C-c o h") 'auto-fill-mode)

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
    :repeat abort
    (interactive "<r>")
    (evil-exit-visual-state)
    (evil-ex-normal beg end "."))
  (define-key evil-visual-state-map (kbd ".") 'evgeni-visual-dot-repeat)

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

  ;; start %s/.../... with the word at point
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
    (define-key evil-visual-state-map (kbd "s-c") (kbd "\"+y"))
    (define-key evil-insert-state-map  (kbd "s-v") (kbd "+"))
    (define-key evil-ex-completion-map (kbd "s-v") (kbd "+"))
    (define-key evil-ex-search-keymap  (kbd "s-v") (kbd "+")))

  ;; don't move point on :s// and :g//
  (defun evgeni-save-excursion-advice (orig-fun &rest args)
    (save-excursion (apply orig-fun args)))
  ;; (advice-add 'evil-ex-substitute :around #'evgeni-save-excursion-advice)
  (advice-add 'evil-ex-global :around #'evgeni-save-excursion-advice)

  (require 'evil-development)

  ;; other packages which integrate with evil
  (use-package evil-collection
    :ensure t
    :init
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

  (use-package evil-indent-plus
    :ensure t
    :defer t
    :init
    (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
    (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
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
          (move-text-line-up))))))

(use-package! avy
  :ensure t
  :defer 3
  :bind (:map evil-normal-state-map
              ("gj" . avy-goto-line-below)
              ("gk" . avy-goto-line-above)
              :map evil-motion-state-map
              ("gh" . avy-goto-char-timer))
  :config
  (setq avy-timeout-seconds 1.2)
  (custom-set-faces
   '(avy-lead-face-0 ((t (:inherit 'highlight))))
   '(avy-lead-face ((t (:inherit 'highlight)))))

  (setq avy-background t)
  (setq avy-keys '(?a ?s ?d ;; ?f
                      ?g ;; ?h
                      ?j ?k
                      ?l ?\;
                      ?1 ?2 ?3 ?4 ?5 ?6))
  (setq avy-style 'de-bruijn) ;; at-full

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
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)

  (defun evgeni-dired-current-dir ()
    (interactive)
    (dired ".")))

(use-package! magit
  :ensure t
  :defer 30
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
              ("U m" . magit-merge-popup)
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

  ;; lower untracked
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-untracked-files 'magit-insert-staged-changes 1)

  ;; lower stashes
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-stashes 'magit-insert-unpushed-to-pushremote 1)

  (define-key magit-dispatch-popup-map "/" 'isearch-forward)
  (define-key magit-dispatch-popup-map "?" 'isearch-backward)

  (add-hook 'git-commit-mode-hook 'flyspell-mode))

(use-package! smerge-mode
  :defer t
  :config
  (evil-define-minor-mode-key 'normal 'smerge-mode
    (kbd "] n")     'smerge-next
    (kbd "[ n")     'smerge-prev
    (kbd "C-c C-c") 'smerge-keep-current
    (kbd "C-c C-k") 'smerge-kill-current
    ;; "b" mnemonic for both
    (kbd "C-c C-b") 'smerge-keep-all))

(use-package! vdiff
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

  (evil-define-minor-mode-key 'normal 'vdiff-mode "q" 'evgeni-vdiff-close-if-readonly)

  :config

  (add-hook 'vdiff-mode-hook (lambda () (diff-hl-mode -1)))
  ;; (add-hook 'vdiff-mode-off-hook (lambda () (diff-hl-mode +1)))

  (setq vdiff-auto-refine t)
  (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)

  (defun evgeni-vdiff-close-if-readonly ()
    (interactive)
    (if buffer-read-only
        (vdiff-quit)
      (call-interactively 'evil-record-macro))))

(use-package! vdiff-magit
  :ensure t
  :after magit
  :init
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

(use-package! org
  :defer 60
  :ensure t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages '((shell . t)))

  (evil-ex-define-cmd "cap[ture]" 'org-capture)

  (setq org-startup-indented t)
  (setq org-startup-folded "showall")
  (setq org-confirm-babel-evaluate nil)

  (evil-define-key '(normal insert) org-mode-map
    (kbd "TAB") 'org-cycle)

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

(use-package! xref
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
  (evil-define-key 'normal xref--xref-buffer-mode-map "q" 'delete-window)
  ;; don't use swiper in this buffer - swiper clears the highlighting in the xref buffer
  (evil-define-key 'normal xref--xref-buffer-mode-map "/" 'evil-ex-search-forward))

(use-package! smart-compile
  :ensure t
  :commands smart-compile
  :bind (:map evil-normal-state-map
              ("g m" . evgeni-smart-compile)
              ("g RET" . evgeni-smart-compile))
  :config
  (evil-define-command evgeni-smart-compile ()
    :motion nil
    :move-point nil
    :repeat nil
    (interactive)
    (call-interactively 'smart-compile))

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

  (ex! "mak[e]" 'evgeni-smart-compile))

(use-package! ivy
  :ensure t
  :defer t
  :bind (:map evil-normal-state-map
              ("SPC" . ivy-switch-buffer))
  :init
  (evil-define-key 'normal evgeni-intercept-mode-map
    (kbd "SPC") 'ivy-switch-buffer)
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
              ("C-c C-r" . ivy-resume)
              ("/" . swiper)
              :map evil-normal-state-map
              (", /" . swiper-all))
  :config
  (setq swiper-goto-start-of-match t)
  (jump! swiper))

(use-package! counsel
  :ensure t
  :init
  (ex! "faces" 'counsel-faces)
  (ex! "find-lib" 'counsel-find-library)
  :bind (([remap find-file] . counsel-find-file)
         ([remap execute-extended-command] . counsel-M-x)
         ([remap describe-function] . counsel-describe-function)
         ([remap describe-key] . describe-key)
         ([remap describe-variable] . counsel-describe-variable)
         :map evil-motion-state-map
         ("g /"   . counsel-git-grep)
         :map evil-normal-state-map
         (", f"   . counsel-imenu)
         ("K"     . evgeni-counsel-git-grep)
         ("g P" . counsel-yank-pop)
         ("C-c C-z" . evgeni-shell)
         :map evil-visual-state-map
         ("K"     . evgeni-counsel-git-grep))
  :config

  (defun evgeni-shell ()
    "Switch to a shell buffer, or create one."
    (interactive)
    (ivy-read "Switch to shell buffer: "
              (counsel-list-buffers-with-mode 'shell-mode)
              :action #'counsel-switch-to-buffer-or-window
              :initial-input (concat (expand-file-name (or (cdr (project-current)) default-directory)) " (shell)")
              :caller 'counsel-switch-to-shell-buffer))

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

  (jump! counsel-imenu)
  (jump! counsel-find-file)
  (jump! counsel-git-grep)
  (jump! evgeni-counsel-git-grep))

(use-package! imenu-anywhere
  :ensure t
  :bind (:map evil-normal-state-map
              (", F"  . imenu-anywhere))
  :config
  (jump! imenu-anywhere))

(use-package! winner
  :defer 2
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
  :bind (:map evil-motion-state-map
              ("g SPC"   . evgeni-projectile-find-file))
  :init
  (ex! "proj[ectile]" 'projectile-switch-project)
  :config
  (require 'counsel)
  (defun evgeni-projectile-find-file ()
    (interactive)
    (if (projectile-project-p)
        (if (eq (projectile-project-vcs) 'git)
            (counsel-git)
          (find-file-in-project))
      (projectile-switch-project)))

  (setq projectile-switch-project-action
        (lambda ()
          (dired (projectile-project-root))))
  (setq projectile-mode-line (format " [%s]" (projectile-project-name)))
  (setq projectile-completion-system 'default)
  (projectile-global-mode))

(use-package! company
  :ensure t
  :config
  (setq company-idle-delay nil)
  (setq company-dabbrev-downcase nil)
  (evil-global-set-key 'insert (kbd "TAB") 'company-complete-common-or-cycle)
  (global-company-mode))

(use-package! yasnippet
  :ensure t
  :disabled t
  :config
  (use-package! yasnippet-snippets
    :ensure t))
(use-package evil-expat
  :load-path "~/dev/evil-expat"
  :defer 1)

(use-package evil-goggles
  :defer 1
  :ensure t
  :load-path "~/dev/evil-goggles"
  :config
  (setq evil-goggles-blocking-duration 0.100)
  ;; (setq evil-goggles-async-duration 0.200)

  (evil-goggles-mode)
  (evil-goggles-use-diff-faces)
  ;; (evil-goggles-use-magit-faces)
  )

(use-package! evil-regression
  :load-path "~/dev/evil-regression"
  :disabled t
  :bind (:map evil-insert-state-map
              ( "C-r" . evil-regression-paste-from-register)))

(use-package recentf
  :defer 1
  :init
  (setq recentf-max-saved-items 500)
  (setq recentf-exclude '("/tmp/" "/ssh:" (expand-file-name "~/Maildir/")))
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
  :after dired
  :config
  (evil-define-key 'normal dired-mode-map (kbd "RET") 'dired-single-buffer)
  (evil-define-key 'normal dired-mode-map "-" (lambda nil (interactive) (dired-single-buffer ".."))))

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
          ("*Occur*"                   :align below :size 10)
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
  :defer t
  :config
  ;; (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (ex! "undo-tree" 'undo-tree-visualize)
  (setq undo-tree-auto-save-history t)

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
  :defer 1
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
  (ex! "only" 'zygospore-toggle-delete-other-windows)

  ;; alias :On and :o with :on
  (ex! "On" "on")
  (ex! "o" "on"))

(use-package hl-todo
  :ensure t
  :defer 3
  :config
  (add-hook 'prog-mode-hook 'hl-todo-mode)
  (add-hook 'text-mode-hook 'hl-todo-mode))

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
  (add-hook 'diff-hl-mode-hook 'diff-hl-flydiff-mode)

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
  :init
  (ex! "gitlink" 'git-link)
  (ex! "gitlink-commit" 'git-link-commit)
  :config
  (setq git-link-open-in-browser (display-graphic-p)))

(use-package shell-toggle
  :ensure t
  :disabled t
  :bind* ("C-c C-z" . shell-toggle)
  :init
  (setq shell-toggle-launch-shell 'shell-toggle-eshell))

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
    ("k" text-scale-increase "in")
    ("0" (text-scale-set 0) "reset"))

  (defun evgeni-toggles ()
    (interactive)
    (message "hi2")
    (setq evil-inhibit-operator t)
    (evgeni-toggles-hydra/body))

  ;; use linum-mode if display-line-numbers-mode isn't available
  (unless (fboundp 'display-line-numbers-mode)
    (defalias 'display-line-numbers-mode 'linum-mode)
    (use-package linum
      :config
      (setq linum-format "%d ")))

  (defhydra evgeni-toggles-hydra (:exit t)
    "toggles"
    ("c" global-hl-line-mode "global-hl-line-mode")
    ("w" visual-line-mode "visual-line-mode")
    ("k" toggle-input-method "toggle-input-method")
    ("n" display-line-numbers-mode "toggle-input-method")
    ("h" auto-fill-mode "auto-fill-mode")))


(use-package clojure-mode
  :ensure t
  :defer t
  :config

  (define-key clojure-mode-map (kbd "C-c <") 'sp-forward-slurp-sexp)
  (define-key clojure-mode-map (kbd "C-c >") 'sp-forward-barf-sexp)

  (modify-syntax-entry ?> "w" clojure-mode-syntax-table)

  (use-package monroe
    :ensure t
    :disabled t
    :init
    (ex! "monroe" 'monroe)
    :config
    (evil-define-minor-mode-key 'normal 'monroe-interaction-mode (kbd "C-]") 'monroe-jump)
    (add-hook 'clojure-mode-hook 'clojure-enable-monroe))

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

(use-package helpful
  :ensure t
  :disabled t
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-key] . helpful-key)
         ([remap describe-variable] . helpful-variable))
  :config
  (evil-define-key 'normal helpful-mode-map "q" 'quit-window))

(use-package ox-hugo
  :ensure t
  :after ox)

(use-package! comint
  :defer t
  :config
  (evil-define-key 'insert comint-mode-map
    (kbd "TAB") #'company-complete-common-or-cycle
    (kbd "C-p") #'comint-previous-matching-input-from-input
    (kbd "C-n") #'comint-next-matching-input-from-input))

(use-package! eshell
  :defer t
  :config
  (evil-define-key 'insert eshell-mode-map (kbd "C-p") 'eshell-previous-matching-input-from-input)
  (evil-define-key 'insert eshell-mode-map (kbd "C-n") 'eshell-next-matching-input-from-input))

(use-package atomic-chrome
  :ensure t
  :if (display-graphic-p)
  :defer 5
  :config
  (setq atomic-chrome-buffer-open-style 'frame)
  (setq atomic-chrome-default-major-mode 'markdown-mode)
  (atomic-chrome-start-server))
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
  (minions-mode))

(use-package moody
  :ensure t
  :if (display-graphic-p)
  :config
  (when (string-equal system-type "darwin")
    (setq moody-slant-function #'moody-slant-apple-rgb))
  (setq moody-mode-line-height 20)
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)

  (defun evgeni-moody-setup (&rest args)
    (let* ((line-active (face-attribute 'mode-line :foreground))
           (line-inactive (face-attribute 'mode-line-inactive :foreground)))

      (message "line-active: %S" line-active)

      ;; active
      (set-face-attribute 'mode-line          nil :overline   line-active)
      (set-face-attribute 'mode-line          nil :underline  line-active)

      ;; inactive
      (set-face-attribute 'mode-line-inactive nil :overline   line-inactive)
      (set-face-attribute 'mode-line-inactive nil :underline  line-inactive)

      ;; no :box supported
      (set-face-attribute 'mode-line          nil :box        nil)
      (set-face-attribute 'mode-line-inactive nil :box        nil)))
  ;; (evgeni-moody-setup)
  ;; (advice-add 'load-theme :after #'evgeni-moody-setup)
  )

(use-package narrow-reindent
  :ensure t
  :commands evgeni-narrow-or-widen
  :init
  (ex! "nar[row]" 'evgeni-narrow-or-widen)
  :config
  (evil-define-command evgeni-narrow-or-widen (bang begin end)
    (interactive "<!><r>")
    (cond
     ((region-active-p)
      (narrow-reindent-to-region begin end))
     ((buffer-narrowed-p)
      (if (buffer-base-buffer)
          (evil-delete-buffer (current-buffer)) ;; wipe out indirect buffer
        (narrow-reindent-widen)))
     (bang
      (switch-to-buffer (clone-indirect-buffer nil nil))
      (evgeni-narrow-or-widen nil begin end))
     (t
      (narrow-reindent-to-defun)))))

(use-package beacon
  :ensure t
  :config
  (setq beacon-color
        (face-attribute 'mode-line :background)))

