
;; (package-initialize)

(setq gc-cons-threshold (* 10 1024 1024))

(let ((gc-cons-threshold (* 256 1024 1024))
      (file-name-handler-alist nil))

  (load (expand-file-name "package-utils.el" user-emacs-directory))

  ;; settings
  (setq custom-file (expand-file-name "custom-file.el" user-emacs-directory))
  (setq inhibit-startup-screen t)
  (setq make-backup-files nil)
  (save-place-mode 1) 
  (show-paren-mode 1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)

  (setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name))

  ;; packages

  (setq evil-search-module 'evil-search)

  (install-packages '(
		      ;; evil packages
		      evil
		      evil-commentary
		      evil-indent-plus

		      ;; non-evil
		      avy
		      magit
		      ))

  (evil-mode)
  (evil-commentary-mode)
  (evil-indent-plus-default-bindings)

  ;; key bindings
  (global-set-key "\C-ch" help-map)

  (defalias 'after 'with-eval-after-load)

  (defun nmap (lhs rhs) (define-key evil-normal-state-map (kbd lhs) rhs))
  (defun imap (lhs rhs) (define-key evil-insert-state-map (kbd lhs) rhs))

  (after "evil"
    (nmap "C-w C-w" 'ace-window)
    (nmap "-" (lambda () (interactive) (dired ".")))
    (nmap "C-c C-c" 'avy-goto-word-or-subword-1)
    (nmap "U U" 'magit-status)
    (imap "TAB" 'hippie-expand)

    (after "dired"
      (define-key dired-mode-map (kbd "-") 'dired-up-directory)
      ))


  ;; perl
  (add-hook 'cperl-mode-hook (progn
			       (nmap "C-p" 'perl-beginning-of-function)
			       (nmap "C-n" 'perl-end-of-function)
			       ))
  )

