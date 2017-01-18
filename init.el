
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
  (defalias 'yes-or-no-p 'y-or-n-p)
  (require 'recentf)
  (recentf-mode t)
  (setq recentf-max-saved-items 50)

  (setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name))

  ;; packages
  (install-packages '(
		      ;; evil packages
		      evil
		      evil-commentary
		      evil-indent-plus
		      evil-surround
		      evil-exchange

		      ;; non-evil
		      avy
		      magit
		      flycheck
		      ))

  (setq-default flycheck-disabled-checkers '(perl-perlcritic))

  (setq evil-search-module 'evil-search)
  (evil-mode)
  (evil-commentary-mode)
  (evil-indent-plus-default-bindings)
  (global-evil-surround-mode)
  (setq magit-display-buffer-function
      #'magit-display-buffer-fullframe-status-v1)

  ;; utils

  (defun ido-recentf-open ()
      "Use `ido-completing-read' to \\[find-file] a recent file"
        (interactive)
          (if (find-file (ido-completing-read "Find recent file: " recentf-list))
                  (message "Opening file...")
                  (message "Aborting")))

  ;; key bindings
  (global-set-key "\C-ch" help-map)

  (defalias 'after 'with-eval-after-load)

  (defun nmap (lhs rhs) (define-key evil-normal-state-map (kbd lhs) rhs))
  (defun imap (lhs rhs) (define-key evil-insert-state-map (kbd lhs) rhs))

  (after "evil"
    (nmap "C-w C-w" 'ace-window)
    (nmap "-" (lambda () (interactive) (dired ".")))
    (nmap "C-c C-c" 'avy-goto-word-or-subword-1)
    (nmap "RET" 'save-buffer)
    (nmap "U U" 'magit-status)
    (nmap ", f" 'imenu)
    (nmap "g SPC" 'find-file-in-project)
    (nmap "SPC" 'ido-recentf-open)
    (nmap "C-c o c" 'hl-line-mode)

    (imap "TAB" 'hippie-expand)
    (imap "C-e" 'end-of-line)
    (imap "C-a" 'beginning-of-line-text)

    (nmap "[ Q" 'first-error)
    (nmap "] q" 'next-error)
    (nmap "[ q" 'previous-error)

    (after "dired"
      (define-key dired-mode-map (kbd "-") 'dired-up-directory)
      ))


  ;; perl
  (add-hook 'cperl-mode-hook (progn
			       (nmap "C-p" 'perl-beginning-of-function)
			       (nmap "C-n" 'perl-end-of-function)
			       ))
  )

