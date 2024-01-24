;;; early-init.el -*- lexical-binding: t; -*-

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

(setq package-enable-at-startup nil)

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
	  (expand-file-name  "var/eln-cache/" user-emacs-directory))))

(when (display-graphic-p)
  ;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
  (push '(menu-bar-lines . 1) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)

  ;; Resizing the Emacs frame can be a terribly expensive part of changing the
  ;; font. By inhibiting this, we easily halve startup times with fonts that are
  ;; larger than the system default.
  (setq frame-inhibit-implied-resize t)

  ;; Set default font
  (set-face-attribute 'default nil
                      :family "Fira Code"
                      :height 13
                      :weight 'normal ;; 'regular?
                      :width 'normal))
