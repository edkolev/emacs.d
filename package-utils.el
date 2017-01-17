(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(defun install-packages (packages)
  "Install the given pacakges"
  (interactive)
  (mapc #'(lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
        packages))
