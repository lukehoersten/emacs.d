;; ~/.emacs.d/elisp/package-require.el
;; Luke Hoersten <Luke@Hoersten.org>

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)
(package-refresh-contents)

(defun package-require (packages)
  "Ensure that a given package is installed"
  (mapc (lambda (package)
          (unless (package-installed-p package)
            (package-install package)
            (message "Installing %s package...done" package)))
        packages))

(message "Loading packages...done")
(provide 'package-require)
