;; ~/.emacs.d/elisp/ansible-init.el
;; Luke Hoersten <Luke@Hoersten.org>

;; Ensure packages are installed
(dolist (package '(jinja2-mode ansible-doc))
  (unless (package-installed-p package)
    (package-install package)))

(add-hook 'yaml-mode-hook #'ansible-doc-mode)

(message "Loading ansible-init...done")
(provide 'ansible-init)
