;; ~/.emacs.d/elisp/ansible-init.el
;; Luke Hoersten <Luke@Hoersten.org>

(dolist (package '(yaml-mode jinja2-mode company-ansible ansible-doc))
  (unless (package-installed-p package)
    (package-install package)))

(add-hook 'yaml-mode-hook #'ansible-doc-mode)

(message "Loading ansible-init...done")
(provide 'ansible-init)
