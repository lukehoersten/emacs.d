;; ~/.emacs.d/elisp/ansible-init.el
;; Luke Hoersten <Luke@Hoersten.org>

;; Require packages
(require 'package-require)
(package-require '(yaml-mode jinja2-mode company company-ansible ansible-doc))

(add-hook 'yaml-mode-hook #'ansible-doc-mode)

(message "Loading ansible-init...done")
(provide 'ansible-init)
