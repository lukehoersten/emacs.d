;; ~/.emacs.d/elisp/javascript-init.el
;; Luke Hoersten <Luke@Hoersten.org>

;; Require packages
(require 'package-require)
(package-require '(auto-complete ac-js2 js2-mode json-mode))

(require 'auto-complete-config)

;;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq-default ac-js2-evaluate-calls t)

(message "Loading javascript-init...done")
(provide 'javascript-init)
