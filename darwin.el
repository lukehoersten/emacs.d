;; ~/.emacs.d/darwin.el
;; Luke Hoersten <Luke@Hoersten.org>

;;(setq-default ns-command-modifier)
(if window-system
    (progn
      (menu-bar-mode t)
      (set-default-font "Menlo-12")
      ))

(provide 'darwin)