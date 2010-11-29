;; ~/.emacs.d/darwin.el
;; Luke Hoersten <Luke@Hoersten.org>

;; Keyboard layout: caps=command, command=alt
(setq-default ns-command-modifier 'control)

(if window-system
    (progn
      (tabbar-mode nil)
      (menu-bar-mode t)
      (set-default-font "Menlo-12")
      ))

(message "Loaded Darwin settings")
(provide 'darwin)
