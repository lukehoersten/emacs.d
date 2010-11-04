;; ~/.emacs.d/linux.el
;; Luke Hoersten <Luke@Hoersten.org>

(if window-system
    (progn
      (set-default-font "Monospace-10")

      ;; twilight theme
      (require 'color-theme)
      (load "color-theme-twilight")
      (color-theme-twilight)
      ))

(provide 'linux)