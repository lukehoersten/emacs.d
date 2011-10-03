;; ~/.emacs.d/color-theme-init.el
;; Luke Hoersten <Luke@Hoersten.org>

;;; color theme
(if window-system
   (progn
     (require 'color-theme)

     ;; solarized
     (add-to-list 'load-path "~/.emacs.d/thirdparty/color-theme-solarized")
     (require 'color-theme-solarized)
     (color-theme-solarized-light)
     ;; (color-theme-solarized-dark)

     ;; twilight
     ;; (load "color-theme-twilight")
     ;; (color-theme-twilight)
     ))

(message "Loading color-theme-init...done")
(provide 'color-theme-init)
