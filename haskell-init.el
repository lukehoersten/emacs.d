;; ~/.emacs.d/haskell-init.el
;; Luke Hoersten <Luke@Hoersten.org>

;; scion
(if (file-exists-p "~/.cabal/share/scion-0.1.0.10/emacs")
    (progn
      (add-to-list 'load-path "~/.cabal/share/scion-0.1.0.10/emacs")
      (require 'scion)
      (setq scion-program "~/.cabal/bin/scion-server")
      (add-hook
       'haskell-mode-hook
       (lambda ()
         (scion-mode 1)
         (scion-flycheck-on-save 1)
         (setq scion-completing-read-function 'ido-completing-read)))))

(add-hook
 'haskell-mode-hook
 (lambda ()
   (haskell-indentation-mode nil)
   (haskell-indent-mode t)
   (capitalized-words-mode t)
   (haskell-doc-mode t)
   (setq
    haskell-indent-offset 4
    whitespace-line-column 78))
 t) ; append instead of prepend else haskell-mode overwrites these settings
(provide 'haskell-init)