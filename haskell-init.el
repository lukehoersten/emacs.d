;; ~/.emacs.d/haskell-init.el
;; Luke Hoersten <Luke@Hoersten.org>

(add-to-list 'load-path "~/.emacs.d/thirdparty/haskell-mode/")
(require 'haskell-mode-autoloads)

;; (add-to-list 'load-path "~/.emacs.d/thirdparty/haskell-mode") ; override haskell mode on system
;;(add-to-list 'load-path "~/.cabal/share/ghc-mod-2.0.2") ; load ghc-mod from cabal
;; (autoload 'ghc-init "~/.emacs.d/thirdparty/ghc-mod-2.0.2/ghc" nil t)

;; (load "haskell-site-file")

;; ;; scion
;; (if (file-exists-p "~/.cabal/share/scion-0.3/emacs/scion.el")
;;     (progn
;;       (add-to-list 'load-path "~/.cabal/share/scion-0.3/emacs")
;;       (require 'scion)
;;       (setq scion-program "~/.cabal/bin/scion-server")
;;       (add-hook
;;        'haskell-mode-hook
;;        (lambda ()
;;          (scion-mode 1)
;;          (scion-flycheck-on-save 1)
;;          (setq scion-completing-read-function 'ido-completing-read)))))

(add-hook
 'haskell-mode-hook
 (lambda ()
   ;; (ghc-init)
   (turn-on-haskell-indent)
   (capitalized-words-mode)
   (turn-on-haskell-doc-mode)
   (turn-on-haskell-decl-scan)
   (imenu-add-menubar-index)
   (setq
    haskell-font-lock-haddock t
    haskell-stylish-on-save t
    ;; haskell-tags-on-save t
    haskell-program-name "ghci"
    haskell-indent-offset 4
    whitespace-line-column 78)
   ))

(message "Loading haskell-init...done")
(provide 'haskell-init)
