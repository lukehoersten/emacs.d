;; ~/.emacs.d/haskell-init.el
;; Luke Hoersten <Luke@Hoersten.org>

(add-to-list 'load-path "~/.emacs.d/thirdparty/haskell-mode") ; override haskell mode on system
(load "haskell-site-file")

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
   (turn-on-haskell-indent)
   (capitalized-words-mode)
   (turn-on-haskell-doc-mode)
   (turn-on-haskell-decl-scan)
   (imenu-add-menubar-index)
   (local-set-key (kbd "C-x C-s") 'haskell-mode-save-buffer)
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
