;; ~/.emacs.d/elisp/haskell-intero-init.el
;; Luke Hoersten <Luke@Hoersten.org>

;;; Code:

;; Require packages
(require 'package-require)
(package-require '(haskell-mode intero yasnippet haskell-snippets flycheck company))

(require 'haskell)
(require 'haskell-mode)
(require 'haskell-interactive-mode)
(require 'haskell-snippets)
(require 'company)

(add-hook 'haskell-mode-hook 'intero-mode)

(setq
 haskell-stylish-on-save t
 haskell-indentation-layout-offset 4
 haskell-indentation-left-offset 4

 haskell-notify-p t
 haskell-align-imports-pad-after-name t
 haskell-ask-also-kill-buffers nil
 haskell-import-mapping t

 haskell-interactive-mode-eval-pretty t
 haskell-interactive-mode-scroll-to-bottom t
 haskell-interactive-mode-eval-mode 'haskell-mode
 haskell-interactive-popup-errors nil)

(message "Loading haskell-init...done")
(provide 'haskell-intero-init)
