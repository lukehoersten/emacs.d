;; ~/.emacs.d/elisp/haskell-init.el
;; Luke Hoersten <Luke@Hoersten.org>

;;; Code:

;; Require packages
(require 'package-require)
(package-require '(haskell-mode ghc yasnippet haskell-snippets flycheck company company-ghc))

(require 'haskell)
(require 'haskell-mode)
(require 'haskell-interactive-mode)
(require 'haskell-snippets)
(require 'company)

(add-to-list 'company-backends 'company-ghc)

(defun haskell-who-calls (&optional prompt)
  "Grep the codebase to see who uses the symbol at point."
  (interactive "P")
  (let ((sym (if prompt
                 (read-from-minibuffer "Look for: ")
               (haskell-ident-at-point))))
    (let ((existing (get-buffer "*who-calls*")))
      (when existing
        (kill-buffer existing)))
    (let ((buffer
           (grep-find (format "cd %s && find . -name '*.hs' -exec grep -inH -e %s {} +"
                              (haskell-session-current-dir (haskell-session))
                              sym))))
      (with-current-buffer buffer
        (rename-buffer "*who-calls*")
        (switch-to-buffer-other-window buffer)))))

;;; haskell-mode
(add-hook
 'haskell-mode-hook
 (lambda ()
   (flycheck-mode t)
   (imenu-add-menubar-index)
   (haskell-indentation-mode t)
   (subword-mode t)
   (capitalized-words-mode t)
   (interactive-haskell-mode t)))

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
 haskell-interactive-popup-errors nil

 haskell-process-args-cabal-repl '("--ghc-options=-ferror-spans" "--with-ghc=ghci-ng")
 haskell-process-args-ghci '("-ferror-spans")
 haskell-process-args-stack-ghci '("--ghc-options=-ferror-spans" "--with-ghc=ghci-ng")
 haskell-process-auto-import-loaded-modules t
 haskell-process-reload-with-fbytecode nil
 haskell-process-log t
 haskell-process-suggest-haskell-docs-imports t
 haskell-process-suggest-remove-import-lines t
 haskell-process-use-presentation-mode t)


;; keys
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-d") 'haskell-describe)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-process-clear)
(define-key haskell-mode-map (kbd "M-,") 'haskell-who-calls)
(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-goto-loc)
(define-key haskell-mode-map (kbd "C-c i") 'haskell-navigate-imports)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-r") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)

(define-key interactive-haskell-mode-map (kbd "C-?") 'haskell-mode-find-uses)
(define-key interactive-haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)
(define-key interactive-haskell-mode-map (kbd "M-.") 'haskell-mode-goto-loc)
(define-key interactive-haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key interactive-haskell-mode-map (kbd "C-c C-k") 'haskell-process-clear)
(define-key interactive-haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)

(define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-interactive-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-interactive-mode-map (kbd "C-c C-t") 'haskell-process-do-type)

(message "Loading haskell-init...done")
(provide 'haskell-init)
