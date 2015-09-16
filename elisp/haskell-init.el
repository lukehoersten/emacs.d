;; ~/.emacs.d/elisp/haskell-init.el
;; Luke Hoersten <Luke@Hoersten.org>

;; Require packages
(require 'package-require)
(package-require '(haskell-mode yasnippet haskell-snippets flycheck flycheck-haskell shm))

(add-to-list 'load-path "~/.emacs.d/elisp/stack-mode")

;; Load haskell-mode from source
;; (add-to-list 'load-path "~/Code/elisp/haskell-mode/")
;; (require 'haskell-mode-autoloads)

(require 'haskell)
(require 'haskell-mode)
(require 'stack-mode)
(require 'haskell-interactive-mode)
(require 'haskell-snippets)
(require 'shm)

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
   ;; (imenu-add-menubar-index)
   (flycheck-mode)
   (flycheck-haskell-setup)
   (flycheck-disable-checker 'haskell-ghc)
   ;; (haskell-indentation-mode t)
   (stack-mode)
   (subword-mode)
   (electric-indent-mode 0)
   (structured-haskell-mode t)
   (set-face-background 'shm-quarantine-face "lemonchiffon")
   (interactive-haskell-mode t)))

(custom-set-variables
 '(capitalized-words-mode t)
 '(haskell-stylish-on-save t)

 '(haskell-indentation-layout-offset 4)
 '(haskell-indentation-left-offset 4)

 '(haskell-interactive-mode-eval-pretty t)
 '(haskell-interactive-mode-scroll-to-bottom t)
 '(haskell-interactive-mode-eval-mode 'haskell-mode)

 '(haskell-notify-p t)
 '(haskell-align-imports-pad-after-name t)
 '(haskell-ask-also-kill-buffers nil)
 '(haskell-import-mapping t)

 '(shm-use-presentation-mode t)
 '(shm-auto-insert-skeletons t)
 '(shm-auto-insert-bangs t))


;;  ;; '(haskell-process-type 'cabal-repl)
;;  ;; ;; '(haskell-process-args-cabal-repl '("--ghc-option=-ferror-spans" "--with-ghc=ghci-ng")) ;; ghci-ng
;;  ;; ;; '(haskell-process-path-ghci "ghci-ng") ;; ghci-ng
;;  ;; '(haskell-process-args-ghci "-ferror-spans")
;;  ;; '(haskell-process-suggest-remove-import-lines t)
;;  ;; '(haskell-process-auto-import-loaded-modules t)
;;  ;; '(haskell-process-log t)
;;  ;; '(haskell-process-reload-with-fbytecode nil)
;;  ;; '(haskell-process-use-presentation-mode t)
;;  ;; '(haskell-process-suggest-haskell-docs-imports t)
;;  ;; '(haskell-process-suggest-hoogle-imports t)
;;  ;; '(haskell-process-generate-tags nil)
;;  ;; '(haskell-process-show-debug-tips nil)



;; ;; haskell-interactive-mode keybindings
;; (define-key interactive-haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
;; (define-key interactive-haskell-mode-map (kbd "M-,") 'haskell-who-calls)
;; (define-key interactive-haskell-mode-map (kbd "M-.") 'haskell-mode-goto-loc)
;; (define-key interactive-haskell-mode-map (kbd "C-?") 'haskell-mode-find-uses)
;; (define-key interactive-haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;; (define-key interactive-haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)
;; (define-key interactive-haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
;; (define-key interactive-haskell-mode-map (kbd "C-c C-k") 'haskell-process-clear)
;; (define-key interactive-haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)

;; (define-key haskell-interactive-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
;; (define-key haskell-interactive-mode-map (kbd "C-<left>") 'haskell-interactive-mode-error-backward)
;; (define-key haskell-interactive-mode-map (kbd "C-<right>") 'haskell-interactive-mode-error-forward)
;; (define-key haskell-interactive-mode-map (kbd "C-c c") 'haskell-process-cabal)

;; ;; haskell-mode
;; (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
;; (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
;; (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
;; (define-key haskell-mode-map (kbd "C-c i") 'haskell-navigate-imports)
;; (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
;; (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;; (define-key haskell-mode-map (kbd "C-c C-d") 'haskell-describe)
;; (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-process-clear)
;; (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
;; (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

;; ;; cabal
;; (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
;; (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-process-clear)
;; (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;; (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)

;; shm
(define-key shm-map (kbd "C-c C-p") 'shm/expand-pattern)
(define-key shm-map (kbd "C-c C-s") 'shm/case-split)
(define-key shm-map (kbd "C-\\") 'shm/goto-last-point)

(message "Loading haskell-init...done")
(provide 'haskell-init)
