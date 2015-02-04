;; ~/.emacs.d/elisp/haskell-init.el
;; Luke Hoersten <Luke@Hoersten.org>

;; Require packages
(require 'package-require)
(package-require '(haskell-mode hi2 flycheck)) ;;  flycheck-haskell hindent shm

(require 'flycheck)

;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))
;; (require 'flycheck-haskell)

(require 'haskell)
(require 'haskell-mode)
(require 'haskell-process)
(require 'haskell-flycheck)
(require 'haskell-interactive-mode)

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

(defun haskell-process-all-types ()
  "List all types in a grep-mode buffer."
  (interactive)
  (let ((session (haskell-session)))
    (switch-to-buffer (get-buffer-create (format "*%s:all-types*"
                                                 (haskell-session-name (haskell-session)))))
    (setq haskell-session session)
    (cd (haskell-session-current-dir session))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((haskell-process-log nil))
        (insert (haskell-process-queue-sync-request (haskell-process) ":all-types")))
      (unless (eq major-mode  'compilation-mode)
        (compilation-mode)
        (setq compilation-error-regexp-alist
              haskell-compilation-error-regexp-alist)))))

;;; haskell-mode
(add-hook
 'haskell-mode-hook
 (lambda ()
   (imenu-add-menubar-index)
   (flycheck-mode)
   (flycheck-disable-checker `haskell-ghc)
   (turn-on-hi2)
   (interactive-haskell-mode)))

(custom-set-variables
 '(capitalized-words-mode t)
 '(haskell-stylish-on-save t)

 '(hi2-layout-offset 4)
 '(hi2-left-offset 4)

 '(haskell-process-type 'cabal-repl)
 '(haskell-process-args-cabal-repl '("--ghc-option=-ferror-spans" "--with-ghc=ghci-ng"))
 '(haskell-process-path-ghci "ghci-ng")
 '(haskell-process-args-ghci "-ferror-spans")
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-reload-with-fbytecode nil)
 '(haskell-process-use-presentation-mode t)
 '(haskell-process-suggest-haskell-docs-imports t)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-generate-tags nil)
 '(haskell-process-show-debug-tips nil)

 '(haskell-notify-p t)
 '(haskell-align-imports-pad-after-name t)
 '(haskell-ask-also-kill-buffers t)
 '(haskell-import-mapping t)

 '(haskell-interactive-mode-eval-pretty t)
 '(haskell-interactive-mode-scroll-to-bottom t)
 '(haskell-interactive-mode-eval-mode 'haskell-mode))

;; haskell-interactive-mode keybindings
(define-key interactive-haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key interactive-haskell-mode-map (kbd "M-,") 'haskell-who-calls)
(define-key interactive-haskell-mode-map (kbd "M-.") 'haskell-mode-goto-loc)
(define-key interactive-haskell-mode-map (kbd "C-?") 'haskell-mode-find-uses)
(define-key interactive-haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key interactive-haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key interactive-haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key interactive-haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key interactive-haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)

(define-key haskell-interactive-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-interactive-mode-map (kbd "C-<left>") 'haskell-interactive-mode-error-backward)
(define-key haskell-interactive-mode-map (kbd "C-<right>") 'haskell-interactive-mode-error-forward)
(define-key haskell-interactive-mode-map (kbd "C-c c") 'haskell-process-cabal)

(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-d") 'haskell-describe)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

(define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)

(message "Loading haskell-init...done")
(provide 'haskell-init)


;;;; Old haskell stuff
;; ;;; haskell-mode
;; (autoload 'ghc-init "ghc" nil t)
;; (add-hook
;;  'haskell-mode-hook
;;  (lambda ()
;;    (ghc-init)
;;    (capitalized-words-mode t)
;;    (turn-on-hi2)
;;    (imenu-add-menubar-index)
;;    (interactive-haskell-mode)
;;    (local-set-key (kbd "C-c i") 'haskell-navigate-imports) ; go to imports. prefix to return
;;    (local-set-key (kbd "M-p") 'move-line-up) ; need to override default M-p function
;;    (local-set-key (kbd "M-n") 'move-line-down)
;;    (local-set-key (kbd "C-1") 'ghc-display-errors)
;;    (local-set-key (kbd "C-.") 'ghc-goto-next-error)
;;    (local-set-key (kbd "C-,") 'ghc-goto-prev-error)

;;    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
;;    (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
;;    (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
;;    (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
;;    (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;;    (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;;    (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
;;    (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

;;    (setq
;;     ghc-ghc-options '("-isrc")
;;     haskell-program-name "cabal repl"
;;     haskell-stylish-on-save t
;;     hi2-layout-offset 4
;;     hi2-left-offset 4
;;     whitespace-line-column 78
;;     ;; haskell-process-type 'cabal-repl
;;     haskell-process-suggest-remove-import-lines t
;;     haskell-process-auto-import-loaded-modules t
;;     haskell-process-log t
;;     )))

;; ;;; ghci-mode
;; (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion)
