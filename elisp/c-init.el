;; ~/.emacs.d/c-hook.el
;; Luke Hoersten <Luke@Hoersten.org>

(require 'hoersten-c-style)

(add-hook
 'c-mode-common-hook
 (lambda ()
   ;; indentation
   (setq
    tab-width 3
    c-basic-offset 3
    indent-tabs-mode t
    standard-indent 3
    whitespace-line-column 120)

   (setq
    compile-command "scons "
    c-hungry-delete-key t)

   (c-toggle-auto-newline t) ; auto newline
   (c-subword-mode t)

   ;; custom keys
   (local-set-key (kbd "C-c f")   'ff-find-other-file) ; toggle header/source file

   ;; ;; code folding
   ;; (local-set-key (kbd "C-c v") 'hs-toggle-hiding)
   ;; (local-set-key (kbd "<f1>")  'hs-hide-all)
   ;; (local-set-key (kbd "<f2>")  'hs-show-all)
   ;; (hs-minor-mode t) ; enable hide-show mode

   ;; gdb settings
   (setq
    gdb-many-windows t                ; gdb many windows
    gdb-use-separate-io-buffer t      ; gdb stdio output
    gud-tooltip-mode t)               ; mouse hover variables
   (local-set-key (kbd "C-c g") 'gdb) ; gdb

   ;; auto-close bracing
   (setq parens-require-spaces nil)
   (dolist (key '("(" "[")) (define-key (current-local-map) key 'insert-pair))
   ))

(add-to-list 'auto-mode-alist '("\\.ipp$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl$" . c++-mode))

(message "Loading c-init...done")
(provide 'c-init)
