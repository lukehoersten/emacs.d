;; ~/.emacs.d/hoersten-c-style.el
;; Luke Hoersten <Luke@Hoersten.org>

;; hoersten c++-style
(c-add-style
 "hoersten"
 '(;; indentation
   (indent-tabs-mode       . t)
   (tab-width              . 3)
   (c-basic-offset         . 3)

   ;; brace cleanups
   (c-cleanup-list
    brace-else-brace
    brace-elseif-brace
    brace-catch-brace
    empty-defun-braces
    defun-close-semi
    list-close-comma
    scope-operator)

   ;; syntactic symbols
   (c-offsets-alist
    (substatement-open     . 0)
    (inline-open           . 0)
    (case-label            . +)
    (innamespace           . 0)
    (arglist-cont-nonempty . +)
    (cpp-macro             . -))))

;; c-like language settings (c, c++, java, etc.)
;;(require 'doxymacs)
(setq-default
 c-hungry-delete-key t       ; enable hungry delete
 c-default-style "hoersten") ; load c-style

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

   (c-toggle-auto-newline t) ; auto newline
   (c-subword-mode t)

   ;; custom keys
   (local-set-key (kbd "C-c f")   'ff-find-other-file) ; toggle header/source file
   (local-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

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

(provide 'hoersten-c-style)