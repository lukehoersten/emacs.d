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
(setq-default c-default-style "hoersten") ; load c-style

(message "Loading hoersten-c-style...done")
(provide 'hoersten-c-style)
