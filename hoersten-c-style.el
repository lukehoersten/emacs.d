;; ~/.emacs.d/hoersten-c-style.el - Luke Hoersten - v1.0

;; hoersten c++-style
(c-add-style "hoersten"
             '(;; indentation
               (indent-tabs-mode . t)
               (tab-width        . 3)
               (c-basic-offset   . 3)

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
                (substatement-open . 0)
                (inline-open       . 0)
                (case-label        . +)
                (innamespace       . 0)
                (cpp-macro         . -))))

;; c-like language settings (c, c++, java, etc.)
(setq-default c-hungry-delete-key t)                             ; enable hungry delete
(setq-default c-default-style "hoersten")                        ; load c-style
(add-hook 'c-mode-common-hook
          (lambda()
            ;; indentation
            (setq tab-width 3)
            (setq c-basic-offset 3)
            (setq indent-tabs-mode t)

            (c-toggle-auto-newline t)                            ; auto newline
            (auto-fill-mode t)                                   ; word wrap

            ;; custom keys
            (local-set-key (kbd "C-c f")   'ff-find-other-file)  ; toggle header/source file
            (local-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

            ;; code folding
            (local-set-key (kbd "C-c v") 'hs-toggle-hiding)
            (local-set-key (kbd "<f1>")  'hs-hide-all)
            (local-set-key (kbd "<f2>")  'hs-show-all)
            (hs-minor-mode t)                                    ; enable hide-show mode
            (hs-hide-all)                                        ; hide all blocks by default

            ;; highlight todos
            (font-lock-add-keywords
             nil
             '(("\\([@]\\(TODO\\|todo\\|warning\\|note\\)\\)" 1 font-lock-warning-face t)))))
