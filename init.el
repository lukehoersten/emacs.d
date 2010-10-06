;; ~/.emacs.d/init.el (~/.emacs)
;; Luke Hoersten <Luke@Hoersten.org>

;; general
(add-to-list 'load-path "~/.emacs.d/")                  ; set default emacs load path

(setq-default
 ediff-split-window-function 'split-window-horizontally ; diff horizontally
 x-select-enable-clipboard t                            ; paste from X buffer
 inhibit-splash-screen t                                ; disable splash screen
 truncate-lines t                                       ; truncate, not wrap, lines
 indent-tabs-mode nil                                   ; only uses spaces for indentation
 split-width-threshold 181)                             ; min width to split window horizontially

(put 'set-goal-column 'disabled nil)                    ; enable goal column setting
(put 'narrow-to-region 'disabled nil)                   ; enable hiding
(put 'narrow-to-page 'disabled nil)

(menu-bar-mode nil)                                     ; remove menu bar
(display-time-mode t)                                   ; show clock
(column-number-mode t)                                  ; show column numbers
(delete-selection-mode t)                               ; replace highlighted text
(windmove-default-keybindings)                          ; move between windows with shift-arrow
(ido-mode t)                                            ; file/buffer selector
(setq-default ido-enable-flex-matching t)               ; fuzzy matching for ido mode
(add-hook 'text-mode-hook 'flyspell-mode t)             ; spellcheck text
(add-hook 'text-mode-hook 'turn-on-auto-fill)           ; autofill text

;; whitespace
(global-whitespace-mode t)                              ; show whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)        ; cleanup whitespace on exit
(setq-default
 whitespace-line-column 120                             ; column width
 whitespace-style                                       ; whitespace to highlight
 '(trailing lines-tail empty indentation
            space-before-tab space-after-tab))

;; coding
(which-func-mode t)                                     ; show current function
(show-paren-mode t)                                     ; show matching paren
(transient-mark-mode t)                                 ; show highlighting
(global-font-lock-mode t)                               ; syntax highlighting
(setq-default compile-command "scons ")                 ; compile command
(global-set-key (kbd "C-c c") 'compile)                 ; compile
(global-set-key (kbd "C-c r") 'recompile)               ; recompile
(global-set-key (kbd "C-c C-c")                         ; comment
                'comment-or-uncomment-region)

;; line numbers
(mapc
 (lambda (x)
   (add-hook x 'linum-mode))
 '(text-mode-hook
   c-mode-common-hook
   python-mode-hook
   haskell-mode-hook
   emacs-lisp-mode-hook))

;; use only spaces for alignment
(global-set-key (kbd "C-c a") 'align-with-spaces)
(defun align-with-spaces (beg end pattern)
  "Align selected using only spaces for whitespace."
  (interactive "r\nsAlign by: ")
  (let ((indent-tabs-mode nil))
    (align-string beg end pattern 1)
    (align-entire beg end)
    (untabify beg end)
    (indent-region beg end)
    (whitespace-cleanup-region beg end)))

;; x stuff
(if window-system
    (progn
      (tool-bar-mode nil)   ; remove tool bar
      (scroll-bar-mode nil) ; remove scroll bar
      (visual-line-mode t)  ; word wrap break on whitespace
      (set-default-font "Monospace-10")

      ;; twilight theme
      (require 'color-theme)
      (load "color-theme-twilight")
      (color-theme-twilight)
      (global-hl-line-mode t)))

;; terminal and shell
(global-set-key (kbd "C-c t") '(lambda () (interactive) (ansi-term "bash" "term"))) ; start term
(global-set-key (kbd "C-c s") 'shell) ; start shell - acts like emacs buffer
(ansi-color-for-comint-mode-on)       ; color in shell buffer
(setq-default
 comint-prompt-read-only t            ; don't type on prompt
 comint-scroll-to-bottom-on-input t   ; only type on prompt
 comint-scroll-show-maximum-output t) ; place text at bottom

;;;;;;;;;;;;; includes & requires ;;;;;;;;;;;;;

;; includes
(require 'hoersten-pastebin-region) ; send selected text to pastebin
(require 'hoersten-c-style)         ; load c specific lisp
(require 'vala-mode)                ; vala programming language

;; nav mode
(add-to-list 'load-path "~/.emacs.d/nav/")
(require 'nav)

;; unicode
(require 'pretty-mode)
(global-pretty-mode t)

;; snippets
(add-to-list 'load-path "~/.emacs.d/yasnippet/")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet/snippets/")

;; python mode
(add-hook
 'python-mode-hook
 (lambda ()
   (setq
    tab-width 3
    python-indent 3
    indent-tabs-mode t)))

;; haskell mode
(add-hook
 'haskell-mode-hook
 (lambda ()
   (capitalized-words-mode)
   (setq
    haskell-font-lock-symbols 'unicode
    haskell-doc-mode t
    imenu-add-menubar-index
    inferior-haskell-wait-and-jump t
    haskell-indent-mode t
    haskell-indent-offset 3)))

;; zencoding html
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ; Auto-start on any markup modes

;; unique buffer names with dirs
(require 'uniquify)
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":")

;; org mode
(add-hook
 'org-mode-hook
 (lambda ()
   (local-set-key (kbd "M-p") 'org-move-item-up)
   (local-set-key (kbd "M-S-p") 'org-move-subtree-up)
   (local-set-key (kbd "M-n") 'org-move-item-down)
   (local-set-key (kbd "M-S-n") 'org-move-subtree-down)))
