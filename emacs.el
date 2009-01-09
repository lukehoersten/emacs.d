;; ~/.emacs - Luke Hoersten - v4.0

;; general
(setq-default load-path (cons "~/.emacs.d" load-path))                ; set default emacs load path

(setq-default ediff-split-window-function 'split-window-horizontally) ; diff horizontally
(setq-default x-select-enable-clipboard t)                            ; paste from X buffer
(setq-default inhibit-splash-screen t)                                ; disable splash screen
;;(setq-default show-trailing-whitespace t)                             ; show trailing whitespace
(put 'set-goal-column 'disabled nil)                                  ; enable goal column setting
(put 'narrow-to-region 'disabled nil)                                 ; enable hiding

(display-time-mode t)                                                 ; show clock
(column-number-mode t)                                                ; show column numbers
(delete-selection-mode t)                                             ; replace highlighted text
(windmove-default-keybindings)                                        ; move between windows with shift-arrow

(ido-mode t)                                                          ; file/buffer selector
(setq-default ido-enable-flex-matching t)                             ; fuzzy matching is a must have
(add-hook 'text-mode-hook 'flyspell-mode t)                           ; spellcheck text

;; coding
(which-func-mode t)                                                   ; show current function
(show-paren-mode t)                                                   ; show matching paren
(transient-mark-mode t)                                               ; show highlighting
(global-font-lock-mode t)                                             ; syntax highlighting
(global-whitespace-mode t)                                            ; show whitespace
(setq-default whitespace-style '(tab-mark trailing tabs empty))       ; what whitespace elements to show
(add-hook 'before-save-hook 'whitespace-cleanup)                      ; cleanup whitespace on exit
(load "hoersten-pastebin-region")                                     ; send selected text to pastebin
(load "mercurial")                                                    ; load mercurial mode
(load "ahg")                                                          ; load suplimental mercurial mode
(load "hoersten-c-style")                                             ; load c specific lisp
(global-set-key (kbd "C-c c") 'compile)

;; gdb settings
(setq-default gdb-many-windows t)                                     ; gdb many windows
(setq-default gdb-use-separate-io-buffer t)                           ; gdb stdio output
(setq-default gud-tooltip-mode t)                                     ; mouse hover variables
(global-set-key (kbd "C-c g") 'gdb)                                   ; gdb

;; haskell
(setq haskell-font-lock-symbols 'unicode)

;; use only spaces for alignment
(global-set-key (kbd "C-c a") 'align-with-spaces) ; align
(defun align-with-spaces (beg end pattern)
  "Align selected using only spaces for whitespace."
  (interactive "r\nsAlign by: ")
  (let ((indent-tabs-mode nil))
    (align-string beg end pattern 1)
    (align-entire beg end)
    (untabify beg end)
    (indent-region beg end)
    (whitespace-cleanup-region beg end)))

;; Shell
(global-set-key (kbd "C-c s") 'shell)              ; start shell
(ansi-color-for-comint-mode-on)                    ; color in shell buffer
(setq-default comint-scroll-to-bottom-on-input t)  ; only type on prompt
(setq-default comint-scroll-show-maximum-output t) ; place text at bottom

;; map file extensions to modes
(setq-default auto-mode-alist
              (append
               '(("\\.ipp$" . c++-mode)
                 ("\\.inl$" . c++-mode)
                 ("SCons"   . python-mode)
                 ("\\.jj$"  . java-mode))
               auto-mode-alist))

;; x stuff
(if (not window-system)
    (menu-bar-mode nil) ; remove menu bar in no-x mode
  (tool-bar-mode nil)   ; remove tool bar
  (scroll-bar-mode nil) ; remove scroll bar
  (custom-set-faces '(default ((t (:background "#000000" :foreground "#ffffff" :height 100 :family "DejaVu Sans Mono")))))
  (setq default-frame-alist '((width . 100) (height . 50) (menu-bar-lines . 1)))

  ;; twilight theme
  (require 'color-theme)
  (load "color-theme-twilight")
  (color-theme-twilight)
  (global-hl-line-mode t))
