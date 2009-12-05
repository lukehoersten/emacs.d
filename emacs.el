;; ~/.emacs
;; Luke Hoersten <Luke@Hoersten.org>

;; general
(setq-default load-path (cons "~/.emacs.d/" load-path))               ; set default emacs load path

(setq-default ediff-split-window-function 'split-window-horizontally) ; diff horizontally
(setq-default x-select-enable-clipboard t)                            ; paste from X buffer
(setq-default inhibit-splash-screen t)                                ; disable splash screen
(setq-default truncate-lines t)
(put 'set-goal-column 'disabled nil)                                  ; enable goal column setting
(put 'narrow-to-region 'disabled nil)                                 ; enable hiding

(menu-bar-mode nil)                                                   ; remove menu bar
(display-time-mode t)                                                 ; show clock
(column-number-mode t)                                                ; show column numbers
(delete-selection-mode t)                                             ; replace highlighted text
(windmove-default-keybindings)                                        ; move between windows with shift-arrow
(setq-default indent-tabs-mode nil)                                   ; mouse hover variables
(ido-mode t)                                                          ; file/buffer selector
(setq-default ido-enable-flex-matching t)                             ; fuzzy matching for ido mode
(add-hook 'text-mode-hook 'flyspell-mode t)                           ; spellcheck text

;; whitespace
(global-whitespace-mode t)                                            ; show whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)                      ; cleanup whitespace on exit
(setq-default whitespace-line-column 120)                             ; column width
(setq-default whitespace-style
              '(tabs tab-mark trailing lines-tail
                     space-before-tab indentation empty))             ; what whitespace elements to show

;; coding
(which-func-mode t)                                                   ; show current function
(show-paren-mode t)                                                   ; show matching paren
(transient-mark-mode t)                                               ; show highlighting
(global-font-lock-mode t)                                             ; syntax highlighting
(global-set-key (kbd "C-c c") 'compile)                               ; compile
(global-set-key (kbd "C-c r") 'recompile)                             ; recompile

;; includes
(require 'hoersten-pastebin-region)                                   ; send selected text to pastebin
(require 'mercurial)                                                  ; load mercurial mode
(require 'nav)                                                        ; load nav bar
(require 'hoersten-c-style)                                           ; load c specific lisp

;; unicode
(require 'pretty-mode)
(global-pretty-mode t)
(setq haskell-font-lock-symbols 'unicode)

;; snippets
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets/")

;; shell
(global-set-key (kbd "C-c s") 'shell)              ; start shell
(ansi-color-for-comint-mode-on)                    ; color in shell buffer
(setq-default comint-scroll-to-bottom-on-input t)  ; only type on prompt
(setq-default comint-scroll-show-maximum-output t) ; place text at bottom

;; org mode
(add-hook 'org-mode-hook
          '(lambda ()
             (local-set-key (kbd "M-p") 'org-move-item-up)
             (local-set-key (kbd "M-S-p") 'org-move-subtree-up)
             (local-set-key (kbd "M-n") 'org-move-item-down)
             (local-set-key (kbd "M-S-n") 'org-move-subtree-down)))

;; unique buffer names with dirs
(require 'uniquify)
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":")

;; line numbers
;;(global-linum-mode t)
(mapc (lambda (x)
        (add-hook x 'linum-mode))
      '(text-mode-hook
        c-mode-common-hook
        python-mode-hook
        haskell-mode-hook
        emacs-lisp-mode-hook))

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
