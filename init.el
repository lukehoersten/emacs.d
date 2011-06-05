;; ~/.emacs.d/init.el (~/.emacs)
;; Luke Hoersten <Luke@Hoersten.org>

;;;; General ;;;;
(add-to-list 'load-path "~/.emacs.d")            ; set default emacs load path

(setq-default
 ediff-split-window-function
  'split-window-horizontally              ; diff horizontally
 x-select-enable-clipboard t              ; paste from X buffer
 inhibit-splash-screen t                  ; disable splash screen
 truncate-lines t                         ; truncate, not wrap, lines
 indent-tabs-mode nil                     ; only uses spaces for indentation
 split-width-threshold 181                ; min width to split window horizontially
 split-height-threshold 120               ; min width to split window vertically
 reb-re-syntax 'string)                   ; use string syntax for regexp builder

(put 'set-goal-column 'disabled nil)      ; enable goal column setting
(put 'narrow-to-region 'disabled nil)     ; enable hiding
(put 'narrow-to-page 'disabled nil)

(column-number-mode t)                    ; show column numbers
(delete-selection-mode t)                 ; replace highlighted text
(windmove-default-keybindings)            ; move between windows with shift-arrow
(fset 'yes-or-no-p 'y-or-n-p)             ; replace yes/no prompts

;;; coding
(which-func-mode t)                       ; show current function
(show-paren-mode t)                       ; show matching paren
(transient-mark-mode t)                   ; show highlighting
(global-font-lock-mode t)                 ; syntax highlighting
(global-set-key (kbd "C-c c") 'compile)   ; compile
(global-set-key (kbd "C-c r") 'recompile) ; recompile
(global-set-key
 (kbd "C-c C-c")
 'comment-or-uncomment-region)            ; toggle region comment

;;; Darwin
(if (string-match "darwin" (emacs-version))
    (progn
      (setq-default ns-command-modifier 'control)
      (tabbar-mode nil)))

;;; Xorg
(if window-system
    (progn
      (defun get-font ()
        "Get appropriate font based on system and hostname."
        (cond
         ((string-match "darwin" (emacs-version)) "Menlo-12")
         ((string-match "HoldenCaulfield" (system-name)) "monospace-6")
         ((string-match "lhoersten-66113" (system-name)) "monospace-8")
         ("monospace-10")))

      (tool-bar-mode -1)      ; remove tool bar
      (scroll-bar-mode -1)    ; remove scroll bar
      (visual-line-mode t)    ; word wrap break on whitespace
      (global-hl-line-mode t) ; highlight current line
      (set-frame-font (get-font))))

;;; terminal
(global-set-key (kbd "C-c s") 'eshell) ; start shell
(add-hook
 'eshell-mode-hook
 (lambda ()
   (setenv "TERM" "emacs") ; enable colors
   (setenv "PATH" (concat "/opt/ghc7/bin:" "~/.cabal/bin:" (getenv "PATH")))))


;;;; Mode-Specific ;;;;

;;; text-mode
(add-hook 'text-mode-hook 'flyspell-mode t)             ; spellcheck text
(add-hook 'text-mode-hook 'turn-on-auto-fill)           ; autofill text

;;; ido-mode
(ido-mode t)                                            ; file/buffer selector
(setq-default
 ido-enable-flex-matching t                             ; fuzzy matching for ido mode
 ido-create-new-buffer 'always                          ; create new buffer without prompt
 ido-everywhere t)                                      ; use ido where possible

;;; whitespace-mode
(global-whitespace-mode t)                              ; show whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)        ; cleanup whitespace on exit
(setq-default
 whitespace-line-column 120                             ; column width
 whitespace-style                                       ; whitespace to highlight
 '(trailing lines-tail empty indentation
            space-before-tab space-after-tab))

;;; python-mode
(add-hook 'python-mode-hook (lambda () (setq indent-tabs-mode t)))

;;; org-mode
(add-hook
 'org-mode-hook
 (lambda ()
   (local-set-key (kbd "M-p") 'org-move-item-up)
   (local-set-key (kbd "M-S-p") 'org-move-subtree-up)
   (local-set-key (kbd "M-n") 'org-move-item-down)
   (local-set-key (kbd "M-S-n") 'org-move-subtree-down)))

;;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer) ; better buffer browser
(setq-default
 ibuffer-show-empty-filter-groups nil
 ibuffer-saved-filter-groups
 '(("default"
    ("Emacs Lisp" (mode . emacs-lisp-mode))
    ("Haskell" (mode . haskell-mode))
    ("Python" (mode . python-mode))
    ("C++" (mode . c++-mode)))))
(add-hook
 'ibuffer-mode-hook
 (lambda ()
   (ido-mode t)
   (ibuffer-switch-to-saved-filter-groups "default")))


;;;; Requires ;;;;

(add-to-list 'load-path "~/.emacs.d/thirdparty") ; set default third party path

;;; language init
(require 'c-init)             ; c specific elisp
(require 'haskell-init)       ; haskell specific elisp
(require 'vala-mode)          ; vala programming language

;;; function init
(require 'align-with-spaces)  ; use only spaces for alignment
(require 'pastebin-region)    ; send selected text to pastebin
(require 'move-line)          ; move line up or down
(require 'rainbow-delimiters) ; multi-colored parens

;;; twilight theme
(if window-system
    (progn
      (require 'color-theme)
      (load "color-theme-twilight")
      (color-theme-twilight)))

;;; yasnippets
(add-to-list 'load-path "~/.emacs.d/thirdparty/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/thirdparty/yasnippet/snippets")
(setq-default yas/prompt-functions '(yas/ido-prompt yas/dropdown-prompt)) ; use ido for multiple snippets

;;; zencoding-mode - html
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ; Auto-start on any markup modes

;;; unique buffer names with dirs
(require 'uniquify)
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":")

;;; coding-modes map
(mapc
 (lambda (x)
   (add-hook x 'linum-mode)
   (add-hook x 'rainbow-delimiters-mode))
 '(text-mode-hook
   c-mode-common-hook
   python-mode-hook
   haskell-mode-hook
   emacs-lisp-mode-hook))
