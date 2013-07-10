;; ~/.emacs.d/init.el (~/.emacs)
;; Luke Hoersten <Luke@Hoersten.org>

;;;; General ;;;;
(add-to-list 'load-path "~/.emacs.d")     ; set default emacs load path

(setq-default
 ediff-split-window-function
 'split-window-horizontally              ; diff horizontally
 inhibit-splash-screen t                  ; disable splash screen
 truncate-lines t                         ; truncate, not wrap, lines
 indent-tabs-mode nil                     ; only uses spaces for indentation
 split-width-threshold 181                ; min width to split window horizontially
 split-height-threshold 120               ; min width to split window vertically
 reb-re-syntax 'string                    ; use string syntax for regexp builder
 require-final-newline 'visit-save)       ; add a newline automatically

(put 'set-goal-column 'disabled nil)      ; enable goal column setting
(put 'narrow-to-region 'disabled nil)     ; enable hiding
(put 'narrow-to-page 'disabled nil)

(column-number-mode t)                    ; show column numbers
(delete-selection-mode t)                 ; replace highlighted text
(windmove-default-keybindings)            ; move between windows with shift-arrow
(fset 'yes-or-no-p 'y-or-n-p)             ; replace yes/no prompts
(global-hl-line-mode t)                   ; highlight current line


;;; Coding
(which-func-mode t)                       ; show current function
(show-paren-mode t)                       ; show matching paren
(transient-mark-mode t)                   ; show highlighting
(global-font-lock-mode t)                 ; syntax highlighting
(global-set-key (kbd "C-c c") 'compile)   ; compile
(global-set-key (kbd "C-c r") 'recompile) ; recompile
(global-set-key (kbd "C-c a") 'align-regexp) ; align
(global-set-key (kbd "C-c g") 'rgrep)     ; grep
(subword-mode t)                          ; move by camelCase words


;;; Darwin
(setq is-mac (equal system-type 'darwin))
(when is-mac
  (setq-default
   mac-command-modifier 'meta
   ns-pop-up-frames nil
   ispell-program-name "/usr/local/bin/aspell"))

;;; Xorg
(when window-system
  (tool-bar-mode -1)      ; remove tool bar
  (scroll-bar-mode -1)    ; remove scroll bar
  (menu-bar-mode -1)      ; remove menu bar
  (visual-line-mode t)    ; word wrap break on whitespace
  (set-frame-font (if is-mac "Ubuntu Mono-14" "Ubuntu Mono-10.5")))

;;;; Mode-Specific ;;;;

;;; text-mode
(add-hook 'fundamental-mode-hook 'flyspell-mode)      ; spellcheck text
(add-hook 'fundamental-mode-hook 'turn-on-auto-fill)    ; autofill text

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
    ("Jython" (mode . jython-mode))
    ("Clojure" (mode . clojure-mode))
    ("Markup" (mode . sgml-mode))
    ("HTML" (mode . html-mode))
    ("CSS" (mode . css-mode))
    ("C++" (mode . c++-mode)))))
(add-hook
 'ibuffer-mode-hook
 (lambda ()
   (ido-mode t)
   (ibuffer-switch-to-saved-filter-groups "default")))


;;;; Requires and Packages ;;;;

;;; packages
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; install packages
(let ((ensure-installed
       (lambda (name)
         (unless (package-installed-p name) (package-install name))))
      (packages '(ac-js2, auto-complete, exec-path-from-shell, expand-region,
                  flymake-easy, flymake-hlint, ghc, ghci-completion, haskell-mode,
                  js2-mode, multiple-cursors, rainbow-delimiters, rainbow-mode,
                  skewer-mode, solarized-theme, visual-regexp, yasnippet,
                  zencoding-mode)))
  (mapc ensure-installed packages))

;;; requires
(require 'c-init)             ; c specific elisp
(require 'move-line)          ; move line up or down
(require 'uniquify)           ; unique buffer names with dirs
(require 'auto-complete-config)

;;; auto-config-mode
(ac-config-default)

;;; terminal
(global-set-key (kbd "C-c s") 'eshell) ; start shell
(exec-path-from-shell-initialize)
(eshell)
(add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "emacs")))

;;; uniquify
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":")

;;; color-theme
(setq-default
 custom-safe-themes
 '("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
   "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
(load-theme 'solarized-light)

;;; yasnippets
(setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt)) ; use ido for multiple snippets
(setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode t)

;;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook '(ac-js2-mode t))
(setq-default ac-js2-evaluate-calls t)

;;; html-mode
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . html-mode))
(add-hook
 'html-mode-hook
 (lambda ()
   (local-set-key (kbd "C-c t") 'mc/mark-sgml-tag-pair)
   (zencoding-mode)
   (rainbow-mode)))

;;; css-mode
(add-hook 'css-mode-hook 'rainbow-mode)

;;; coding-modes map
(mapc
 (lambda (x)
   (add-hook x
             (lambda ()
               (linum-mode t)
               (rainbow-delimiters-mode t)
               (auto-complete-mode t))))
 '(text-mode-hook
   c-mode-common-hook
   python-mode-hook
   haskell-mode-hook
   js2-mode-hook
   html-mode-hook
   css-mode-hook
   clojure-mode-hook
   emacs-lisp-mode-hook))

;;; haskell-mode
(autoload 'ghc-init "ghc" nil t)
(add-hook
 'haskell-mode-hook
 (lambda ()
   (ghc-init)
   (flymake-mode t)
   (capitalized-words-mode t)
   (turn-on-haskell-indent)
   (turn-on-haskell-doc-mode)
   (turn-on-haskell-decl-scan)
   (imenu-add-menubar-index)
   (local-set-key (kbd "C-c i") 'haskell-navigate-imports) ; go to imports. prefix to return
   (setq
    ghc-ghc-options '("-isrc")
    haskell-font-lock-haddock t
    haskell-stylish-on-save t
    haskell-indent-offset 4
    whitespace-line-column 78)))

;;; ghci-mode
(add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion)

;;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)

;;; flymake-mode
(add-hook
 'flymake-mode-hook
 (lambda ()
   (local-set-key (kbd "C-1") 'flymake-display-err-menu-for-current-line)
   (local-set-key (kbd "C-.") 'flymake-goto-next-error)
   (local-set-key (kbd "C-,") 'flymake-goto-prev-error)
   ))

;;; multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;; visual-regexp
(global-set-key (kbd "C-M-%") 'vr/query-replace)
(global-set-key (kbd "M-%") 'vr/replace)
