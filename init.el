;; ~/.emacs.d/init.el (~/.emacs)
;; Luke Hoersten <Luke@Hoersten.org>

;;;; General ;;;;
(add-to-list 'load-path "~/.emacs.d/lisp")     ; set default emacs load path

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
   ring-bell-function 'ignore
   mac-command-modifier 'meta
   ns-pop-up-frames nil
   ispell-program-name "/usr/local/bin/aspell"))

;;; Xorg
(when window-system
  (tool-bar-mode -1)      ; remove tool bar
  (scroll-bar-mode -1)    ; remove scroll bar
  (unless is-mac (menu-bar-mode -1)) ; remove menu bar
  (visual-line-mode t)    ; word wrap break on whitespace
  (set-frame-font (if is-mac "Ubuntu Mono-12" "Ubuntu Mono-10.5")))

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
      (packages '(ac-js2 auto-complete exec-path-from-shell expand-region
                  ghc ghci-completion haskell-mode
                  js2-mode multiple-cursors rainbow-delimiters rainbow-mode
                  skewer-mode solarized-theme visual-regexp yasnippet
                  zencoding-mode json-mode markdown-mode)))
  (mapc ensure-installed packages))

;;; requires
(require 'c-init)             ; c specific elisp
(require 'move-line)          ; move line up or down
(require 'uniquify)           ; unique buffer names with dirs
(require 'auto-complete-config)

;;; auto-complete-mode
(ac-config-default)
(global-set-key (kbd "M-/") 'auto-complete)
(setq-default ac-auto-start nil)

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

;;; gradle-mode
(add-to-list 'auto-mode-alist '("\\.gradle$" . groovy-mode))

;;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook 'ac-js2-mode)
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
   (capitalized-words-mode t)
   (turn-on-hi2)
   (imenu-add-menubar-index)
   (interactive-haskell-mode)
   (local-set-key (kbd "C-c i") 'haskell-navigate-imports) ; go to imports. prefix to return
   (local-set-key (kbd "M-p") 'move-line-up) ; need to override default M-p function
   (local-set-key (kbd "M-n") 'move-line-down)
   (local-set-key (kbd "C-1") 'ghc-display-errors)
   (local-set-key (kbd "C-.") 'ghc-goto-next-error)
   (local-set-key (kbd "C-,") 'ghc-goto-prev-error)

   (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
   (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
   (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
   (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
   (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
   (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
   (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
   (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

   (setq
    ghc-ghc-options '("-isrc")
    haskell-program-name "cabal repl"
    haskell-stylish-on-save t
    hi2-layout-offset 4
    hi2-left-offset 4
    whitespace-line-column 78
    ;; haskell-process-type 'cabal-repl
    haskell-process-suggest-remove-import-lines t
    haskell-process-auto-import-loaded-modules t
    haskell-process-log t
    )))

;;; ghci-mode
(add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion)

;;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)

;;; move-line
(global-set-key (kbd "M-p") 'move-line-up)
(global-set-key (kbd "M-n") 'move-line-down)

;;; multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;; visual-regexp
(global-set-key (kbd "C-M-%") 'vr/query-replace)
(global-set-key (kbd "M-%") 'vr/replace)
