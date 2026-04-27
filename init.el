;;; init --- Summary: Luke Hoersten <Luke@Hoersten.org> personal init file  -*- lexical-binding: t; -*-

;;; Commentary:
;;; ~/.emacs.d/init.el (~/.emacs)

;;; Code:
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

(setq custom-file (concat user-emacs-directory "custom.el")) ; don't auto-edit init.el

(setq-default
 gc-cons-threshold 20000000                   ; gc every 20 MB allocated
 inhibit-splash-screen t                      ; disable splash screen
 truncate-lines t                             ; truncate, not wrap, lines
 indent-tabs-mode nil                         ; only uses spaces for indentation
 split-width-threshold 181                    ; min width to split window horizontially
 split-height-threshold 120                   ; min width to split window vertically
 reb-re-syntax 'string                        ; use string syntax for regexp builder
 fill-column 120                              ; line width
 require-final-newline 'visit-save)           ; add a newline automatically

(put 'set-goal-column 'disabled nil)          ; enable goal column setting
(put 'narrow-to-region 'disabled nil)         ; enable hiding
(put 'narrow-to-page 'disabled nil)

(setq use-short-answers t)                    ; replace yes/no prompts with y/n
(windmove-default-keybindings)                ; move between windows with shift-arrow

(column-number-mode t)                        ; show column numbers
(delete-selection-mode t)                     ; replace highlighted text
(which-function-mode t)                       ; function name at point in mode line
(electric-pair-mode t)                        ; automatically close opening characters
(global-subword-mode t)                       ; move by camelCase words
(global-hl-line-mode t)                       ; highlight current line
(global-auto-revert-mode t)                   ; auto-reload files when changed on disk
(global-set-key (kbd "C-c b") 'browse-url-at-point) ; open URL at point
(global-set-key (kbd "C-c c") 'compile)       ; compile
(global-set-key (kbd "C-c r") 'recompile)     ; recompile
(global-set-key (kbd "C-c a") 'align-regexp)  ; align
(global-set-key (kbd "C-c g") 'consult-ripgrep) ; ripgrep with preview


;;; ediff
(setq-default
  ediff-split-window-function 'split-window-horizontally
  ediff-window-setup-function 'ediff-setup-windows-plain)


;;; MacOS
(defvar is-mac (equal system-type 'darwin))
(when is-mac
  (setq-default
   ring-bell-function 'ignore
   mac-command-modifier 'meta
   ns-pop-up-frames nil
   dired-use-ls-dired nil))                             ; macOS ls doesn't support --dired


;;; Xorg
(when window-system
  (tool-bar-mode -1)                          ; remove tool bar
  (scroll-bar-mode -1)                        ; remove scroll bar
  (unless is-mac (menu-bar-mode -1))          ; remove menu bar
  (set-frame-font "Inconsolata-12" nil t))


;;;; Packages ;;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Install packages if missing
(unless package-archive-contents
  (package-refresh-contents))

(dolist (package '(
                   ;; completion
                   corfu                 ; completion popup UI (capf-native)
                   cape                  ; completion-at-point extensions for corfu
                   vertico               ; vertical minibuffer completion
                   orderless             ; fuzzy/space-separated completion style
                   consult               ; enhanced search and navigation commands
                   embark                ; contextual actions on completion candidates
                   embark-consult        ; embark integration for consult
                   marginalia            ; annotations in completion buffers

                   ;; editing
                   expand-region         ; expand selection by semantic units
                   move-text             ; move lines up/down with M-p/M-n
                   paredit               ; structured editing for Lisp
                   visual-regexp         ; visual feedback for regexp replace

                   ;; search
                   avy                   ; jump to visible text by typing target chars
                   rg                    ; ripgrep results buffer
                   wgrep                 ; editable grep/rg results buffers

                   ;; git
                   magit                 ; git interface
                   forge                 ; GitHub/GitLab integration for magit
                   magit-todos           ; show TODOs in magit status
                   diff-hl               ; inline git diff indicators in fringe

                   ;; UI
                   rainbow-delimiters    ; colorize matching parens by depth
                   ibuffer-project       ; group ibuffer by project
                   solarized-theme       ; color theme
                   auto-dark             ; switch theme with macOS dark mode
                   helpful               ; better Help buffers with source links

                   ;; language modes
                   markdown-mode         ; markdown editing
                   terraform-mode        ; Terraform/HCL editing
                   emmet-mode            ; HTML/CSS abbreviation expansion
                   json-reformat         ; JSON pretty-printing
                   treesit-auto          ; auto-install and use tree-sitter modes
                   ansible-doc           ; Ansible module documentation lookup
                   jinja2-mode           ; Jinja2 template syntax

                   ;; tools
                   exec-path-from-shell  ; sync shell PATH into Emacs
                   jinx                  ; spell checker (libenchant)
                   ghostel               ; libghostty terminal emulator
                   vterm                 ; terminal emulator - needed for claude-code-ide
                   ))
  (unless (package-installed-p package)
    (package-install package)))


;;; custom requires
(require 'c-init)
(require 'ansible-init)

;; ;; claude-code-context
;; (add-to-list 'load-path "~/Dev/code/git/elisp/claude-code-context")
;; (require 'claude-code-context)
;; (claude-code-context-mode 1)

;;; claude-code-ide
(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu)
  :custom
  (claude-code-ide-vterm-anti-flicker t)      ; batch rapid vterm updates to reduce flicker
  (claude-code-ide-vterm-render-delay 0.005)  ; 5ms collection window for batched updates
  :config
  (claude-code-ide-emacs-tools-setup))


;;; text-mode
(add-hook 'fundamental-mode-hook 'jinx-mode)          ; spellcheck text
(add-hook 'fundamental-mode-hook 'turn-on-auto-fill)  ; autofill text


;;; whitespace-mode
(global-whitespace-mode t)                            ; show whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)      ; cleanup whitespace on exit
(setq-default
 whitespace-line-column 120                           ; column width
 whitespace-style                                     ; whitespace to highlight
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
(global-set-key (kbd "C-x C-b") 'ibuffer)             ; better buffer browser
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-project-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'project-file-relative)
              (ibuffer-do-sort-by-project-file-relative))))
(setq ibuffer-show-empty-filter-groups nil)


;;; emacs server (for emacsclient)
(require 'server)
(unless (server-running-p)
  (server-start))

;;; shell
(global-set-key (kbd "C-c s") 'eshell)  ; start shell
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "PYTHONPATH")
(eshell)
(setq-default tramp-default-method "ssh")
(add-hook 'eshell-mode-hook
          (lambda ()
            (setenv "TERM" "emacs")
            (setenv "PAGER" "cat")))


;;; vertico / orderless / consult / marginalia
(vertico-mode t)                                      ; vertical completion UI
(marginalia-mode t)                                   ; annotations in completion
(setq-default
 completion-styles '(orderless basic)                 ; orderless completion style
 completion-category-defaults nil
 completion-category-overrides '((file (styles partial-completion))))
(global-set-key (kbd "C-x b") 'consult-buffer)        ; enhanced buffer switching
(global-set-key (kbd "C-x 4 b") 'consult-buffer-other-window)
(global-set-key (kbd "C-x 5 b") 'consult-buffer-other-frame)
(global-set-key (kbd "M-y") 'consult-yank-pop)        ; enhanced yank-pop


;;; embark
(global-set-key (kbd "C-,") 'embark-act)              ; contextual actions on target at point
(global-set-key (kbd "C-.") 'embark-dwim)             ; default action on target
(with-eval-after-load 'embark-consult
  (add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode))


;;; cape (completion extensions)
(add-to-list 'completion-at-point-functions #'cape-dabbrev) ; dynamic abbrev completion
(add-to-list 'completion-at-point-functions #'cape-file)    ; file path completion


;;; which-key (built-in Emacs 30+)
(which-key-mode t)


;;; helpful (better Help buffers)
(global-set-key (kbd "C-h f") 'helpful-callable)
(global-set-key (kbd "C-h v") 'helpful-variable)
(global-set-key (kbd "C-h k") 'helpful-key)
(global-set-key (kbd "C-h x") 'helpful-command)


;;; magit
(with-eval-after-load 'magit-todos
  (magit-todos-mode t))                                   ; show TODOs in magit status


;;; diff-hl (inline git diff in fringe)
(global-diff-hl-mode t)
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)


;;; emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)


;;; corfu (completion UI)
(global-corfu-mode t)
(setq-default
 corfu-auto nil              ; manual trigger only
 corfu-cycle t)              ; wrap around candidates
(global-set-key (kbd "M-/") 'completion-at-point)


;;; flymake (linting/diagnostics)
(add-hook 'prog-mode-hook 'flymake-mode)                  ; enable linting for all programming modes


;;; eglot (LSP client)
(with-eval-after-load 'eglot
  (setq-default
   eglot-autoshutdown t                                   ; shutdown server when last buffer is killed
   eglot-send-changes-idle-time 0.5))                     ; debounce for sending changes

(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'js-ts-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(add-hook 'c-ts-mode-hook 'eglot-ensure)
(add-hook 'c++-ts-mode-hook 'eglot-ensure)
(add-hook 'yaml-ts-mode-hook 'eglot-ensure)
(add-hook 'json-ts-mode-hook 'eglot-ensure)
(add-hook 'css-ts-mode-hook 'eglot-ensure)
(add-hook 'html-ts-mode-hook 'eglot-ensure)
(add-hook 'scss-mode-hook 'eglot-ensure)
(add-hook 'haskell-mode-hook 'eglot-ensure)


;;; uniquify
(setq-default
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":")


;;; color-theme
(setq custom-safe-themes t)
(require 'solarized-theme)
(require 'auto-dark)
(setq auto-dark-allow-osascript t
      auto-dark-themes '((solarized-dark) (solarized-light)))
(auto-dark-mode 1)


;;; markdown-mode
(add-hook 'markdown-mode-hook 'jinx-mode)
(setq-default markdown-command "pandoc -f gfm")


;;; treesit-auto (automatically use tree-sitter modes and install grammars)
(require 'treesit-auto)
(setq treesit-auto-install t)
(global-treesit-auto-mode)


;;; html-mode
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . html-mode))
(add-hook 'html-mode-hook 'emmet-mode)


;;; line numbers and delimiters for all code and text
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'rainbow-delimiters-mode)


;;; avy
(global-set-key (kbd "C-:") 'avy-goto-char-2)         ; jump to visible text by 2 chars


;;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)


;;; move-text
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)


;;; project.el
(with-eval-after-load 'project
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel" ?s) t)
  (add-to-list 'project-switch-commands '(claude-code-ide "Claude" ?c) t))


(provide 'init)
;;; init.el ends here
