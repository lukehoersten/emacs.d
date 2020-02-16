;;; init --- Summary: Luke Hoersten <Luke@Hoersten.org> personal init file

;;; Commentary:
;;; ~/.emacs.d/init.el (~/.emacs)

;;; Code:
(add-to-list 'load-path "~/.emacs.d/elisp")   ; set default emacs load path

(setq-default
 gc-cons-threshold 20000000                   ; gc every 20 MB allocated (form flx-ido docs)
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

(fset 'yes-or-no-p 'y-or-n-p)                 ; replace yes/no prompts with y/n
(windmove-default-keybindings)                ; move between windows with shift-arrow

(column-number-mode t)                        ; show column numbers
(delete-selection-mode t)                     ; replace highlighted text
(which-function-mode t)                       ; function name at point in mode line
(transient-mark-mode t)                       ; highlight selection between point and mark
(electric-pair-mode t)                        ; automatically close opening characters
(global-font-lock-mode t)                     ; syntax highlighting
(global-subword-mode t)                       ; move by camelCase words
(global-hl-line-mode t)                       ; highlight current line
(global-set-key (kbd "C-c c") 'compile)       ; compile
(global-set-key (kbd "C-c r") 'recompile)     ; recompile
(global-set-key (kbd "C-c a") 'align-regexp)  ; align
(global-set-key (kbd "C-c g") 'rg)            ; rg


;;; ediff
(setq-default
  ediff-split-window-function 'split-window-horizontally
  ediff-window-setup-function 'ediff-setup-windows-plain)


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
  (tool-bar-mode -1)                          ; remove tool bar
  (scroll-bar-mode -1)                        ; remove scroll bar
  (unless is-mac (menu-bar-mode -1))          ; remove menu bar
  (set-frame-font "Inconsolata-12" nil t))


;;;; Packages ;;;;
(package-initialize)
(require 'package-require)
(package-require '(rg company exec-path-from-shell expand-region flx-ido
 smex markdown-mode markdown-mode+ hgignore-mode move-text paredit
 rainbow-delimiters json-mode json-reformat flycheck
 solarized-theme terraform-mode visual-regexp yasnippet yaml-mode
 zencoding-mode))


;;; custom requires
(require 'haskell-intero-init)
(require 'javascript-init)
(require 'c-init)
(require 'ansible-init)


;;; text-mode
(add-hook 'fundamental-mode-hook 'flyspell-mode)      ; spellcheck text
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
(require 'ibuffer)
(require 'ibuf-ext)
(defun ibuffer-generate-filter-groups-by-major-mode ()
  (flet
      ((mode-group
        (mode)
        (let ((mode-title
               (capitalize (car (split-string (symbol-name mode) "-" t)))))
          (cons mode-title `((mode . ,mode)))))
       (buffer-modes
        ()
        (flet ((buffer-mode (buffer) (buffer-local-value 'major-mode buffer)))
          (ibuffer-remove-duplicates (remq nil (mapcar 'buffer-mode (buffer-list)))))))
    (mapcar 'mode-group (buffer-modes))))

(defun ibuffer-major-mode-group-hook ()
  (interactive)
  (setq ibuffer-filter-groups (ibuffer-generate-filter-groups-by-major-mode))
  (ibuffer-update nil t)
  (message "ibuffer-major-mode: groups set"))

(setq-default ibuffer-show-empty-filter-groups nil)
(add-hook 'ibuffer-hook (lambda () (ibuffer-major-mode-group-hook)))


;;; shell
(global-set-key (kbd "C-c s") 'eshell)  ; start shell
(exec-path-from-shell-copy-env "PYTHONPATH")
(exec-path-from-shell-initialize)
(eshell)
(setq tramp-default-method "ssh")
(add-hook 'eshell-mode-hook
          (lambda ()
            (setenv "TERM" "emacs")
            (setenv "PAGER" "cat")))

;;; ido / smex / completion
(setq-default
 ido-enable-flex-matching t                           ; fuzzy matching for ido mode
 ido-create-new-buffer 'always                        ; create new buffer without prompt
 ido-max-window-height 1                              ; max ido window height
 ido-everywhere t                                     ; use ido where possible
 ido-use-faces nil)
(ido-mode t)                                          ; file/buffer selector
(flx-ido-mode t)
(global-set-key (kbd "M-/") 'completion-at-point)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;;; emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)


;;; company-mode
(global-company-mode t)
(global-set-key (kbd "M-/") 'company-complete)
(setq-default
 company-idle-delay nil
 company-minimum-prefix-length 2
 company-selection-wrap-around t
 company-show-numbers t
 company-tooltip-align-annotations t)


;;; flycheck-mode
(global-flycheck-mode t)


;;; uniquify
(require 'uniquify)                     ; unique buffer names with dirs
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":")


;;; color-theme
(setq-default
 custom-safe-themes
 '("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
   "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
(when window-system (load-theme 'solarized-light))


;;; show-paren-mode - needs to be loaded after theme
(setq-default
 show-paren-style 'expression
 show-paren-delay 0)
(set-face-attribute
 'show-paren-match nil
 :background (face-background 'highlight)
 :foreground (face-foreground 'highlight))
(show-paren-mode t)


;;; yasnippets
(with-eval-after-load 'yasnippet
  (setq yas-snippet-dirs (remq 'yas-installed-snippets-dir yas-snippet-dirs)))
(setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt)) ; use ido for multiple snippets
(yas-global-mode t)


;;; markdown-mode
(add-hook 'markdown-mode-hook 'flyspell-mode)
(setq-default markdown-command "pandoc -f gfm")


;;; html-mode
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . html-mode))
(add-hook 'html-mode-hook 'zencoding-mode)


;;; color-modes map
(mapc
 (lambda (x)
   (add-hook x
    (lambda ()
      (linum-mode t)
      (rainbow-delimiters-mode t))))
 '(text-mode-hook
   c-mode-common-hook
   python-mode-hook
   haskell-mode-hook
   js2-mode-hook
   html-mode-hook
   css-mode-hook
   sass-mode-hook
   clojure-mode-hook
   emacs-lisp-mode-hook
   conf-mode-hook
   yaml-mode-hook
   sql-mode-hook))


;;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)


;;; move-text
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)


;;; visual-regexp
(global-set-key (kbd "C-M-%") 'vr/query-replace)
(global-set-key (kbd "M-%") 'vr/replace)

(provide 'init)
;;; init.el ends here
