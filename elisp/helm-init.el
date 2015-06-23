(require 'package-require)
(package-require '(helm company helm-company flycheck helm-flycheck helm-flyspell helm-ls-hg helm-package))

;;; helm
(require 'helm-config)
;; (helm-mode t)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x f") 'helm-browse-project)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(setq-default
 helm-buffers-fuzzy-matching t
 helm-M-x-fuzzy-match t)

;;;; helm company
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "M-/") 'helm-company)
     (define-key company-active-map (kbd "M-/") 'helm-company)))

;;;; helm flycheck
 (eval-after-load 'flycheck
   '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

;;;; helm flyspell
;; (define-key flyspell-mode-map (kbd "C-;") 'helm-flyspell-correct)

;;;; helm eshell
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map
                [remap eshell-pcomplete]
                'helm-esh-pcomplete)
              (define-key eshell-mode-map
                (kbd "M-p")
                'helm-eshell-history)))
