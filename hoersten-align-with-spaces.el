;; ~/.emacs.d/hoersten-align-with-spaces.el
;; Luke Hoersten <Luke@Hoersten.org>

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

(message "Loaded Hoersten align-with-spaces function")
(provide 'hoersten-align-with-spaces)
