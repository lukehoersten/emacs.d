;; ~/.emacs.d/hoersten-pastebin-region.el
;; Luke Hoersten <Luke@Hoersten.org>

;; custom keys
(global-set-key (kbd "C-c w") 'pastebin-region)

;; Based on http://www.emacswiki.org/cgi-bin/wiki/download/pastebin.el
(defvar pastebin-type-assoc
  '((emacs-lisp-mode . "common-lisp")
    (c-mode          . "c")
    (python-mode     . "python")
    (nxml-mode       . "xml")
    (c++-mode        . "cpp")))

(defun pastebin-region (start end)
  "Send selected text to dpaste pastebin."
  (interactive "r")
  (let* ((pastebin-url "http://inf/paste/")
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data
          (format "title=%s&content=%s&lexer=%s&author=%s"
                  (url-hexify-string (buffer-file-name))                                         ; title
                  (url-hexify-string (buffer-substring-no-properties start end))                 ; content
                  (url-hexify-string (or (assoc-default major-mode pastebin-type-assoc) "text")) ; lexer
                  (url-hexify-string (user-full-name)))))                                        ; author
    (url-retrieve pastebin-url (lambda (arg)
                                 (cond
                                  ((equal :error (car arg))
                                   (signal (cdr arg)))
                                  ((equal :redirect (car arg))
                                   (let ((redirected (cadr arg)))
                                     (message redirected)
                                     (with-temp-buffer
                                       (insert redirected)
                                       (clipboard-kill-ring-save (point-min) (point-max))))))))))

(provide 'hoersten-pastebin-region)
