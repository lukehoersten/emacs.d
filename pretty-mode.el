;;; -*- coding: utf-8 -*-
;; Minor mode for redisplaying parts of the buffer as pretty symbols
;; originally modified from Trent Buck's version at http://paste.lisp.org/display/42335,2/raw
;; Also includes code from `sml-mode'
;; See also http://www.emacswiki.org/cgi-bin/wiki/PrettyLambda
;; written by Arthur Danskin <arthurdanskin@gmail.com>
;;
;; to install:
;; (require 'pretty-mode)
;; (global-pretty-mode 1)
;; or
;; (add-hook 'my-pretty-language-hook 'turn-on-pretty-mode)


(require 'cl)

;; modified from `sml-mode'
(defun pretty-font-lock-compose-symbol (alist)
  "Compose a sequence of ascii chars into a symbol."
  (let* ((start (match-beginning 0))
         (end (match-end 0))
         (syntax (char-syntax (char-after start))))
    (if (or (if (eq syntax ?w)
                (or (eq (char-syntax (char-before start)) ?w)
                    (eq (char-syntax (char-after end)) ?w))
              (memq (char-syntax (char-before start)) '(?. ?\\)))
            (memq (get-text-property start 'face)
                  '(font-lock-doc-face font-lock-string-face
                                       font-lock-comment-face)))
        (remove-text-properties start end '(composition))
      (compose-region start end (cdr (assoc (match-string 0) alist)))
      ))
  nil)

(defvar pretty-interaction-mode-alist
  '((inferior-scheme-mode . scheme-mode)
    (lisp-interaction-mode . emacs-lisp-mode)
    (inferior-lisp-mode . lisp-mode)
    (inferior-ess-mode . ess-mode)
    (inf-haskell-mode . haskell-mode)
    (inferior-erlang-mode . erlang-mode)
    (tuareg-interactive-mode . tuareg-mode)
    (inferior-python-mode . python-mode)
    (inferior-octave-mode . octave-mode)
    (inferior-ruby-mode . ruby-mode))
  "Alist mapping from inferior process interaction modes to their
  corresponding script editing modes.")


(defun pretty-font-lock-keywords (alist)
  "Return a `font-lock-keywords' style entry for replacing
regular expressions with symbols. ALIST has the form ((STRING .
REPLACE-CHAR) ...)."
  (when alist
    `((,(regexp-opt (mapcar 'car alist))
       (0 (pretty-font-lock-compose-symbol
           ',alist))))))

(defun pretty-keywords (&optional mode)
  "Return the font-lock keywords for MODE, or the current mode if
MODE is nil. Return nil if there are no keywords."
  (let* ((mode (or mode major-mode))
         (kwds (cdr-safe
                (or (assoc mode pretty-patterns)
                    (assoc (cdr-safe
                            (assoc mode pretty-interaction-mode-alist))
                           pretty-patterns)))))
    (pretty-font-lock-keywords kwds)))

(defgroup pretty nil "Minor mode for replacing text with symbols "
  :group 'faces)

(define-minor-mode pretty-mode
  "Toggle Pretty minor mode.
With arg, turn Pretty minor mode on if arg is positive, off otherwise.

Pretty mode builds on `font-lock-mode'. Instead of highlighting
keywords, it replaces them with symbols. For example, lambda is
displayed as λ in lisp modes."
  :group 'pretty
                                        ;  :lighter " λ"
  (if pretty-mode
      (progn
        (font-lock-add-keywords nil (pretty-keywords) t)
        (font-lock-fontify-buffer))
    (font-lock-remove-keywords nil (pretty-keywords))
    (remove-text-properties (point-min) (point-max) '(composition nil))))

(defun turn-on-pretty-if-desired ()
  "Turn on `pretty-mode' if the current major mode supports it."
  (if (pretty-keywords)
      (pretty-mode 1)))

(define-globalized-minor-mode global-pretty-mode
  pretty-mode turn-on-pretty-if-desired
  :init-value t)

(defun turn-off-pretty-mode ()
  (interactive)
  (pretty-mode -1))


(defun turn-on-pretty-mode ()
  (interactive)
  (pretty-mode +1))

(defun pretty-compile-patterns (patterns)
  "Set pretty patterns in a convenient way.

PATTERNS should be of the form ((GLYPH (REGEXP MODE ...) ...)
...). GLYPH should be a character. MODE should be the name of a
major mode without the \"-mode\". Returns patterns in the form
expected by `pretty-patterns'"
  (let ((pretty-patterns))
    (loop for (glyph . pairs) in patterns do
          (loop for (regexp . major-modes) in pairs do
                (loop for mode in major-modes do
                      (let* ((mode (intern (concat (symbol-name mode)
                                                   "-mode")))
                             (assoc-pair (assoc mode pretty-patterns))

                             (entry (cons regexp glyph)))
                        (if assoc-pair
                            (push entry (cdr assoc-pair))
                          (push (cons mode (list entry))
                                pretty-patterns))))))
    pretty-patterns))

;;; (setq-default pretty-patterns pretty-patterns-default)
(defconst pretty-patterns-default
  (let* ((lispy '(scheme emacs-lisp lisp))
         (mley '(tuareg haskell sml erlang))
         (c-like '(c c++ perl sh python java ess ruby))
         (all (append lispy mley c-like (list 'octave))))
    (pretty-compile-patterns
     `(
       (?≠ ("!=" ,@c-like scheme octave)
           ("<>" tuareg octave)
           ("~=" octave)
           ("/=" haskell)
           ("=/=" erlang))
       (?≤ ("<=" ,@all))
       (?≤ ("=<" erlang))
       (?λ ("fun" erlang))
       (?≥ (">=" ,@all))
       (?← ("<-" ,@mley ess)
           ("!" erlang))
       (?→ ("->" ,@mley ess c c++ perl))
       (?↑ ("\\^" tuareg))
       (?⇒ ("=>" sml perl ruby))
;;;        (?∅ ("nil" emacs-lisp ruby)
;;;            ("null" scheme java)
;;;            ("NULL" c c++)
;;;            ("None" python)
;;;            ("()" ,@mley))
       (?≡ ("==" ,@c-like erlang haskell))
       (?∀ ("BOOST_FOREACH" c++))
       (?∷ ("::" ,@all))
       (?√ ("sqrt" ,@all))
       (?∑ ("sum" python))
       (?α ("alpha" ,@all))
       (?β ("beta" ,@all))
       (?γ ("gamma" ,@all))
       (?δ ("delta" ,@all))
       (?ε ("epsilon" ,@all))
       (?ζ ("zeta" ,@all))
       (?η ("eta" ,@all))
       (?θ ("theta" ,@all))
       (?ι ("iota" ,@all))
       (?κ ("kappa" ,@all))
       (?λ ("lambda" ,@all))
       (?μ ("mu" ,@all))
       (?ν ("nu" ,@all))
       (?ν ("vega" ,@all))
       (?ξ ("xi" ,@all))
       (?ο ("omicron" ,@all))
       (?π ("pi" ,@all))
       (?ρ ("rho" ,@all))
       (?σ ("sigma" ,@all))
       (?τ ("tau" ,@all))
       (?υ ("upsilon" ,@all))
       (?φ ("phi" ,@all))
       (?χ ("chi" ,@all))
       (?ψ ("psi" ,@all))
       (?ω ("omega" ,@all))
       (?² ("**2" python tuareg octave))
       (?³ ("**3" python tuareg octave))
       (?ⁿ ("**n" python tuareg octave))
       (?ₐ ("[a]" ,@c-like))
       (?ₓ ("[x]" ,@c-like))
       (?₀ ("[0]" ,@c-like)
           ("/0" erlang))
       (?₁ ("[1]" ,@c-like)
           ("/1" erlang))
       (?₂ ("[2]" ,@c-like)
           ("/2" erlang))
       (?₃ ("[3]" ,@c-like)
           ("/3" erlang))
       (?₄ ("[4]" ,@c-like)
           ("/4" erlang))
       (?₅ ("[5]" ,@c-like)
           ("/5" erlang))
       (?₆ ("[6]" ,@c-like)
           ("/6" erlang))
       (?₇ ("[7]" ,@c-like)
           ("/7" erlang))
       (?₈ ("[8]" ,@c-like)
           ("/8" erlang))
       (?₉ ("[9]" ,@c-like)
           ("/9" erlang))
       (?∧ ("\\<And\\>"     emacs-lisp lisp python)
           ("\\<andalso\\>" sml erlang)
           ("&&"            c c++ perl haskell))
       (?∨ ("\\<or\\>"      emacs-lisp lisp)
           ("\\<orelse\\>"  sml erlang)
           ("||"            c c++ perl haskell erlang))
       (?¬ ("!"       c c++))
       )))
  "default value for `pretty-patterns'")

;; TODO fix type
(defcustom pretty-patterns pretty-patterns-default
  "*List of pretty patterns.

Should be a list of the form ((MODE ((REGEXP . GLYPH) ...)) ...)"
  :group 'pretty
  :type '(alist :key-type variable :value-type (alist :key-type (string) :value-type (character))))


(defun pretty-add-keywords (mode keywords)
  "Add pretty character KEYWORDS to MODE

MODE should be a symbol, the major mode command name, such as
`c-mode' or nil. If nil, pretty keywords are added to the current
buffer. KEYWORDS should be a list where each element has the
form (REGEXP . CHAR). REGEXP will be replaced with CHAR in the
relevant buffer(s)."
  (font-lock-add-keywords
   mode (mapcar (lambda (kw) `((,(car kw)
                                (0 (prog1 nil
                                     (compose-region (match-beginning 0)
                                                     (match-end 0)
                                                     ,(cdr kw)))))))
                keywords)))

(defun pretty-regexp (regexp glyph)
  "Replace REGEXP with GLYPH in buffer."
  (interactive "MRegexp to replace:
MCharacter to replace with: ")
  (pretty-add-keywords nil `((,regexp . ,(string-to-char glyph))))
  (font-lock-fontify-buffer))




(provide 'pretty-mode)
