;;; ruby-trace-mode --- Syntax highlighting for ruby traces -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:

(provide 'ruby-trace-mode)

(defvar ruby-trace-highlights
  '(("\\<\\(in\\|at\\)\\>" . font-lock-keyword-face)
    ("#[0-9]+" . font-lock-constant-face)))

(define-derived-mode ruby-trace-mode prog-mode "ruby-trace"
  "Minor mode for syntax highlighting ruby traces."
  (setq font-lock-defaults '(ruby-trace-highlights)))

;; Hi-lock: (("\\bat\\b" (0 (quote font-lock-keyword-face) prepend)))
;; Hi-lock: (("\\bin\\b" (0 (quote font-lock-keyword-face) prepend)))
;; Hi-lock: (("#[0-9]+" (0 (quote font-lock-type-face) prepend)))

;;; ruby-trace-mode.el ends here
