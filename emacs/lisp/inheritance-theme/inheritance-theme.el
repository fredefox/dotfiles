;;; inheritance-dark-theme --- Summary:
;;;
;;; A bare-bones version of the monokai-dark theme.
;;;
;;; Commentary:
;;; Code:
(deftheme inheritance
  "Theme to make various packages use inherited values for
  styling from other packages.")

(custom-theme-set-variables
 'inheritance
 '(agda2-highlight-face-groups (quote default-faces)))

(require 'color)

(custom-theme-set-faces
 'inheritance
 '(agda2-highlight-confluence-problem-face ((t (:inherit flycheck-error))))
 '(agda2-highlight-keyword-face ((t (:inherit font-lock-keyword-face))))
 '(company-preview ((t (:inherit shadow))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-preview-search ((t (:inherit company-preview))))
 '(company-scrollbar-bg ((t (:inherit mode-line-inactive))))
 '(company-scrollbar-fg ((t (:inherit mode-line))))
 '(company-tooltip ((t (:inherit default))))
 '(company-tooltip-common ((t (:inherit shadow))))
 '(company-tooltip-selection ((t (:inherit highlight))))
 '(custom-group-tag ((t (:inherit variable-pitch))))
 '(custom-group-tag-1 ((t (:inherit variable-pitch))))
 '(ediff-even-diff-A ((t (:inherit lazy))))
 '(ediff-even-diff-B ((t (:inherit lazy))))
 '(ediff-even-diff-C ((t (:inherit lazy))))
 '(ediff-odd-diff-A ((t (:inherit lazy-highlight))))
 '(ediff-odd-diff-B ((t (:inherit lazy-highlight))))
 '(ediff-odd-diff-C ((t (:inherit lazy-highlight))))
 '(fringe ((t (:inherit \1))))
 '(ido-indicator ((t (:inherit highlight :width condensed))))
 '(ido-only-match ((t (:inherit highlight))))
 '(ido-subdir ((t (:inherit default))))
 '(js2-error ((t (:inherit error))))
 '(js2-external-variable ((t (:inherit font-lock-builtin-face))))
 '(js2-function-param ((t (:inherit font-lock-variable-name-face))))
 '(js2-instance-member ((t (:inherit font-lock-variable-name-face))))
 '(js2-jsdoc-html-tag-delimiter ((t (:inherit basic))))
 '(js2-jsdoc-type ((t (:inherit font-lock-type-face))))
 ; The built-in warning is not appropriate here.  It's too strong.
 '(js2-warning ((t (:inherit flycheck-warning))))
 '(magit-bisect-bad ((t (:foreground "red"))))
 '(magit-branch-upstream ((t (:inherit magit-branch-remote :slant italic))))
 '(show-paren-match-expression ((t (:inherit lazy-highlight))))
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "#338033"))))
 '(smerge-refined-removed ((t (:inherit smerge-refined-change :background "#803333")))))

(provide-theme 'inheritance)
;;; inheritance-theme.el ends here
