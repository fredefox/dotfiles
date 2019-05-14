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
 '(fringe ((t (:inherit \1))))
 '(custom-group-tag ((t (:inherit variable-pitch))))
 '(custom-group-tag-1 ((t (:inherit variable-pitch))))
 '(company-scrollbar-bg ((t (:inherit mode-line-inactive))))
 '(company-scrollbar-fg ((t (:inherit mode-line))))
 '(company-tooltip ((t (:inherit default))))
 '(company-tooltip-common ((t (:inherit shadow))))
 '(company-tooltip-selection ((t (:inherit highlight))))
 '(company-preview ((t (:inherit shadow))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-preview-search ((t (:inherit company-preview))))
 '(ido-indicator ((t (:inherit highlight :width condensed))))
 '(ido-only-match ((t (:inherit highlight))))
 '(ido-subdir ((t (:inherit shadow))))
 '(js2-error ((t (:inherit error))))
 '(js2-external-variable ((t (:inherit font-lock-builtin-face))))
 '(js2-function-param ((t (:inherit font-lock-variable-name-face))))
 '(js2-instance-member ((t (:inherit font-lock-variable-name-face))))
 '(js2-jsdoc-html-tag-delimiter ((t (:inherit basic))))
 '(js2-jsdoc-type ((t (:inherit font-lock-type-face))))
 '(js2-warning ((t (:inherit warning)))))

(provide-theme 'inheritance)
;;; inheritance-theme.el ends here
