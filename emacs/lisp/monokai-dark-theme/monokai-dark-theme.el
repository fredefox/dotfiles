;;; monokai-dark-theme --- Summary:
;;;
;;; A bare-bones version of the monokai-dark theme.
;;;
;;; Commentary:
;;; Code:
;; (provide 'monokai-dark-theme)
(deftheme monokai-dark
  "Created 2019-03-28 by fredefox.")

(custom-theme-set-variables
 'monokai-dark
 '(agda2-highlight-face-groups (quote default-faces)))

(require 'color)

(custom-theme-set-faces
 'monokai-dark
 '(default ((t (:inherit nil :stipple nil :background "#292b2e" :foreground "#e8e8e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :family "Source Code Pro"))))
 '(fringe ((t (:inherit \1))))
 '(custom-group-tag ((t (:inherit variable-pitch :foreground "light blue" :weight bold :height 1))))
 '(custom-group-tag-1 ((t (:inherit variable-pitch :foreground "pink" :weight bold :height 1))))
 '(shadow ((t (:foreground "grey70"))))
 '(variable-pitch ((t nil)))
 '(whitespace-trailing ((t (:background "#542b2e"))))
 '(company-scrollbar-bg ((t (:inherit mode-line-inactive))))
 '(company-scrollbar-fg ((t (:inherit mode-line))))
 '(company-tooltip ((t (:inherit default))))
 '(company-tooltip-common ((t (:inherit shadow))))
 '(company-tooltip-selection ((t (:inherit highlight))))
 '(custom-group-tag ((t (:inherit variable-pitch :foreground "light blue" :weight bold :height 1.0))))
 '(company-preview ((t (:inherit shadow))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-preview-search ((t (:inherit company-preview)))))

(provide-theme 'monokai-dark)
;;; monokai-dark-theme.el ends here
