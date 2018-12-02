;;;; Initialization
(require 'package)
(add-to-list 'package-archives
             '("MELPA Stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)


;;; Variables

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-program-args nil)
 '(blink-cursor-mode nil)
 '(echo-keystrokes 1e-10)
 '(package-selected-packages
   (quote
    (ssh-agency dash yaml-mode restart-emacs markdown-mode magit helm haml-mode form-feed dashboard))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


;;;; Agda

(add-to-list 'load-path                   "~/.config/emacs/libs/haskell-mode/")
(add-to-list 'Info-default-directory-list "~/.config/emacs/libs/haskell-mode/")
(add-to-list 'load-path                   "~/.config/emacs/libs/agda-mode/")
(add-to-list 'Info-default-directory-list "~/.config/emacs/libs/agda-mode/")

;; (load-file (let ((coding-system-for-read 'utf-8))
;;      (shell-command-to-string "agda-mode locate")))

(require 'agda2-mode)


;;;; Captain Hook

(add-hook 'text-mode-hook 'recentf-mode)
(add-hook 'text-mode-hook 'column-number-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'form-feed-mode)
;; I think this breaks e.g. the color-picker
; (add-hook 'text-mode-hook 'form-feed-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;; global-company-mode keeps recentering the point on the screen for
;; some reason
; (add-hook 'after-init-hook 'global-company-mode)
;; (add-hook 'after-init-hook 'flycheck-mode)

;; ;; (require 'haskell-unicode-input-method)


;;;; Flycheck

(global-set-key (kbd "C-c f") 'flyckeck-next-error)
(global-set-key (kbd "C-c b") 'flycheck-prev-error)


;;;; Dashboard

(dashboard-setup-startup-hook)
;; (setq dashboard-startup-banner 'logo)


;; ;; (defun scroll-down-half ()
;; ;;   (interactive)
;; ;;   (move-to-window-line nil)
;; ;;   (recenter-top-bottom 0))
;; ;; (defun scroll-up-half ()
;; ;;   (interactive)
;; ;;   (move-to-window-line nil)
;; ;;   (recenter-top-bottom -1))

;; (global-set-key (kbd "C-S-v") 'scroll-down-half)
;; (global-set-key (kbd "C-M-v") 'scroll-up-half)
(setenv "MANWIDTH" "72")
(setq-default indent-tabs-mode nil)
(put 'downcase-region 'disabled nil)

(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

(global-unset-key (kbd "C-z"))

(require 'haskell-unicode-input-method)
(add-hook 'haskell-mode-hook
  (lambda () (set-input-method "haskell-unicode")))

(global-set-key (kbd "C-x g") 'magit-status)

(show-paren-mode)

(fset 'haskell/def
   [M-backspace ?\C-y ?  ?: ?: ?  ?_ return ?\C-y ?  ?= ?  ?_ ?\C-y])

;; (global-set-key (kbd "C-x C-e") 'magit-status)
(setq x-select-enable-clipboard t)

(require 'haskell-mode-autoloads)
(delete-selection-mode 1)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(setq dashboard-startup-banner "/home/fredefox/.local/share/emacs/fredefox.svg")
(setq dashboard-items '((recents  . 40)))
