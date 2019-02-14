;;;; Initialization
(require 'package)
(add-to-list 'package-archives
             '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("MELPA" . "https://melpa.org/packages/") t)

(package-initialize)


;;; Variables

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-program-args nil)
 '(auth-sources (quote ("~/.authinfo" "~/.authinfo.gpg" "~/.netrc")))
 '(blink-cursor-mode nil)
 '(display-buffer-alist (quote (("*Buffer List*" display-buffer-same-window))))
 '(echo-keystrokes 1e-10)
 '(haskell-indentation-where-post-offset 0)
 '(haskell-indentation-where-pre-offset 0)
 '(haskell-tags-on-save t)
 '(js-indent-level 2)
 '(line-move-visual nil)
 '(markdown-command "pandoc -t html")
 '(package-selected-packages
   (quote
    (image+ company flycheck lsp-haskell forge frames-only-mode projectile lsp-ui lsp-mode purescript-mode markdown-mode+ ssh-agency dash yaml-mode restart-emacs markdown-mode magit helm haml-mode form-feed dashboard)))
 '(projectile-mode t nil (projectile))
 '(projectile-project-search-path (quote ("~/git/")))
 '(purescript-mode-hook (quote (turn-on-purescript-indentation)) t)
 '(sgml-basic-offset 1)
 '(temp-buffer-resize-mode nil)
 '(vc-follow-symlinks nil)
 '(window-combination-resize t))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


;;;; Agda

(add-to-list 'load-path                   "~/.config/emacs/libs/haskell-mode/")
(add-to-list 'Info-default-directory-list "~/.config/emacs/libs/haskell-mode/")
(add-to-list 'load-path                   "~/.config/emacs/libs/agda-mode/")
(add-to-list 'Info-default-directory-list "~/.config/emacs/libs/agda-mode/")
(add-to-list 'load-path                   "~/.config/emacs/libs/purescript-mode/")
(add-to-list 'Info-default-directory-list "~/.config/emacs/libs/purescript-mode/")
(add-to-list 'load-path                   "~/.config/emacs/libs/psc-ide-emacs/")
(add-to-list 'Info-default-directory-list "~/.config/emacs/libs/psc-ide-emacs/")

(require 'purescript-mode-autoloads)

(require 'psc-ide)

(add-hook 'purescript-mode-hook
  (lambda ()
    (psc-ide-mode)
    (company-mode)
    (flycheck-mode)
    (turn-on-purescript-indentation)))

;; (load-file (let ((coding-system-for-read 'utf-8))
;;              "/home/fredefox/.cabal/store/ghc-8.4.4/Agda-2.6.0-eb370edb312aa9c0898503c71027e277278a4f3b94a09bc4e57221769a05cada/share/emacs-mode/agda2.el"))

;; Temp disabled.
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
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode)


;;;; Dashboard

(dashboard-setup-startup-hook)
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
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

;; Temp. disabled
;; (require 'haskell-unicode-input-method)
;; (add-hook 'haskell-mode-hook
;;   (lambda () (set-input-method "haskell-unicode")))

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

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

;; (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(setq dashboard-startup-banner "/home/fredefox/.local/share/emacs/fredefox.svg")
(setq dashboard-items '((recents  . 40)))
