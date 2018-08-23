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
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476")))
 '(echo-keystrokes 0.001)
 '(package-selected-packages
   (quote
    (purescript-mode dash-functional dash lsp-mode yaml-mode spacemacs-theme restart-emacs markdown-mode magit idris-mode helm haskell-mode haml-mode form-feed dashboard))))

(menu-bar-mode -1)
(tool-bar-mode -1)


;;;; Fonts

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#292b2e" :foreground "#b2b2b2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 145 :width normal :foundry "ADBE" :family "Source Code Pro"))))
 '(agda2-highlight-catchall-clause-face ((t (:box (:line-width 1 :color "white smoke")))))
 '(agda2-highlight-coverage-problem-face ((t (:box (:line-width 1 :color "wheat")))))
 '(agda2-highlight-datatype-face ((t (:foreground "royal blue"))))
 '(agda2-highlight-function-face ((t (:foreground "dodger blue"))))
 '(agda2-highlight-number-face ((t (:foreground "purple"))))
 '(agda2-highlight-postulate-face ((t (:foreground "royal blue"))))
 '(agda2-highlight-primitive-face ((t (:foreground "royal blue"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "royal blue"))))
 '(agda2-highlight-reachability-problem-face ((t (:box (:line-width 1 :color "dark gray")))))
 '(agda2-highlight-record-face ((t (:foreground "royal blue"))))
 '(agda2-highlight-termination-problem-face ((t (:box (:line-width 1 :color "light salmon")))))
 '(agda2-highlight-unsolved-constraint-face ((t (:box (:line-width 1 :color "Gold")))))
 '(agda2-highlight-unsolved-meta-face ((t (:box (:line-width 1 :color "Gold")))))
 '(custom-group-tag ((t (:inherit variable-pitch :foreground "light blue" :weight bold :height 1))))
 '(custom-group-tag-1 ((t (:inherit variable-pitch :foreground "pink" :weight bold :height 1))))
 '(markdown-header-face-1 ((t (:inherit bold :foreground "#4f97d7" :height 1.0))))
 '(markdown-header-face-2 ((t (:inherit bold :foreground "#2d9574" :height 1.0))))
 '(markdown-header-face-3 ((t (:foreground "#67b11d" :weight normal :height 1.0))))
 '(whitespace-trailing ((t (:background "#542b2e")))))


;;;; Agda

(load-file (let ((coding-system-for-read 'utf-8))
     (shell-command-to-string "agda-mode locate")))

;; ;; This will make e.g. make Agda's input-method avaiable for
;; ;; activation globally.
(require 'agda-input)


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


;;;; Helm

;; ;; Enable helm's recent files
;; (setq recentf-max-menu-items 25)
;; (global-set-key (kbd "C-x C-r") 'helm-recentf)

;; ;; Use helm by default
;; (define-key global-map [remap find-file] 'helm-find-files)
;; (define-key global-map [remap occur] 'helm-occur)
;; (define-key global-map [remap list-buffers] 'helm-buffers-list)
;; (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
;; (define-key global-map [remap execute-extended-command] 'helm-M-x)

;; ;; Swap TAB and C-j
;; (define-key helm-find-files-map "\t" 'helm-execute-persistent-action)
;; (define-key helm-find-files-map (kbd "C-j") 'helm-select-action)

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

;; (turn-on-purescript-indentation)
(global-set-key (kbd "C-x g") 'magit-status)

;; Haskell IDE
(add-to-list 'load-path "/home/fredefox/.emacs.d/git/lsp-mode")
(add-to-list 'load-path "/home/fredefox/.emacs.d/git/lsp-ui")
(add-to-list 'load-path "/home/fredefox/.emacs.d/git/lsp-haskell")
;; (require 'lsp-mode)
;; (require 'lsp-ui)
;; (require 'lsp-haskell)

;; (lsp-define-stdio-client
;;  ;; This can be a symbol of your choosing. It will be used as a the
;;  ;; prefix for a dynamically generated function "-enable"; in this
;;  ;; case: lsp-prog-major-mode-enable
;;  lsp-prog-major-mode
;;  "language-id"
;;  ;; This will be used to report a project's root directory to the LSP
;;  ;; server.
;;  (lambda () default-directory)
;;  ;; This is the command to start the LSP server. It may either be a
;;  ;; string containing the path of the command, or a list wherein the
;;  ;; car is a string containing the path of the command, and the cdr
;;  ;; are arguments to that command.
;;  '("/my/lsp/server" "and" "args"))

;; ;; Here we'll add the function that was dynamically generated by the
;; ;; call to lsp-define-stdio-client to the major-mode hook of the
;; ;; language we want to run it under.
;; ;;
;; ;; This function will turn lsp-mode on and call the command given to
;; ;; start the LSP server.
;; ;; (add-hook 'prog-major-mode #'lsp-prog-major-mode-enable)

;; ;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;; ;; (add-hook 'haskell-mode-hook #'lsp-haskell-enable)
;; ;; (add-hook 'haskell-mode-hook 'flycheck-mode)

(show-paren-mode)

(fset 'haskell/def
   [M-backspace ?\C-y ?  ?: ?: ?  ?_ return ?\C-y ?  ?= ?  ?_ ?\C-y])

;; (global-set-key (kbd "C-x C-e") 'magit-status)
(setq x-select-enable-clipboard t)
