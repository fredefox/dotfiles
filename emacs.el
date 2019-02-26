;;; init.el --- Summary
;;; Commentary:
;;; Initialization
;;; Code:
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
 '(agda2-highlight-face-groups (quote default-faces))
 '(agda2-program-args nil)
 '(auth-sources (quote ("~/.authinfo.gpg" "~/.authinfo" "~/.netrc")))
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(delete-selection-mode t)
 '(dired-isearch-filenames t)
 '(display-buffer-alist
   (quote
    ((".*Man.*" display-buffer-same-window)
     ("*Buffer List*" display-buffer-same-window))))
 '(echo-keystrokes 1e-10)
 '(erc-nick "fredefox")
 '(erc-port 667)
 '(erc-server "irc.freenode.net")
 '(flycheck-emacs-lisp-load-path (quote inherit))
 '(flycheck-ghc-language-extensions (quote ("UnicodeSyntax" "TypeApplications")))
 '(flycheck-hlint-language-extensions (quote ("UnicodeSyntax" "TypeApplications")))
 '(haskell-indentation-where-post-offset 0)
 '(haskell-indentation-where-pre-offset 0)
 '(haskell-language-extensions (quote ("UnicodeSyntax" "TypeApplications")))
 '(haskell-tags-on-save t)
 '(indent-tabs-mode nil)
 '(initial-scratch-message nil)
 '(js-indent-level 2)
 '(line-move-visual nil)
 '(markdown-command "pandoc -t html")
 '(menu-bar-mode nil)
 '(message-send-mail-function (quote smtpmail-send-it))
 '(package-selected-packages
   (quote
    (image+ company org-jira which-key flycheck es-mode lsp-haskell forge projectile lsp-ui lsp-mode editorconfig purescript-mode markdown-mode+ ssh-agency dash yaml-mode restart-emacs markdown-mode magit helm haskell-mode haml-mode form-feed dashboard)))
 '(projectile-mode t nil (projectile))
 '(projectile-project-search-path (quote ("~/git/")))
 '(purescript-mode-hook (quote (turn-on-purescript-indentation)) t)
 '(recentf-max-menu-items 255)
 '(recentf-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 101)
 '(scroll-margin 3)
 '(select-enable-clipboard t)
 '(send-mail-function (quote smtpmail-send-it))
 '(sgml-basic-offset 1)
 '(show-paren-mode t)
 '(set-mark-command-repeat-pop t)
 '(shr-width 80)
 '(split-window-keep-point nil)
 '(temp-buffer-resize-mode nil)
 '(tool-bar-mode nil)
 '(vc-follow-symlinks nil)
 '(window-combination-resize t)
 '(window-resize-pixelwise t))


;;;; Additional packages

(defvar extra-libs-root "~/.config/emacs/libs")

(defvar additional-packages
  (list
    "haskell-mode"
    "agda-mode"
    "purescript-mode"
    "psc-ide-emacs"))

;; TODO: Use the following pattern
;; (substitute-in-file-name "$XDG_DATA_HOME/emacs/libs/%s")

(add-to-list 'load-path                   "~/.config/emacs/libs/haskell-mode/")
(add-to-list 'Info-default-directory-list "~/.config/emacs/libs/haskell-mode/")
(add-to-list 'load-path                   "~/.config/emacs/libs/agda-mode/")
(add-to-list 'Info-default-directory-list "~/.config/emacs/libs/agda-mode/")
(add-to-list 'load-path                   "~/.config/emacs/libs/purescript-mode/")
(add-to-list 'Info-default-directory-list "~/.config/emacs/libs/purescript-mode/")
(add-to-list 'load-path                   "~/.config/emacs/libs/psc-ide-emacs/")
(add-to-list 'Info-default-directory-list "~/.config/emacs/libs/psc-ide-emacs/")

(require 'mu4e)

(require 'purescript-mode-autoloads)

(require 'psc-ide)

(add-hook 'purescript-mode-hook
  (lambda ()
    (psc-ide-mode)
    (company-mode)
    (turn-on-purescript-indentation)))

(require 'agda2-mode)

;; (require 'lsp)
;; Shame! `lsp-ui` is emitting:
;; Eager macro-expansion failure: (wrong-type-argument listp kind)
;; (require 'lsp-ui)
;; (require 'lsp-haskell)
;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;; (add-hook 'haskell-mode-hook #'lsp)


;;;; Captain Hook

(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(add-hook 'text-mode-hook
  (lambda ()
    (word-wrap-mode)
    (recentf-mode)
    (flyspell-mode)))

(add-hook 'prog-mode-hook
          (lambda ()
            (form-feed-mode)
            (flycheck-mode)))
;; I think this breaks e.g. the color-picker
; (add-hook 'text-mode-hook 'form-feed-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'ruby-mode-hook (lambda() (subword-mode 1)))
;; global-company-mode keeps recentering the point on the screen for
;; some reason
; (add-hook 'after-init-hook 'global-company-mode)
;; (add-hook 'after-init-hook 'flycheck-mode)

;; ;; (require 'haskell-unicode-input-method)


;;;; Projectile
(require 'projectile)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


;;;; Dashboard

(require 'dashboard)
(dashboard-setup-startup-hook)
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(setq dashboard-startup-banner (substitute-in-file-name "$XDG_DATA_HOME/emacs/banner.svg"))
(setq dashboard-items '((recents  . 40)))

;;;; Miscelaneous
(setq-default indent-tabs-mode nil)

(setq ring-bell-function
      (lambda ()
        (let ((orig-bg (face-background 'mode-line)))
          (set-face-background 'mode-line (face-attribute 'error :foreground))
          (run-with-idle-timer 0.1 nil
                               (lambda (bg)
                                 (set-face-background 'mode-line bg)) orig-bg))))

(global-unset-key (kbd "C-z"))

;;;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(require 'haskell-mode-autoloads)

;;;; Faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "chocolate1"))))
 '(font-lock-keyword-face ((t (:foreground "Cyan1"))))
 '(variable-pitch ((t nil))))

;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")


;;;; mu4e
(setq send-mail-function 'smtpmail-send-it
      user-mail-address "fhi.1990@gmail.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      ;; smtpmail-stream-type  'starttls
      smtpmail-smtp-service 587)

;; Now I set a list of
;; (defvar my-mu4e-account-alist
;;   '(("Gmail"
;;      (mu4e-sent-folder "/Gmail/sent")
;;      (user-mail-address "fhi.1990@gmail.com")
;;      (smtpmail-smtp-user "fhi.1990")
;;      (smtpmail-local-domain "gmail.com")
;;      (smtpmail-default-smtp-server "smtp.gmail.com")
;;      (smtpmail-smtp-server "smtp.gmail.com")
;;      (smtpmail-smtp-service 587)
;;      )
;;      ;; Include any other accounts here ...
;;     ))

;;; ERC
(require 'erc)
;; (erc :server "irc.freenode.net" :port 6667 :nick "fredefox")
;; (setq erc-autojoin-channels-alist
;;       '(("#data.coop" "#haskell" "#Agda")))

(setq ruby-deep-indent-paren nil)
(setq ruby-use-smie nil)

;; org-jira [https://github.com/ahungry/org-jira]
(require 'org-jira)
(setq jiralib-url "https://zendesk.atlassian.net")

;;; Jira
(add-to-list 'load-path "~/.config/emacs.d/lisp")
(require 'jira)
(define-key jira-mode-map (kbd "C-c j") 'jira-command-map)

;;; init.el ends here
