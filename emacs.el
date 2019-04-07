;;; init.el --- Summary
;;; Commentary:
;;; Initialization
;;; Code:
(require 'package)
(add-to-list 'package-archives
             '("MELPA" . "https://melpa.org/packages/") t)

(package-initialize)


;;; Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-program-args nil)
 '(auth-sources (quote ("~/.authinfo.gpg" "~/.authinfo" "~/.netrc")))
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(company-tooltip-minimum-width 35)
 '(custom-safe-themes
   (quote
    ("1c643a2d75eb06e39c552005eeb8b4cf52deccd895eaab3880fb299ae6bc41b4" default)))
 '(delete-selection-mode t)
 '(dired-isearch-filenames t)
 '(display-buffer-alist
   (quote
    ((".*Man.*" display-buffer-same-window)
     ("Buffer List" display-buffer-same-window
     ("magit-revision:" display-buffer-same-window)))))
 '(echo-keystrokes 1e-10)
 '(erc-autojoin-channels-alist (quote (("irc.freenode.net" "#haskell" "#data.coop"))))
 '(erc-autojoin-mode t)
 '(erc-nick "fredefox")
 '(erc-port 6667)
 '(erc-prompt-for-password nil)
 '(erc-server "irc.freenode.net")
 '(exec-path-from-shell-check-startup-files nil)
 '(flycheck-emacs-lisp-load-path (quote inherit))
 '(flycheck-ghc-language-extensions
   (quote
    ("UnicodeSyntax" "TypeApplications" "OverloadedStrings")))
 '(flycheck-hlint-language-extensions
   (quote
    ("UnicodeSyntax" "TypeApplications" "OverloadedStrings")))
 '(global-company-mode t)
 '(haskell-indentation-where-post-offset 0)
 '(haskell-indentation-where-pre-offset 0)
 '(haskell-language-extensions (quote ("UnicodeSyntax" "TypeApplications")))
 '(haskell-tags-on-save t)
 '(indent-tabs-mode nil)
 '(initial-scratch-message nil)
 '(js-indent-level 2)
 '(line-move-visual nil)
 '(magit-display-buffer-function (quote magit-display-buffer-same-window-except-diff-v1))
 '(magit-popup-display-buffer-action nil)
 '(markdown-command "pandoc -t html")
 '(menu-bar-mode nil)
 '(message-send-mail-function (quote smtpmail-send-it))
 '(org-agenda-files "~/.config/orgmode/agenda_files")
 '(package-selected-packages
   (quote
    (fill-column-indicator rjsx-mode image+ company org-jira which-key flycheck es-mode lsp-haskell forge projectile exec-path-from-shell lsp-ui lsp-mode editorconfig purescript-mode markdown-mode+ ssh-agency dash yaml-mode restart-emacs markdown-mode magit helm haskell-mode haml-mode form-feed dashboard)))
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "node_modules")))
 '(projectile-globally-ignored-files (quote ("/TAGS" "/vendor" "/.bundle" "/node_modules")))
 '(projectile-mode t nil (projectile))
 '(projectile-project-search-path (quote ("~/git/")))
 '(purescript-mode-hook (quote (turn-on-purescript-indentation)))
 '(recentf-max-menu-items 255)
 '(recentf-mode t)
 '(ruby-align-chained-calls 1)
 '(ruby-align-to-stmt-keywords t)
 '(ruby-insert-encoding-magic-comment nil)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 101)
 '(scroll-margin 0)
 '(select-enable-clipboard t)
 '(send-mail-function (quote smtpmail-send-it))
 '(set-mark-command-repeat-pop t)
 '(sgml-basic-offset 2)
 '(show-paren-mode t)
 '(shr-width 80)
 '(split-window-keep-point nil)
 '(tags-add-tables t)
 '(temp-buffer-resize-mode nil)
 '(tool-bar-mode nil)
 '(vc-follow-symlinks nil)
 '(window-combination-resize t)
 '(window-resize-pixelwise t))


;;;; Faces

;;; Needed on MAC because we're not using Xresources :(

(load-theme 'monokai-dark)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;; Additional packages

(defvar extra-libs-root "~/.config/emacs/libs")

(defvar additional-packages
  (list
    "agda-mode"
    "psc-ide-emacs"))

(setenv "XDG_CONFIG_HOME" (substitute-in-file-name "$HOME/.config"))
(setenv "XDG_DATA_HOME" (substitute-in-file-name "$HOME/.local/share"))
(setenv "XDG_CACHE_HOME" (substitute-in-file-name "$HOME/.cache"))

;; (let* ((additional-packages
;;        (list
;;         "agda-mode"
;;         "psc-ide-emacs"))
;;       (libs (substitute-in-file-name "$XDG_CONFIG_HOME/emacs/libs"))
;;       (add-package (lambda (package)
;;                      (let ((p (format "%s/%s" libs package)))
;;                        (print p)
;;                        (add-to-list 'load-path p)
;;                        (add-to-list 'Info-default-directory-list p)))))
;;   (mapc add-package additional-packages))

(let ((default-directory (substitute-in-file-name "$XDG_CONFIG_HOME/emacs/lisp")))
  (normal-top-level-add-subdirs-to-load-path))

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
(global-unset-key (kbd "s-q"))

(global-set-key (kbd "C-x r v") 'revert-buffer)

(add-hook 'text-mode-hook
          (lambda ()
            (toggle-word-wrap 1)
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
;; FIXME: My MAC is not loading the place where I set `XDG_DATA_HOME`
(setq dashboard-startup-banner (substitute-in-file-name "$HOME/.local/share/emacs/banner.png"))
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

(require 'haskell)

;;;; Ruby
(require 'ruby-mode)

;; I'm confused about the less worse option here.  I think the best
;; option is to use smie (the default).
(setq ruby-use-smie 1)

(setq ruby-deep-indent-paren nil)
(setq ruby-align-to-stmt-keywords t)

(add-to-list 'load-path "~/.config/emacs/git/spark")
(add-to-list 'load-path "~/.config/emacs/git/chruby")

(require 'chruby)

(setq select-enable-clipboard t)

(add-hook 'ruby-mode-hook '(lambda ()
  (global-set-key (kbd "C-c C-M-n") 'ruby-forward-sexp)
  (global-set-key (kbd "C-c C-M-p") 'ruby-backward-sexp)))

  (defun sql-beautify-region (beg end)
    "Beautify SQL in region between beg and END.
Dependency:
npm i -g sql-formatter-cli"
    (interactive "r")
    (save-excursion
      (shell-command-on-region beg end "sql-formatter-cli" nil t)))
  (defun sql-beautify-buffer ()
    "Beautify SQL in buffer."
    (interactive)
    (sql-beautify-region (point-min) (point-max)))
  (add-hook 'sql-mode-hook '(lambda ()
                              ;; beautify region or buffer
                              (local-set-key (kbd "C-c t") 'sql-beautify-region)))
(put 'dired-find-alternate-file 'disabled nil)

;; org-jira [https://github.com/ahungry/org-jira]
(require 'org-jira)
(setq jiralib-url "https://zendesk.atlassian.net")

;;; Jira
(add-to-list 'load-path "~/.config/emacs/lisp")
(require 'jira)
(define-key jira-mode-map (kbd "C-c j") 'jira-command-map)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;; init.el ends here
