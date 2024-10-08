;;; init.el --- Summary
;;; Commentary:
;;; Initialization
;;; Code:
(require 'package)
(add-to-list 'package-archives
             '("MELPA" . "https://melpa.org/packages/") t)

(package-initialize)


;;; Custom

(defvar
  haskell-language-extensions
  (quote ("UnicodeSyntax" "TypeApplications" "OverloadedStrings" "LambdaCase" "StandaloneDeriving" "DerivingStrategies" "DeriveGeneric" "DeriveAnyClass" "KindSignatures" "DerivingVia" "ConstraintKinds" "FlexibleContexts" "GeneralizedNewtypeDeriving" "ExplicitForAll" "ScopedTypeVariables"))
  "List of enabled Haskell language extensions.")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-name-width 30)
 '(agda2-program-args nil)
 '(auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(company-tooltip-minimum-width 35)
 '(css-indent-offset 2)
 '(custom-safe-themes
   '("e16181ddd62be929e53287afcb1a9977953bdb913dc095fe58267b0db80ee681" "224f84d5013ad0b98a43c54683302309a7cba53c0e37480a65284fd365774400" "d59e18ab7969fd68103ab0fe07e03c1830fd77c21c12a3fb4fe970931ddaf68d" "670df6cad1a732850a5d90ce2b0326969bd7596881dc1fed6b35091520a3da97" "aa81baddda211ffab84a5dc68750ac519d4841be63907a6b5de0cd72e631b172" "c91a5bf65b3f79ab28ab350b1d16c24d8b8bc1201e9c6c2106a60f98bceae754" default))
 '(dashboard-banner-logo-title "")
 '(dashboard-footer-icon "")
 '(dashboard-footer-messages '(""))
 '(dashboard-startup-banner "/home/fredefox/.local/share/emacs/banner.png")
 '(delete-selection-mode t)
 '(dired-isearch-filenames t)
 '(display-buffer-alist
   '(("*Man*" display-buffer-same-window)
     ("*Buffer List*" display-buffer-same-window)))
 '(echo-keystrokes 1e-10)
 '(editorconfig-mode t)
 '(erc-autojoin-channels-alist '(("irc.freenode.net" "#haskell" "#data.coop")))
 '(erc-autojoin-mode t)
 '(erc-nick "fredefox")
 '(erc-port 6667)
 '(erc-prompt-for-password nil)
 '(erc-server "irc.freenode.net")
 '(exec-path-from-shell-check-startup-files nil)
 '(flycheck-disabled-checkers '(haskell-hlint))
 '(flycheck-emacs-lisp-load-path 'inherit)
 '(flycheck-ghc-language-extensions (symbol-value 'haskell-language-extensions))
 '(flycheck-hlint-language-extensions (symbol-value 'haskell-language-extensions))
 '(forge-alist
   '(("github.com" "api.github.com" "github.com" forge-github-repository)
     ("gitlab.com" "gitlab.com/api/v4" "gitlab.com" forge-gitlab-repository)
     ("salsa.debian.org" "salsa.debian.org/api/v4" "salsa.debian.org" forge-gitlab-repository)
     ("framagit.org" "framagit.org/api/v4" "framagit.org" forge-gitlab-repository)
     ("codeberg.org" "codeberg.org/api/v1" "codeberg.org" forge-gitea-repository)
     ("code.orgmode.org" "code.orgmode.org/api/v1" "code.orgmode.org" forge-gogs-repository)
     ("bitbucket.org" "api.bitbucket.org/2.0" "bitbucket.org" forge-bitbucket-repository)
     ("git.savannah.gnu.org" nil "git.savannah.gnu.org" forge-cgit*-repository)
     ("git.kernel.org" nil "git.kernel.org" forge-cgit-repository)
     ("repo.or.cz" nil "repo.or.cz" forge-repoorcz-repository)
     ("git.suckless.org" nil "git.suckless.org" forge-stagit-repository)
     ("git.sr.ht" nil "git.sr.ht" forge-srht-repository)
     ("git.data.coop" "git.data.coop/api/v1" "git.data.coop" forge-gitea-repository)))
 '(global-company-mode t)
 '(haskell-indentation-where-post-offset 0)
 '(haskell-indentation-where-pre-offset 0)
 '(haskell-language-extensions (symbol-value 'haskell-language-extensions))
 '(haskell-tags-on-save (not (null (executable-find "hasktags"))))
 '(indent-tabs-mode nil)
 '(initial-scratch-message nil)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(kill-whole-line t)
 '(line-move-visual nil)
 '(lsp-clients-deno-server "/home/fredefox/.deno/bin/deno")
 '(lsp-eslint-auto-fix-on-save t)
 '(lsp-file-watch-ignored-directories
   '("[/\\\\]\\.git\\'" "[/\\\\]\\.github\\'" "[/\\\\]\\.gitlab\\'" "[/\\\\]\\.circleci\\'" "[/\\\\]\\.hg\\'" "[/\\\\]\\.bzr\\'" "[/\\\\]_darcs\\'" "[/\\\\]\\.svn\\'" "[/\\\\]_FOSSIL_\\'" "[/\\\\]\\.idea\\'" "[/\\\\]\\.ensime_cache\\'" "[/\\\\]\\.eunit\\'" "[/\\\\]node_modules" "[/\\\\]\\.yarn\\'" "[/\\\\]\\.fslckout\\'" "[/\\\\]\\.tox\\'" "[/\\\\]\\.nox\\'" "[/\\\\]dist\\'" "[/\\\\]dist-newstyle\\'" "[/\\\\]\\.stack-work\\'" "[/\\\\]\\.bloop\\'" "[/\\\\]\\.metals\\'" "[/\\\\]target\\'" "[/\\\\]\\.ccls-cache\\'" "[/\\\\]\\.vs\\'" "[/\\\\]\\.vscode\\'" "[/\\\\]\\.venv\\'" "[/\\\\]\\.mypy_cache\\'" "[/\\\\]\\.pytest_cache\\'" "[/\\\\]\\.build\\'" "[/\\\\]__pycache__\\'" "[/\\\\]\\.deps\\'" "[/\\\\]build-aux\\'" "[/\\\\]autom4te.cache\\'" "[/\\\\]\\.reference\\'" "[/\\\\]bazel-[^/\\\\]+\\'" "[/\\\\]\\.cache[/\\\\]lsp-csharp\\'" "[/\\\\]\\.meta\\'" "[/\\\\]\\.nuget\\'" "[/\\\\]Library\\'" "[/\\\\]\\.lsp\\'" "[/\\\\]\\.clj-kondo\\'" "[/\\\\]\\.shadow-cljs\\'" "[/\\\\]\\.babel_cache\\'" "[/\\\\]\\.cpcache\\'" "[/\\\\]\\checkouts\\'" "[/\\\\]\\.gradle\\'" "[/\\\\]\\.m2\\'" "[/\\\\]bin/Debug\\'" "[/\\\\]obj\\'" "[/\\\\]_opam\\'" "[/\\\\]_build\\'" "[/\\\\]\\.elixir_ls\\'" "[/\\\\]\\.elixir-tools\\'" "[/\\\\]\\.terraform\\'" "[/\\\\]\\.terragrunt-cache\\'" "[/\\\\]\\result" "[/\\\\]\\result-bin" "[/\\\\]\\.direnv\\'" "[/\\\\]vendor\\'" "[/\\\\]var/cache\\'"))
 '(lsp-haskell-formatting-provider "fourmolu")
 '(lsp-haskell-plugin-cabal-code-actions-on t)
 '(lsp-haskell-plugin-ghcide-code-actions-bindings-global-on t)
 '(lsp-haskell-plugin-ghcide-code-actions-imports-exports-global-on nil)
 '(lsp-haskell-plugin-ghcide-completions-config-auto-extend-on nil)
 '(lsp-haskell-plugin-hlint-code-actions-on nil)
 '(lsp-haskell-plugin-import-lens-code-actions-on nil)
 '(lsp-haskell-plugin-qualify-imported-names-global-on nil)
 '(lsp-haskell-plugin-refine-imports-global-on nil)
 '(lsp-haskell-plugin-stan-global-on nil)
 '(lsp-headerline-breadcrumb-enable nil)
 '(magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
 '(magit-popup-display-buffer-action nil)
 '(markdown-command "pandoc -t html")
 '(menu-bar-mode nil)
 '(message-send-mail-function 'smtpmail-send-it)
 '(org-agenda-files "~/.config/orgmode/agenda_files")
 '(package-selected-packages
   '(dap-mode chruby php-mode rust-mode flycheck-haskell prettier-js quelpa typescript-mode visual-fill-column ag ripgrep fill-column-indicator rjsx-mode image+ company org-jira which-key flycheck es-mode lsp-haskell forge projectile exec-path-from-shell lsp-ui lsp-mode editorconfig purescript-mode markdown-mode+ ssh-agency dash yaml-mode restart-emacs markdown-mode magit helm haskell-mode haml-mode form-feed dashboard))
 '(projectile-completion-system 'ido)
 '(projectile-globally-ignored-directories
   '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "node_modules" "vendor"))
 '(projectile-globally-ignored-files '("/TAGS" "/vendor" "/.bundle" "/node_modules"))
 '(projectile-mode t nil (projectile))
 '(projectile-project-search-path
   (seq-filter 'file-directory-p
               (seq-drop
                (directory-files "~/git" t)
                2)))
 '(projectile-switch-project-action 'magit-status)
 '(projectile-use-git-grep t)
 '(purescript-mode-hook '(turn-on-purescript-indentation))
 '(recentf-max-menu-items 255)
 '(recentf-mode t)
 '(recentf-save-file (substitute-in-file-name "$HOME/.config/emacs/recentf"))
 '(ruby-align-chained-calls t)
 '(ruby-align-to-stmt-keywords t)
 '(ruby-chained-calls t)
 '(ruby-insert-encoding-magic-comment nil)
 '(rust-indent-offset 2)
 '(safe-local-variable-values
   '((eval setq typescript-indent-level 4)
     (eval prettier-js 0)
     (setq magit-refresh-verbose 1)
     (eval remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
     (eval
      (remove-hook 'magit-refs-sections-hook 'magit-insert-tags))
     (magit-refresh-buffers)
     (git-commit-major-mode . git-commit-elisp-text-mode)))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 101)
 '(scroll-margin 0)
 '(select-enable-clipboard t)
 '(send-mail-function 'smtpmail-send-it)
 '(set-mark-command-repeat-pop t)
 '(sgml-basic-offset 2)
 '(sh-basic-offset 2)
 '(sh-indent-after-continuation nil)
 '(show-paren-mode t)
 '(shr-width 80)
 '(split-window-keep-point nil)
 '(tab-width 2)
 '(tags-add-tables t)
 '(temp-buffer-resize-mode nil)
 '(tool-bar-mode nil)
 '(typescript-indent-level 2)
 '(vc-follow-symlinks nil)
 '(window-combination-resize t)
 '(window-resize-pixelwise t))

(windmove-default-keybindings)


;;;; MAC setup

(defun set-xdg-variables ()
  "Set the XDG base directory variables to sane defaults."
  (setenv "XDG_CONFIG_HOME" (substitute-in-file-name "$HOME/.config"))
  (setenv "XDG_DATA_HOME" (substitute-in-file-name "$HOME/.local/share"))
  (setenv "XDG_CACHE_HOME" (substitute-in-file-name "$HOME/.cache")))

(defun load-monokai ()
  "Load the monokai dark theme."
  (add-to-list 'custom-theme-load-path
             (substitute-in-file-name
              "$XDG_CONFIG_HOME/emacs/lisp/monokai-dark-theme/"))
  (load-theme 'monokai-dark))

(defun x11-shim ()
  "Replace some behaviour otherwise handled by other system services."
  ;; (load-theme 'monokai-dark)
  ;; TODO Why is this not handled by the magic with the load-path above?
  (set-xdg-variables)
  (load-monokai))

;;; Needed on MAC because we're not using Xresources :(
(x11-shim)


;;;; Additional packages
;;;; Maybe we should use qelpa to mangage these.
(defvar extra-libs-root (substitute-in-file-name "$XDG_CONFIG_HOME/emacs/lisp/"))

(defvar additional-packages
      '((agda2-mode . "agda-mode/")
        (psc-ide . "psc-ide/")
        ;; (org-jira . "org-jira/")
        (jira . "jira/")
        (spark . "spark/")
        (chruby . "chruby/")))

(defun load-additional-packages ()
  "Load the additional packages as specified by additional-packages."
  (dolist (spec additional-packages)
  (let* ((package (car spec))
         (package-path (cdr spec))
         (path (concat extra-libs-root package-path)))
    (add-to-list 'load-path path)
    (require package))))

;; (load-additional-packages)

(defun load-additional-themes ()
  "Load additional themes."
  (add-to-list 'custom-theme-load-path (concat extra-libs-root "inheritance-theme/"))
  (load-theme 'inheritance))

(load-additional-themes)

(require 'lsp)
;; Shame! `lsp-ui` is emitting:
;; Eager macro-expansion failure: (wrong-type-argument listp kind)
;; (require 'lsp-ui)
;; (require 'lsp-haskell)
;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'haskell-mode-hook #'lsp)

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))


;;;; Faces

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch-serif ((t (:family "Monospace Serif"))))
 '(font-lock-comment-face ((t (:foreground "chocolate1"))))
 '(region ((t (:background "#285b89"))))
 '(success ((t (:foreground "Green3" :weight bold)))))


;;;; Captain Hook

(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-unset-key (kbd "s-q"))

(global-set-key (kbd "C-c C-o") 'ffap)

(global-set-key (kbd "M-p") 'flycheck-previous-error)
(global-set-key (kbd "M-n") 'flycheck-next-error)

(add-hook 'help-mode-hook
          (lambda ()
            (local-set-key (kbd "M-n") 'forward-button)
            (local-set-key (kbd "n") 'forward-button)
            (local-set-key (kbd "M-p") 'backward-button)
            (local-set-key (kbd "p") 'backward-button)))

(windmove-default-keybindings)

(defun insert-quotations (&optional arg)
  "Enclose following ARG sexps in quotation-marks.
Leave point after open-paren."
  (interactive "*P")
  (insert-pair arg ?\' ?\'))

(defun insert-quotes (&optional arg)
  "Enclose following ARG sexps in quotes.
Leave point after open-quote."
  (interactive "*P")
  (insert-pair arg ?\" ?\"))

(global-set-key "\M-'" 'insert-quotations)
(global-set-key "\M-\"" 'insert-quotes)

(global-set-key (kbd "C-<left>") 'previous-buffer)
(global-set-key (kbd "C-<right>") 'next-buffer)

(global-set-key (kbd "C-x r v") 'revert-buffer)

(add-hook 'text-mode-hook
          (lambda ()
            (toggle-word-wrap t)
            (recentf-mode)
            (flyspell-mode)))

(add-hook 'html-mode-hook
          (lambda ()
            (flyspell-mode -1)))

(add-hook 'prog-mode-hook
          (lambda ()
            (form-feed-mode)
            (flycheck-mode)))

;; I think this breaks e.g. the color-picker
; (add-hook 'text-mode-hook 'form-feed-mode)
(add-hook 'haskell-mode-hook
          (lambda ()
            (subword-mode t)
            (interactive-haskell-mode t)
            ; disable haskell-forward-sexp due to https://github.com/haskell/haskell-mode/issues/1838
            (setq-local forward-sexp-function nil)))

(add-hook 'ruby-mode-hook (lambda () (subword-mode t)))
;; global-company-mode keeps recentering the point on the screen for
;; some reason
; (add-hook 'after-init-hook 'global-company-mode)
;; (add-hook 'after-init-hook 'flycheck-mode)

;; ;; (require 'haskell-unicode-input-method)

(add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-mode))


(add-hook
 'typescript-mode-hook
 (lambda ()
   (lsp)
   (subword-mode t)
   ;; (prettier-js-mode t)
   ))

(defalias 'yes-or-no-p 'y-or-n-p)


;;;; Projectile
(require 'projectile)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


;;;; Dashboard

(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-startup-banner (substitute-in-file-name "$XDG_DATA_HOME/emacs/banner.png"))
(setq dashboard-items '((recents  . 40)))

(add-hook 'dashboard-mode-hook
          (lambda ()
             (local-set-key "n" 'dashboard-next-line)
             (local-set-key "p" 'dashboard-previous-line)))

;;;; Miscelaneous
(setq-default indent-tabs-mode nil)

(defun mode-line-bell-set-background (bg)
  "Set the background color of the mode-line to BG."
  (set-face-background 'mode-line bg))

(defun mode-line-bell-blink ()
  "Briefly change the color of the mode-line."
  (let ((orig-bg (face-background 'mode-line)))
    (mode-line-bell-set-background (face-attribute 'error :foreground))
    (run-with-idle-timer 0.1 nil 'mode-line-bell-set-background orig-bg)))

(setq ring-bell-function 'mode-line-bell-blink)

(global-unset-key (kbd "C-z"))

;;;; Magit
(require 'magit)
;; (global-magit-file-mode t)
(global-set-key (kbd "C-c g g") 'magit-dispatch)
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g f") 'magit-file-dispatch)
(global-set-key (kbd "C-c g b") 'magit-blame)

(require 'haskell)

;;;; Ruby
(require 'ruby-mode)

(setq ruby-deep-indent-paren nil)
(setq ruby-align-to-stmt-keywords t)

(setq select-enable-clipboard t)

(add-hook 'ruby-mode-hook #'(lambda ()
  (local-set-key (kbd "C-M-n") 'ruby-forward-sexp)
  (local-set-key (kbd "C-M-p") 'ruby-backward-sexp)
  (chruby-use-corresponding)))

(defun sql-beautify-region (beg end)
  "Beautify SQL in region between BEG and END.
Dependency:
npm i -g sql-formatter-cli"
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end "sql-formatter-cli" nil t)))

(defun sql-beautify-buffer ()
    "Beautify SQL in buffer."
    (interactive)
    (sql-beautify-region (point-min) (point-max)))

(add-hook 'sql-mode-hook #'(lambda ()
                            ;; beautify region or buffer
                            (local-set-key (kbd "C-c t") 'sql-beautify-region)))
(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(add-hook 'yaml-mode-hook #'(lambda ()
  (flyspell-mode 0)))

(add-hook 'html-mode-hook #'(lambda ()
  (setq-local sgml-basic-offset 1)))

(require 'semantic/symref/grep)

(add-hook 'php-mode-hook #'(lambda ()
  (add-to-list 'semantic-symref-filepattern-alist '(php-mode "*.php" "*.inc"))
  (lsp)))

;; org-jira [https://github.com/ahungry/org-jira]
;; (require 'org-jira)
;; (setq jiralib-url "https://zendesk.atlassian.net")

;;; Jira
;; (require 'jira)
(global-set-key (kbd "C-c j") 'jira)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(exec-path-from-shell-initialize)

(require 'flycheck)
(define-key flycheck-mode-map flycheck-keymap-prefix nil)
(setq flycheck-keymap-prefix (kbd "C-c f"))
(define-key flycheck-mode-map flycheck-keymap-prefix
            flycheck-command-map)

;;; init.el ends here
