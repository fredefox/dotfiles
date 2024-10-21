;;; init.el --- Summary -*- lexical-binding:t -*-
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

(defun subdirectories (directory)
  "List directories in DIRECTORY."
  (seq-filter 'file-directory-p (seq-drop (directory-files directory t) 2)))

(require 'project)
(defun project-prompt-project-dir-ido ()
  "Like `project-prompt-project-dir' but using `ido-completing-read'."
  (ido-completing-read "Switch to project " project--list))

(require 'xdg)

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
   '("733043848a35c2c5048430a6c86cb581bb05c2db1f1d6629f2b9141daa7592d4"
     "8397896ca5b6d6d3f6d7b7dada40f5da715b6b539b26570fd2b4f8a9e4c4853e"
     "1e16406b258e333f7333936edcc976796bbe2dbf079887438301203b743b3bd3"
     "37277266acc00347a163e50c67dd74bf46a705c6ac74a1f7abbc1c7667c4ec46"
     "e16181ddd62be929e53287afcb1a9977953bdb913dc095fe58267b0db80ee681"
     "224f84d5013ad0b98a43c54683302309a7cba53c0e37480a65284fd365774400"
     "d59e18ab7969fd68103ab0fe07e03c1830fd77c21c12a3fb4fe970931ddaf68d"
     "670df6cad1a732850a5d90ce2b0326969bd7596881dc1fed6b35091520a3da97"
     "aa81baddda211ffab84a5dc68750ac519d4841be63907a6b5de0cd72e631b172"
     "c91a5bf65b3f79ab28ab350b1d16c24d8b8bc1201e9c6c2106a60f98bceae754"
     default))
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
     ("gitlab.com" "gitlab.com/api/v4" "gitlab.com"
      forge-gitlab-repository)
     ("salsa.debian.org" "salsa.debian.org/api/v4" "salsa.debian.org"
      forge-gitlab-repository)
     ("framagit.org" "framagit.org/api/v4" "framagit.org"
      forge-gitlab-repository)
     ("codeberg.org" "codeberg.org/api/v1" "codeberg.org"
      forge-gitea-repository)
     ("code.orgmode.org" "code.orgmode.org/api/v1" "code.orgmode.org"
      forge-gogs-repository)
     ("bitbucket.org" "api.bitbucket.org/2.0" "bitbucket.org"
      forge-bitbucket-repository)
     ("git.savannah.gnu.org" nil "git.savannah.gnu.org"
      forge-cgit*-repository)
     ("git.kernel.org" nil "git.kernel.org" forge-cgit-repository)
     ("repo.or.cz" nil "repo.or.cz" forge-repoorcz-repository)
     ("git.suckless.org" nil "git.suckless.org"
      forge-stagit-repository)
     ("git.sr.ht" nil "git.sr.ht" forge-srht-repository)
     ("git.data.coop" "git.data.coop/api/v1" "git.data.coop"
      forge-gitea-repository)))
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
   '("[/\\\\]\\.git\\'" "[/\\\\]\\.github\\'" "[/\\\\]\\.gitlab\\'"
     "[/\\\\]\\.circleci\\'" "[/\\\\]\\.hg\\'" "[/\\\\]\\.bzr\\'"
     "[/\\\\]_darcs\\'" "[/\\\\]\\.svn\\'" "[/\\\\]_FOSSIL_\\'"
     "[/\\\\]\\.idea\\'" "[/\\\\]\\.ensime_cache\\'"
     "[/\\\\]\\.eunit\\'" "[/\\\\]node_modules" "[/\\\\]\\.yarn\\'"
     "[/\\\\]\\.fslckout\\'" "[/\\\\]\\.tox\\'" "[/\\\\]\\.nox\\'"
     "[/\\\\]dist\\'" "[/\\\\]dist-newstyle\\'"
     "[/\\\\]\\.stack-work\\'" "[/\\\\]\\.bloop\\'"
     "[/\\\\]\\.metals\\'" "[/\\\\]target\\'"
     "[/\\\\]\\.ccls-cache\\'" "[/\\\\]\\.vs\\'" "[/\\\\]\\.vscode\\'"
     "[/\\\\]\\.venv\\'" "[/\\\\]\\.mypy_cache\\'"
     "[/\\\\]\\.pytest_cache\\'" "[/\\\\]\\.build\\'"
     "[/\\\\]__pycache__\\'" "[/\\\\]\\.deps\\'" "[/\\\\]build-aux\\'"
     "[/\\\\]autom4te.cache\\'" "[/\\\\]\\.reference\\'"
     "[/\\\\]bazel-[^/\\\\]+\\'" "[/\\\\]\\.cache[/\\\\]lsp-csharp\\'"
     "[/\\\\]\\.meta\\'" "[/\\\\]\\.nuget\\'" "[/\\\\]Library\\'"
     "[/\\\\]\\.lsp\\'" "[/\\\\]\\.clj-kondo\\'"
     "[/\\\\]\\.shadow-cljs\\'" "[/\\\\]\\.babel_cache\\'"
     "[/\\\\]\\.cpcache\\'" "[/\\\\]\\checkouts\\'"
     "[/\\\\]\\.gradle\\'" "[/\\\\]\\.m2\\'" "[/\\\\]bin/Debug\\'"
     "[/\\\\]obj\\'" "[/\\\\]_opam\\'" "[/\\\\]_build\\'"
     "[/\\\\]\\.elixir_ls\\'" "[/\\\\]\\.elixir-tools\\'"
     "[/\\\\]\\.terraform\\'" "[/\\\\]\\.terragrunt-cache\\'"
     "[/\\\\]\\result" "[/\\\\]\\result-bin" "[/\\\\]\\.direnv\\'"
     "[/\\\\]vendor\\'" "[/\\\\]var/cache\\'"))
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
 '(package-native-compile t)
 '(package-quickstart t)
 '(package-selected-packages
   '(ag chruby company dap-mode dash dashboard editorconfig es-mode
        exec-path-from-shell fill-column-indicator flycheck
        flycheck-haskell forge form-feed haml-mode haskell-mode helm
        image+ lsp-haskell lsp-mode lsp-ui magit markdown-mode
        markdown-mode+ php-mode prettier-js projectile purescript-mode
        quelpa restart-emacs ripgrep rjsx-mode rust-mode spark
        ssh-agency typescript-mode visual-fill-column which-key
        yaml-mode))
 '(project-prompter 'project-prompt-project-dir-ido)
 '(project-switch-commands 'magit-status)
 '(projectile-completion-system 'ido)
 '(projectile-globally-ignored-directories
   '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_"
     ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "node_modules"
     "vendor"))
 '(projectile-globally-ignored-files '("/TAGS" "/vendor" "/.bundle" "/node_modules"))
 '(projectile-project-search-path
   (seq-filter 'file-directory-p (seq-drop (directory-files "~/git" t) 2)))
 '(projectile-switch-project-action 'magit-status)
 '(projectile-use-git-grep t)
 '(purescript-mode-hook '(turn-on-purescript-indentation))
 '(recentf-max-menu-items 255)
 '(recentf-mode t)
 '(recentf-save-file (locate-user-emacs-file "recentf"))
 '(ruby-align-chained-calls t)
 '(ruby-align-to-stmt-keywords t)
 '(ruby-chained-calls t)
 '(ruby-insert-encoding-magic-comment nil)
 '(rust-indent-offset 2)
 '(safe-local-variable-values
   '((eval setq flycheck-javascript-eslint-executable "npm exec eslint")
     (eval setq typescript-indent-level 4) (eval prettier-js 0)
     (setq magit-refresh-verbose 1)
     (eval remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
     (eval (remove-hook 'magit-refs-sections-hook 'magit-insert-tags))
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
 '(warning-suppress-types '((lsp-mode)))
 '(window-combination-resize t)
 '(window-resize-pixelwise t))

(windmove-default-keybindings)


;;;; Discover projects
(require 'project)

(let ((inhibit-message t))
  (mapc #'project-remember-projects-under (subdirectories "~/git")))


;;;; Additional packages
;;;; Maybe we should use qelpa to mangage these.
(defvar extra-libs-root (locate-user-emacs-file "lisp"))

(defvar additional-packages
  '(
    ;; (agda2-mode . "/agda-mode")
    ;; (psc-ide . "/psc-ide")
    ;; (spark . "/spark")
    ;; (chruby . "/chruby")
    ))

;; (package-vc-install
;;  '(spark :url "git@github.com:alvinfrancis/spark.git" :vc-backend Git))

(defun load-additional-packages ()
  "Load the additional packages as specified by `additional-packages'."
  (dolist (spec additional-packages)
    (let* ((package (car spec))
           (package-path (cdr spec))
           (path (concat extra-libs-root package-path)))
      (cond ((file-exists-p path)
             (add-to-list 'load-path path)
             (require package))
            (t (warn (format "Additional package `%s' does not exist" package)))))))

(load-additional-packages)

(defvar additional-themes
      '((inheritance . "/inheritance-theme")
        (monokai-dark . "/monokai-dark-theme")))

(defun load-additional-themes ()
  "Load the additional themes as specified by `additional-themes'."
  (dolist (spec additional-themes)
    (let* ((theme (car spec))
           (theme-path (cdr spec))
           (path (concat extra-libs-root theme-path)))
      (cond ((file-exists-p path)
             (add-to-list 'custom-theme-load-path path)
             (load-theme theme))
            (t (warn (format "Additional theme `%s' does not exist" theme)))))))

(load-additional-themes)

(require 'lsp)
(require 'lsp-ui)
(require 'lsp-haskell)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'haskell-mode-hook #'lsp)


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

(keymap-global-set "C-x C-r" 'recentf-open-files)
(keymap-global-unset "s-q")

(keymap-global-set "C-c C-o" 'ffap)

(define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error)
(define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)

(add-hook 'help-mode-hook
          (lambda ()
            (local-set-key (kbd "M-n") 'forward-button)
            (local-set-key (kbd "n") 'forward-button)
            (local-set-key (kbd "M-p") 'backward-button)
            (local-set-key (kbd "p") 'backward-button)))

(require 'view)
(define-key view-mode-map (kbd "n") 'forward-line)
(define-key view-mode-map (kbd "p") 'previous-line)

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

(keymap-global-set "M-'" 'insert-quotations)
(keymap-global-set "M-\"" 'insert-quotes)

(keymap-global-set "C-<left>" 'previous-buffer)
(keymap-global-set "C-<right>" 'next-buffer)

(keymap-global-set "C-x r v" 'revert-buffer)

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

(projectile-mode t)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


;;;; Dashboard

(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-startup-banner (concat (xdg-data-home) "/emacs/banner.png"))
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

(keymap-global-unset "C-z")

;;;; Magit
(require 'magit)
(keymap-global-set "C-c g g" 'magit-dispatch)
(keymap-global-set "C-c g s" 'magit-status)
(keymap-global-set "C-c g f" 'magit-file-dispatch)
(keymap-global-set "C-c g b" 'magit-blame)
(setq magit-process-finish-apply-ansi-colors t)

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

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(exec-path-from-shell-initialize)

(require 'flycheck)
(define-key flycheck-mode-map flycheck-keymap-prefix nil)
(setq flycheck-keymap-prefix (kbd "C-c f"))
(define-key flycheck-mode-map flycheck-keymap-prefix
            flycheck-command-map)

(require 'dap-mode)
(require 'dap-php)
(defconst dap-debug--debug-template-api-sign
  `(
    :type "php"
    :cwd nil
    :request "launch"
    :name "api-sign"
    ;; :stopOnEntry t
    :serverSourceRoot "/app/data"
    :localSourceRoot ,(substitute-in-file-name "$HOME/git/penneo/api-sign")
    ;; :args '("--server=4711")
    :sourceMaps t))

(dap-register-debug-template "api-sign" dap-debug--debug-template-api-sign)

(defun dap-debug-api-sign ()
  "Run `dap-debug` with the configuration `dap-debug--debug-template-api-sign'."
  (interactive)
  (dap-debug dap-debug--debug-template-api-sign))

(define-key dap-mode-map (kbd "<f8>") 'dap-continue)
(define-key dap-mode-map (kbd "<f10>") 'dap-next)
(define-key dap-mode-map (kbd "<f11>") 'dap-step-in)
(define-key dap-mode-map (kbd "S-<f11>") 'dap-step-out)

(require 'treesit)

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (haskell "https://github.com/tree-sitter/tree-sitter-haskell")))

(defun initialize-treesit ()
  "Download and install treesit recipees listed in\
`treesit-language-source-alist'."

  (unless (file-exists-p (locate-user-emacs-file "tree-sitter"))
    (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))))

(initialize-treesit)
;;; init.el ends here
