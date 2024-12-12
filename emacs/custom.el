;;; custom.el --- Summary -*- lexical-binding:t -*-
;;; Commentary:
;;; Initialization
;;; Code:

(defvar
  haskell-language-extensions
  '("UnicodeSyntax" "TypeApplications" "OverloadedStrings" "LambdaCase" "StandaloneDeriving" "DerivingStrategies" "DeriveGeneric" "DeriveAnyClass" "KindSignatures" "DerivingVia" "ConstraintKinds" "FlexibleContexts" "GeneralizedNewtypeDeriving" "ExplicitForAll" "ScopedTypeVariables")
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
   '("4bf8dc4638215ae1e5ed891daae1a0615e036275a9fe11918a94730c63b71749"
     "d685c5355a076a43ff793f8b85ec352508b35139adac124de21b6795fbf613fc"
     "de48118c05b391e4f1754474c6b1de5c52e2018a90a9cd1f02a14e59d4c9ddd5"
     default))
 '(custom-theme-directory (file-name-concat user-emacs-directory "themes"))
 '(dap-ui-controls-screen-position 'posframe-poshandler-frame-top-right-corner)
 '(dashboard-banner-logo-title "")
 '(dashboard-footer-icon "")
 '(dashboard-footer-messages '(""))
 '(dashboard-image-banner-max-width 180)
 '(dashboard-startup-banner (file-name-concat (xdg-data-home) "emacs/banner.png"))
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
 '(flycheck-ghc-language-extensions haskell-language-extensions)
 '(flycheck-hlint-language-extensions haskell-language-extensions)
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
 '(haskell-language-extensions haskell-language-extensions)
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
 '(lsp-keymap-prefix "C-c l")
 '(lsp-kotlin-debug-adapter-enabled t)
 '(lsp-kotlin-debug-adapter-path "/opt/kotlin-debug-adapter/bin/kotlin-debug-adapter")
 '(magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
 '(magit-popup-display-buffer-action nil)
 '(markdown-command "pandoc -t html")
 '(menu-bar-mode nil)
 '(message-send-mail-function 'smtpmail-send-it)
 '(org-agenda-files "~/.config/orgmode/agenda_files")
 '(package-native-compile t)
 '(package-quickstart t)
 '(package-selected-packages
   '(ag chruby company cucumber dap-mode dash dashboard editorconfig
        es-mode exec-path-from-shell fill-column-indicator flycheck
        flycheck-haskell forge form-feed haml-mode haskell-mode
        haskell-ts-mode helm image+ kotlin-mode kotlin-ts-mode
        lsp-haskell lsp-mode lsp-ui magit markdown-mode php-mode
        prettier-js projectile psc-ide purescript-mode quelpa
        restart-emacs ripgrep rjsx-mode rust-mode ssh-agency
        typescript-mode visual-fill-column which-key yaml-mode))
 '(package-vc-selected-packages
   '((chruby :url "git@github.com:plexus/chruby.el.git" :vc-backend Git)
     (psc-ide :url "git@github.com:purescript-emacs/psc-ide-emacs.git"
              :vc-backend Git)
     (cucumber :url "git@github.com:michaelklishin/cucumber.el.git"
               :vc-backend Git)))
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
 '(recentf-max-saved-items 255)
 '(recentf-mode t)
 '(recentf-save-file (locate-user-emacs-file "recentf"))
 '(ruby-align-chained-calls t)
 '(ruby-align-to-stmt-keywords t)
 '(ruby-chained-calls t)
 '(ruby-insert-encoding-magic-comment nil)
 '(rust-indent-offset 2)
 '(safe-local-variable-values
   '((eval add-to-list 'lsp-file-watch-ignored-directories
           "[/\\\\]docs/public" t)
     (eval setq js-indent-level 4)
     (eval setq flycheck-javascript-eslint-executable
           "npm exec eslint")
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
 '(suggest-key-bindings nil)
 '(tab-width 2)
 '(tags-add-tables t)
 '(temp-buffer-resize-mode nil)
 '(tool-bar-mode nil)
 '(typescript-indent-level 2)
 '(vc-follow-symlinks nil)
 '(warning-suppress-types '((lsp-mode)))
 '(window-combination-resize t)
 '(window-resize-pixelwise t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#090101" :foreground "#f6fefe" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 130 :width normal :foundry "ADBO" :family "Source Code Pro"))))
 '(dap-ui-marker-face ((t (:inherit underline))))
 '(fixed-pitch-serif ((t (:family "Monospace Serif"))))
 '(font-lock-comment-face ((t (:foreground "#e66f19"))))
 '(font-lock-string-face ((t (:foreground "#ffcc33"))))
 '(region ((t (:background "#285b89"))))
 '(success ((t (:foreground "Green3" :weight bold)))))

;;; custom.el ends here
