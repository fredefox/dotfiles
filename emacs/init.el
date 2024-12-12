;;; init.el --- Summary -*- lexical-binding:t -*-
;;; Commentary:
;;; Initialization
;;; Code:
(require 'package)
(add-to-list
 'package-archives
 '("MELPA" . "https://melpa.org/packages/") t)

(package-initialize)


;;; Preamble

(defun subdirectories (directory)
  "List directories in DIRECTORY."
  (seq-filter 'file-directory-p (seq-drop (directory-files directory t) 2)))

(require 'project)
(defun project-prompt-project-dir-ido ()
  "Like `project-prompt-project-dir' but using `ido-completing-read'."
  (ido-completing-read "Switch to project " project--list))

(require 'xdg)

(setq custom-file (file-name-concat user-emacs-directory "custom.el"))
(load custom-file)

(unless package-archive-contents
  (package-refresh-contents)
  (package-install-selected-packages t))

(windmove-default-keybindings)


;;;; Discover projects
(require 'project)

(let ((inhibit-message t))
  (mapc #'project-remember-projects-under (subdirectories "~/git")))


(mapc #'load-theme '(inheritance monokai-dark))

(require 'lsp)
(require 'lsp-ui)
(require 'lsp-haskell)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'haskell-mode-hook #'lsp)


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
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.hs" . haskell-mode))

(require 'lsp-eslint)
(add-hook
 'typescript-mode-hook
 (lambda ()
   (lsp)
   (subword-mode t)
   ;; (prettier-js-mode t)
   (add-hook 'before-save-hook #'lsp-eslint-fix-all)))

(defalias 'yes-or-no-p 'y-or-n-p)


;;;; Projectile
(require 'projectile)

(projectile-mode t)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


;;;; Dashboard

(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-startup-banner (file-name-concat (xdg-data-home) "emacs/banner.png"))
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
(require 'dap-kotlin)

(setq dap-auto-configure-features '(sessions locals controls tooltip))

(defconst dap-debug--debug-template-api-sign
  `(
    :type "php"
    :cwd ,(substitute-in-file-name "$HOME/git/penneo/api-sign")
    :request "launch"
    :name "api-sign"
    ;; :stopOnEntry t
    :serverSourceRoot "/app/data"
    :localSourceRoot ,(substitute-in-file-name "$HOME/git/penneo/api-sign")
    :port 9003
    ;; :args '("--server=4711")
    :sourceMaps t))

(defconst dap-debug--debug-template-api-auth
  `(
    :type "php"
    :cwd ,(substitute-in-file-name "$HOME/git/penneo/api-auth")
    :request "launch"
    :name "api-auth"
    ;; :stopOnEntry t
    :serverSourceRoot "/app/data"
    :localSourceRoot ,(substitute-in-file-name "$HOME/git/penneo/api-auth")
    :port 9004
    :enableJsonLogging nil
    ;; :args '("--server=4711")
    :sourceMaps t))

(defconst dap-debug--debug-template-api-pdf
  `(
    :type "kotlin"
    :cwd ,(substitute-in-file-name "$HOME/git/penneo/api-pdf/api")
    :request "launch"
    :name "api-pdf"
    ;; :stopOnEntry t
    :serverSourceRoot "/app/data"
    :mainClass "com.penneo.pdf.api.AppKt"
    :localSourceRoot ,(substitute-in-file-name "$HOME/git/penneo/api-auth")
    :port 9004
    ;; :args '("--server=4711")
    :sourceMaps t))

(defconst dap-debug--debug-template-api-pdf
  `(
    :type "kotlin"
    :request "attach"
    :port 8789
    :mainClass "com.penneo.pdf.api.AppKt"))

(defconst dap-debug--debug-templates
  `((api-sign . ,dap-debug--debug-template-api-sign)
    (api-auth . ,dap-debug--debug-template-api-auth)
    (api-pdf . ,dap-debug--debug-template-api-pdf)))

(mapc (lambda (pair) (dap-register-debug-template (symbol-name (car pair)) (cdr pair))) dap-debug--debug-templates)

;; (mapc #'dap-register-debug-template '(dap-debug--debug-template-api-sign dap-debug--debug-template-api-auth))
;; (dap-register-debug-template "api-sign" dap-debug--debug-template-api-sign)

(defun dap-debug-api-sign ()
  "Run `dap-debug` with the configuration `dap-debug--debug-template-api-sign'."
  (interactive)
  (dap-debug dap-debug--debug-template-api-sign))

(defun dap-debug-api-pdf ()
  "Run `dap-debug` with the configuration `dap-debug--debug-template-api-pdf'."
  (interactive)
  (dap-debug dap-debug--debug-template-api-pdf))

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
        (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (kotlin "git@github.com:fwcd/tree-sitter-kotlin.git")))

(defun initialize-treesit ()
  "Download and install treesit recipees listed in \
`treesit-language-source-alist'."

  (unless (file-exists-p (locate-user-emacs-file "tree-sitter"))
    (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))))

(initialize-treesit)
;;; init.el ends here
