;;; init.el -- Ben's init.el
;;
;; Free
;;
;;; Commentary:
;;
;; Nothing worth mentioning.

;;; Packages:

;; This has to be before we require lsp-mode.
(setq lsp-keymap-prefix "C-c l")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defun install-packages ()
  (straight-use-package 'magit)
  (straight-use-package 'exec-path-from-shell)
  (straight-use-package 'markdown-mode)
  (straight-use-package 'rainbow-mode)
  (straight-use-package 'smart-cursor-color)
  (straight-use-package 'yaml-mode)
  (straight-use-package 'helm)
  (straight-use-package 'helm-ag)
  (straight-use-package 'projectile)
  (straight-use-package 'helm-projectile)
  (straight-use-package 'auto-virtualenv)

  ;; Go
  (straight-use-package 'go-mode)
  (straight-use-package 'flycheck-golangci-lint)
  
  ;; Rust
  (straight-use-package 'rust-mode)

  ;; Python
  (straight-use-package 'python-mode)
  (straight-use-package 'python-black)
  (straight-use-package 'python-isort)
  (straight-use-package 'yasnippet)
  ;; (straight-use-package 'js2-mode)
  (straight-use-package 'typescript-mode)
  ;; (straight-use-package 'jest)
  (straight-use-package 'web-mode)
  ;; (straight-use-package 'cython-mode)
  ;; (straight-use-package 'inf-ruby)
  ;; (straight-use-package 'ruby-mode)
  ;; (straight-use-package 'flymake-ruby)
  ;; (straight-use-package 'robe-mode)
  (straight-use-package 'srcery-theme)

  ;; LSP packages
  (straight-use-package 'lsp-mode)
  (straight-use-package 'lsp-ui)
  (straight-use-package 'flycheck)
  (straight-use-package '(company-mode :files (:defaults "icons")))
  (straight-use-package 'lsp-treemacs)
  (straight-use-package 'lsp-ivy)
  (straight-use-package 'dap-mode)
  (straight-use-package 'lsp-jedi)
  (straight-use-package 'pyvenv)
  )

;; Init UI and features
;; ----------
(defun init-emacs-ui ()
  "Initialize UX options and Emacs features."

  (defun toggle-fullscreen ()
    "Toggle full screen"
    (interactive)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

  ;; TODO: Enable
  (desktop-save-mode -1)
  (electric-pair-mode -1)            ; Automagically close parenthesis / brackets.
  (delete-selection-mode 1)          ; Deletes content of marked-text when typing.
  (menu-bar-mode -1)
  (show-paren-mode 1)                 ; Highlight matching parenthesis.
  (column-number-mode 1)              ; Display column number
  (tool-bar-mode -1)                  ; No toolbar
  (scroll-bar-mode -1)
  (setq-default
   smerge-command-prefix (kbd "C-c v")
   js2-basic-offset 2
   typescript-indent-level 2
   js-jsx-indent-level 2
   js-indent-level 2
   web-mode-markup-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-css-indent-offset 2)

  (setq
   inhibit-startup-screen t
   require-final-newline nil
   make-backup-files -1
   auto-save-default -1
   backup-by-copying t                                     ; don't clobber symlinks
   backup-directory-alist '(("." . "~/.emacs.d/saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t                                       ; use versioned backups
   explicit-shell-file-name "/bin/bash")

  (global-company-mode)

  ;; Theme
  (load-theme 'srcery t)
  (set-frame-parameter nil 'alpha-background 90)
  (set-face-attribute 'default (selected-frame) :height 90)
  )


;; Global Key bindings.
;; -------------
(defun init-key-bindings ()
  (if window-system
      (global-set-key "\C-z" nil))
  (global-set-key (kbd "C-c r")
		  #'(lambda () (interactive) (load-file "~/.emacs.d/init.el")))
  (global-set-key (kbd "C-c S") 'hs-show-all)
  (global-set-key (kbd "C-c H") 'hs-hide-all)
  (global-set-key (kbd "C-c s") 'hs-show-block)
  (global-set-key (kbd "C-c h") 'hs-hide-block)
  (global-set-key (kbd "C-<tab>") 'hs-toggle-hiding)

  (global-set-key (kbd "C-M-h") 'windmove-left)
  (global-set-key (kbd "C-M-l") 'windmove-right)
  (global-set-key (kbd "C-M-k") 'windmove-up)
  (global-set-key (kbd "C-M-j") 'windmove-down)
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

;; Per package configurations
;; --------------------------
(defun ben-comint-mode ()
  "Make sure windmove shortcuts work in comint-mode"
  )

(defun ben-setup-projectile ()
  "Customize projectile"
  ;; NOTE!!! Use a .projectile file to force a project root.
  (setq projectile-completion-system 'helm)

  (projectile-global-mode)
  (projectile-mode +1)
  (helm-projectile-on)

  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


(defun ben-javascript-config ()
  (setq web-mode-markup-indent-offset 2)
  (setq lsp-enable-indentation nil)

  (add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
  (add-to-list 'lsp-enabled-clients 'ts-ls)

  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'web-mode-hook #'lsp)
  )


(defun ben-lsp-common-config ()
  (require 'lsp-mode)
  (require 'lsp-ui)

  (setq lsp-auto-configure t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-peek-show-directory t)
  (setq lsp-ui-imenu-auto-refresh t)
)

(defun ben-setup-yas-config ()
  (yas-global-mode)
  )

(defun ben-python-config ()
  (require 'auto-virtualenv)
  (require 'lsp-jedi)
  (require 'dap-python)


  (defcustom lsp-ruff-executable "ruff-lsp"
    "Command to start the Ruff language server."
    :group 'lsp-python
    :risky t
    :type 'file)

  ;; Register ruff-lsp with the LSP client.
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection (lambda () (list lsp-ruff-executable)))
    :activation-fn (lsp-activate-on "python")
    :add-on? t
    :server-id 'ruff
    :initialization-options (lambda ()
                              (list :settings
                                    (cl-list*
                                     (when
					 pyvenv-virtual-env
                                       (list
                                        :interpreter (vector (f-join (f-long pyvenv-virtual-env) "bin" "python3"))
                                        :workspace (f-long pyvenv-virtual-env)
                                        :path (vector (f-join (f-long pyvenv-virtual-env) "bin" "ruff")))
                                       )
                                     ))
                              )))

  ;; Override function that incorrectly uses pyenv.
  (defun dap-python--pyenv-executable-find (command) (executable-find command))

  (setq 
   lsp-pylsp-plugins-autopep8-enabled nil
   lsp-pylsp-plugins-black-enabled nil
   lsp-pylsp-plugins-flake8-enabled nil
   lsp-pylsp-plugins-jedi-completion-enabled t
   lsp-pylsp-plugins-mccabe-enabled nil
   lsp-pylsp-plugins-preload-enabled nil
   lsp-pylsp-plugins-pycodestyle-enabled nil
   lsp-pylsp-plugins-pydocstyle-enabled nil
   lsp-pylsp-plugins-pylint-enabled nil
   dap-python-debugger 'debugpy
   )

  (add-to-list 'lsp-disabled-clients 'pyls)
  (add-to-list 'lsp-disabled-clients 'jedi)
  (add-to-list 'lsp-enabled-clients 'pylsp)
  (add-to-list 'lsp-enabled-clients 'ruff)

  (defun configure-python-paths ()
    (auto-virtualenv-set-virtualenv)
    (setq dap-python-executable (concat pyvenv-virtual-env "bin/python"))
    (setq lsp-jedi-executable-command (concat pyvenv-virtual-env "bin/jedi-language-server"))
    )

  (defun ben-python-hook ()
    ;; Settings
    (set-fill-column 88)
    (auto-fill-mode 89)
    (setq tab-width 4)

    ;; Modes
    (display-fill-column-indicator-mode)
    (python-isort-on-save-mode)
    (python-black-on-save-mode-enable-dwim)
    (hs-minor-mode)
    (setq-default fill-column 88)


    ;; Key bindings
    (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
    (local-set-key (kbd "C-c t t") (lambda () (interactive) (dap-debug (dap-python--template "pytest-this-test"))))
    (local-set-key (kbd "C-c t f") (lambda () (interactive) (dap-debug (dap-python--template "Python :: Run pytest (buffer)"))))
    (local-set-key (kbd "C-c t a") (lambda () (interactive) (dap-debug (dap-python--template "pytest-all"))))
    (local-set-key (kbd "C-c t b") (lambda () (interactive) (dap-breakpoint-toggle))))

  (defun ben-setup-lsp ()
    (add-hook 'python-mode-hook #'lsp)
    (dap-register-debug-template "pytest-this-test"
				 (list :type "python-test-at-point"
                                       :args ""
                                       :program nil
                                       :module "pytest"
                                       :request "launch"
                                       :name "pytest-this-test"))
    (dap-register-debug-template "pytest-all"
				 (list :type "python"
                                       :args ""
                                       :program nil
				       :cwd "${workspaceFolder}"
				       :target-module ""
                                       :module "pytest"
                                       :request "launch"
                                       :name "pytest-all")))


  (add-hook 'python-mode-hook 'configure-python-paths)
  (ben-setup-lsp)
  (add-hook 'python-mode-hook 'ben-python-hook)
  (add-hook 'window-configuration-change-hook 'configure-python-paths)
  (add-hook 'projectile-after-switch-project-hook 'configure-python-paths)
  (add-hook 'dap-stopped-hook (lambda (arg) (call-interactively #'dap-hydra)))
  )

(defun ben-go-config ()
  ;; Company mode
  (setq
   company-minimum-prefix-length 1
   whitespace-line-column 200
   lsp-prefer-flymake nil
   company-idle-delay 0)
  (add-to-list 'lsp-enabled-clients 'gopls)

  ;; Go - lsp-mode
  ;; Set up before-save hooks to format buffer and add/delete imports.
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

  ;; Start LSP Mode and YASnippet mode
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'yas-minor-mode)
  (add-hook 'go-mode-hook #'whitespace-mode)
  (add-hook 'go-mode-hook (lambda () (setq-local tab-width 4)))
  )

(defun ben-rust-config ()
  ;; Company mode
  (setq
   company-minimum-prefix-length 1
   whitespace-line-column 200
   lsp-prefer-flymake nil
   company-idle-delay 0)
  (add-to-list 'lsp-enabled-clients 'rust-analyzer)

  ;; Go - lsp-mode
  ;; Set up before-save hooks to format buffer and add/delete imports.
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  ;; Start LSP Mode and YASnippet mode
  (add-hook 'rust-mode-hook #'lsp-deferred)
  (add-hook 'rust-mode-hook #'yas-minor-mode)
  )


(defun ben-ruby-config ()
  (setq
   robe-ruby-path (expand-file-name "~/.emacs.d/straight/repos/robe/lib")
   ruby-indent-tabs-mode nil
   ruby-insert-encoding-magic-comment nil
   ruby-insert-encoding-magic-comment nil
   ruby-deep-indent-paren nil
   flycheck-checker-error-threshold 800)

  (add-to-list 'auto-mode-alist
               '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist
               '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))
  (add-hook 'ruby-mode-hook
            (lambda()
              (setq-default indent-tabs-mode nil)
              (flycheck-mode)
              (flymake-ruby-load)
              (hs-minor-mode)
              (whitespace-mode)
              (set-fill-column 120)
              (robe-mode))))


(defun ben-setup-flycheck ()
  (global-flycheck-mode))

(defun ben-setup-clipboard ()
  (if (not (display-graphic-p))
      (cond 
       ((eq system-type 'gnu/linux)
	(setq wl-copy-process nil)
	(defun wl-copy (text)
	  (setq wl-copy-process (make-process :name "wl-copy"
					      :buffer nil
					      :command '("wl-copy" "-f" "-n")
					      :connection-type 'pipe))
	  (process-send-string wl-copy-process text)
	  (process-send-eof wl-copy-process))
	(defun wl-paste ()
	  (if (and wl-copy-process (process-live-p wl-copy-process))
	      nil ; should return nil if we're the current paste owner
            (shell-command-to-string "wl-paste -n | tr -d \r")))
	(setq interprogram-cut-function 'wl-copy)
	(setq interprogram-paste-function 'wl-paste)
        )
       ((eq system-type 'darwin)
	(defun copy-from-osx ()
	  (shell-command-to-string "pbpaste"))

	(defun paste-to-osx (text &optional push)
	  (let ((process-connection-type nil))
	    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
	      (process-send-string proc text)
	      (process-send-eof proc))))

	(setq interprogram-cut-function 'paste-to-osx)
	(setq interprogram-paste-function 'copy-from-osx))))
  )


(defun ben-setup-misc ()
  (smart-cursor-color-mode +1)

  (add-hook 'css-mode-hook
            (lambda()
	      (rainbow-mode 1)
	      (set-fill-column 120)))

  (add-hook 'comint-mode-hook
            (function (lambda () 
		      (local-set-key (kbd "C-M-h") 'windmove-left)
		      (local-set-key (kbd "C-M-l") 'windmove-right)
		      (local-set-key (kbd "C-M-k") 'windmove-up)
		      (local-set-key (kbd "C-M-j") 'windmove-down)))))


(defun ben-setup-custom-functions ()
  (defun close-all-buffers ()
    (interactive)
    (mapc 'kill-buffer (buffer-list)))
  )

(defun init-package-config ()
  "Configure individual packages"
  (exec-path-from-shell-initialize)
  (ben-setup-yas-config)
  (ben-setup-clipboard)
  (ben-setup-projectile)
  (ben-lsp-common-config)
  (ben-javascript-config)
  (ben-python-config)
  (ben-go-config)
  (ben-rust-config)
  ;; (ben-ruby-config)
  ;; (ben-setup-flycheck)
  (ben-setup-misc)
  (ben-setup-custom-functions)
  )

(defun main ()
  (install-packages)
  (init-emacs-ui)
  (init-key-bindings)
  (init-package-config)
  )

(add-hook 'after-init-hook 'main)
(put 'upcase-region 'disabled nil)
