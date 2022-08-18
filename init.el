;;; init.el -- Ben's init.el
;;
;; Free
;;
;;; Commentary:
;;
;; Nothing worth mentioning.

;;; Packages:

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
  (straight-use-package 'elpy)
  (straight-use-package 'flycheck)
  (straight-use-package 'company-mode)
  (straight-use-package 'exec-path-from-shell)
  (straight-use-package 'markdown-mode)
  (straight-use-package 'python-black)
  (straight-use-package 'python-isort)
  (straight-use-package 'python-pytest)
  (straight-use-package 'poetry)
  (straight-use-package 'rainbow-mode)
  (straight-use-package 'smart-cursor-color)
  (straight-use-package 'yaml-mode)
  (straight-use-package 'helm)
  (straight-use-package 'helm-ag)
  (straight-use-package 'projectile)
  (straight-use-package 'helm-projectile)
  (straight-use-package 'js2-mode)
  (straight-use-package 'jest)
  (straight-use-package 'tide)
  (straight-use-package 'web-mode)
  (straight-use-package 'cython-mode)
  (straight-use-package 'robe-mode)
  (straight-use-package 'srcery-theme)
  )

(defun main ()
  (install-packages)
  (init-emacs-ui)
  (init-key-bindings)
  (init-package-config)
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
   js-indent-level 2)
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
   version-control t)                                      ; use versioned backups

  (global-company-mode)

  ;; Theme
  (load-theme 'srcery t)
  (set-frame-parameter nil 'alpha-background 70)
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
  (setq projectile-completion-system 'helm)

  (projectile-global-mode)
  (projectile-mode +1)
  (helm-projectile-on)

  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


(defun ben-javascript-config ()
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)

  (add-hook 'typescript-mode-hook #'setup-tide-mode)

  (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
	    (lambda ()
	      (when (string-equal "tsx" (file-name-extension buffer-file-name))
		(setup-tide-mode))))
  )

(defun ben-python-config ()
  (require 'poetry)

  ;; Make elpy work properly.
  (setq elpy-rpc-virtualenv-path 'current)
  (elpy-enable)
  (add-hook 'pyvenv-post-activate-hooks
	    (lambda ()
	      (setq elpy-rpc-python-command (format "%s/bin/python" python-shell-virtualenv-path))))

  (poetry-tracking-mode)
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))

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
    (local-set-key (kbd "C-c t a") 'python-pytest)
    (local-set-key (kbd "C-c t f") 'python-pytest-file)
    (local-set-key (kbd "C-c t t") 'python-pytest-function)
    )
  (add-hook 'python-mode-hook 'ben-python-hook))


(defun ben-setup-flycheck ()
  (global-flycheck-mode))

(defun ben-setup-wl-clipboard ()
  (unless window-system
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
    (setq interprogram-paste-function 'wl-paste))
  )

(defun ben-setup-misc ()
  (smart-cursor-color-mode +1)

  (add-hook 'css-mode-hook
            (lambda()
	      (rainbow-mode 1)
	      (set-fill-column 120)
	      (fci-mode t)))

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
  (ben-setup-wl-clipboard)
  (ben-setup-projectile)
  (ben-javascript-config)
  (ben-python-config)
  (ben-setup-flycheck)
  (ben-setup-misc)
  (ben-setup-custom-functions)
  )

(add-hook 'after-init-hook 'main)
