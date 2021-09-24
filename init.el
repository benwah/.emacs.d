;;; init.el -- Ben's init.el
;;
;; Free
;;
;;; Commentary:
;;
;; Nothing worth mentioning.

;;; Code:

;;; Configurations:

(desktop-save-mode 0)
(electric-pair-mode -1)            ; Automagically close parenthesis / brackets.
(delete-selection-mode 1)          ; Deletes content of marked-text when typing.
(show-paren-mode 1)                 ; Highlight matching parenthesis.
(column-number-mode 1)              ; Display column number
(tool-bar-mode -1)                  ; No toolbar
(scroll-bar-mode -1)
(setq make-backup-files -1)
(setq auto-save-default -1)
(setq require-final-newline nil)
(setq-default smerge-command-prefix (kbd "C-c v"))
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)

;; Backup directory
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;;; Packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("elpy" . "https://jorgenschaefer.github.io/packages/")
                         ))

(unless (require 'use-package nil 'noerror)
  (require 'package)
  (package-refresh-contents)
  (package-initialize)
  (package-install 'use-package)
  (require 'use-package))

(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package magit
  :ensure t
  :init
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

(use-package yaml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package cyberpunk-theme
  :ensure t
  :init
  (load-theme 'cyberpunk t)
  (set-face-attribute 'default nil :height 130 :family "Fira Code")
)

(use-package smart-cursor-color
  :ensure t
  :init
  (require 'smart-cursor-color)
  (smart-cursor-color-mode +1))

(use-package fill-column-indicator
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :init
  (add-hook 'css-mode-hook
            (lambda()
              (rainbow-mode 1)
              (set-fill-column 120)
              (fci-mode t))))


;; Javascript
(use-package js2-mode
  :ensure t
  :init
  (setq auto-mode-alist (cons '("\\.js[mx]?\\'" . js2-mode) auto-mode-alist)))


(use-package jest
  :after (js2-mode)
  :init
  (add-hook 'js2-mode-hook
	    (lambda () (interactive)
	      (jest-minor-mode)
	      (local-set-key (kbd "C-c t f") 'jest-popup))))

;; Python

;; TODO: PEP8, Black, etc.

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package pyenv-mode
  :ensure t
  :config
  (setq exec-path (append exec-path '("~/.pyenv/bin")))
  (add-hook 'python-mode-hook 'pyenv-mode))

(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))

(use-package blacken
  :ensure t)

(use-package python-pytest
  :ensure t
  :init
  (add-hook 'python-mode-hook
	    (lambda ()
	      (local-set-key (kbd "C-c t a") 'python-pytest)
	      (local-set-key (kbd "C-c t f") 'python-pytest-file)
	      (local-set-key (kbd "C-c t t") 'python-pytest-function))))


(add-hook 'python-mode-hook
          (lambda ()
            (auto-fill-mode 80)
            (setq tab-width 4)
            (setq-default fill-column 79)
            (fci-mode t)))

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))

(use-package projectile
  :ensure t
  :init
  (add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)
  (projectile-global-mode))

(use-package helm-projectile
  :ensure t
  :init
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Key bindings.
;; -------------
(if window-system
    (global-set-key "\C-z" nil))
(global-set-key (kbd "C-c r")
                '(lambda () (interactive) (load-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c S") 'hs-show-all)
(global-set-key (kbd "C-c H") 'hs-hide-all)
(global-set-key (kbd "C-c s") 'hs-show-block)
(global-set-key (kbd "C-c h") 'hs-hide-block)

(global-set-key (kbd "C-M-h") 'windmove-left)
(global-set-key (kbd "C-M-l") 'windmove-right)
(global-set-key (kbd "C-M-k") 'windmove-up)
(global-set-key (kbd "C-M-j") 'windmove-down)

;; Override comint mode key binginds
(add-hook 'comint-mode-hook
          (function (lambda () 
                      (local-set-key (kbd "C-M-h") 'windmove-left)
                      (local-set-key (kbd "C-M-l") 'windmove-right)
                      (local-set-key (kbd "C-M-k") 'windmove-up)
                      (local-set-key (kbd "C-M-j") 'windmove-down))))

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(jest js2-mode po-mode vue-mode flycheck helm-ag yaml-mode use-package solarized-theme smart-cursor-color rainbow-mode python-pytest pyenv-mode monokai-theme material-theme markdown-mode magit helm-projectile fill-column-indicator exec-path-from-shell doom-themes cyberpunk-theme color-theme-sanityinc-tomorrow blacken anaconda-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
