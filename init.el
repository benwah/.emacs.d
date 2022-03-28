;;; init.el -- Ben's init.el
;;
;; Free
;;
;;; Commentary:
;;
;; Nothing worth mentioning.

;;; Packages:
;; Note! Add packages to early-init.el

(defun main ()
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
  (show-paren-mode 1)                 ; Highlight matching parenthesis.
  (column-number-mode 1)              ; Display column number
  (tool-bar-mode -1)                  ; No toolbar
  (scroll-bar-mode -1)
  (setq-default
   smerge-command-prefix (kbd "C-c v")
   js2-basic-offset 2
   js-indent-level 2)
  (setq
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

  ;; Sulimity provides smooth scroll and minimap
  (require 'sublimity)
  (require 'sublimity-map)
  (sublimity-map-set-delay nil)
  (sublimity-mode 1)

  ;; Theme
  (load-theme 'cyberpunk t)
  (set-face-attribute 'default nil :height 100 :family "Fira Code")
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
  (add-hook 'comint-mode-hook
            (function (lambda () 
			(local-set-key (kbd "C-M-h") 'windmove-left)
			(local-set-key (kbd "C-M-l") 'windmove-right)
			(local-set-key (kbd "C-M-k") 'windmove-up)
			(local-set-key (kbd "C-M-j") 'windmove-down))))
)

(defun ben-setup-projectile ()
  "Customize projectile"
  (setq projectile-completion-system 'helm)

  (projectile-global-mode)
  (projectile-mode +1)
  (helm-projectile-on)

  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


(defun ben-mac-os-only ()
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


(defun ben-javascript-config ()
  (setq auto-mode-alist (cons '("\\.js[mx]?\\'" . js2-mode) auto-mode-alist))
  (add-hook 'js2-mode-hook
	    (lambda () (interactive)
	      (jest-minor-mode)
	      (local-set-key (kbd "C-c t f") 'jest-popup))))

(defun ben-python-config ()
  (elpy-enable)
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
    (pyenv-mode)
    (hs-minor-mode)
    (setq-default fill-column 88)

    ;; Key bindings
    (local-set-key (kbd "C-c t a") 'python-pytest)
    (local-set-key (kbd "C-c t f") 'python-pytest-file)
    (local-set-key (kbd "C-c t t") 'python-pytest-function)
    )

  (add-hook 'python-mode-hook 'ben-python-hook)
)

(defun ben-setup-flycheck ()
  (global-flycheck-mode))

(defun init-package-config ()
  "Configure individual packages"
  (ben-setup-projectile)
  (ben-comint-mode)
  (ben-javascript-config)
  (ben-python-config)
  (ben-setup-flycheck)
  (ben-mac-os-only))

(add-hook 'after-init-hook 'main)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(cyberpunk-theme magit jest smart-cursor-color rainbow-mode python-pytest python-isort pyenv-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
