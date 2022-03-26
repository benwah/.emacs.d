;;; early-init.el -- Ben's early-init.el
;;
;; Free
;;
;;; Commentary:
;;
;; Nothing worth mentioning.

(defun init-el-get ()
  "Download and install el-get"
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
  (unless (require 'el-get nil 'noerror)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
  )

;; Packages
;; --------

(defun init-packages ()
  "Initialize packages."

  ;; el-get
  (el-get-bundle elpy)
  (el-get-bundle company-mode)
  (el-get-bundle sublimity)
  (el-get-bundle exec-path-from-shell)
  (el-get-bundle markdown-mode)
  (el-get-bundle pyenv-mode)
  (el-get-bundle python-black)
  (el-get-bundle python-isort)
  (el-get-bundle python-pytest)
  (el-get-bundle rainbow-mode
    (add-hook 'css-mode-hook
              (lambda()
		(rainbow-mode 1)
		(set-fill-column 120)
		(fci-mode t))))
  (el-get-bundle smart-cursor-color
    (require 'smart-cursor-color)
    (smart-cursor-color-mode +1))
  (el-get-bundle yaml-mode)
  (el-get-bundle projectile)
  (el-get-bundle helm-projectile)
  (el-get-bundle js2-mode)
  (el-get-bundle jest)
  (el-get 'sync)

  ;; ELPA
  (package-install 'magit)
  (package-install 'cyberpunk-theme)
  )

(defun main ()
  (init-el-get)
  (init-packages)
  )

(main)
