;;; .emacs --- Emacs configuration file
;;;
;;; Commentary:
;;;     Definition of the packages needed for my configuration
;;;
;;; Code:
;;;    Installation of the needed packages

;;;
;;;  el-get init
;;;

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (goto-char (point-max))
     (eval-print-last-sexp))))
(el-get 'sync)
(setq el-get-verbose t)

(setq el-get-sources
      '((:name el-get :branch "master")
        (:name magit
               :before (progn
                         (global-set-key (kbd "C-c b") 'magit-blame)
                         (global-set-key (kbd "C-c x") 'magit-reset-hard)
                         (global-set-key (kbd "C-c s") 'magit-status))
               :after (setq magit-last-seen-setup-instructions "1.4.0"))
        (:name flycheck
               :after (progn
                        (add-hook 'after-init-hook #'global-flycheck-mode)
                        (setq flycheck-flake8-maximum-line-length 120)))
        (:name undo-tree
               :after (global-undo-tree-mode))
        (:name projectile
               :before (global-set-key (kbd "C-x f") 'projectile-find-file)
               :after (projectile-global-mode))
        (:name goto-last-change
               :before (global-set-key (kbd "C-x :") 'goto-last-change))
        (:name color-theme-solarized
               :after (load-theme 'solarized t))
        (:name multiple-cursors
               :before (global-set-key (kbd "C-c m") 'mc/mark-all-in-region))
	(:name rainbow-delimiters
	       :after (progn
			(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
			(add-hook 'python-mode-hook #'rainbow-delimiters-mode)))
        ))

;;; Define & install my packages
(setq my-packages
      (append
       '(magit-view-file
         sudo-save

         ;;; Emacs Modes
         crontab-mode
         dockerfile-mode
         go-mode
         json-mode
         markdown-mode
         nginx-mode
         python-mode
         yaml-mode)
       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))
(el-get 'sync my-packages)

;;;
;;;  Configuration
;;;

;;; Indent and offset configuration
(setq-default indent-tabs-mode nil)
(setq-default sgml-basic-offset 4)

;;; Add the column in the power bar
(setq column-number-mode t)

;;; Show & delete trailing whitespaces on save
(setq show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Enable ido-mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(provide '.emacs)
;;; .emacs ends here
