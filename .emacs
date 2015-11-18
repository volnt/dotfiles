(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path "~/dev/emacs/el-get/recipes")
(setq el-get-verbose t)

;; personal recipes
(setq el-get-sources
      '((:name el-get :branch "master")
        (:name expand-region
               :before (global-set-key (kbd "C-x =") 'er/expand-region))
        (:name magit
               :before (global-set-key (kbd "C-c s") 'magit-status))
        (:name color-theme-solarized
               :description "Emacs highlighting using Ethan Schoonover's Solarized color scheme"
               :type github
               :pkgname "sellout/emacs-color-theme-solarized"
               ;; This recipe works with both color-theme and custom-theme.
               ;; We depend on `color-theme' always, for simplicity.
               :depends color-theme
               :prepare (progn
                          ;; prepare for `custom-theme'
                          (add-to-list 'custom-theme-load-path default-directory)
                          ;; prepare for `color-theme'
                          (autoload 'color-theme-solarized-light "color-theme-solarized"
                            "color-theme: solarized-light" t)
                          (autoload 'color-theme-solarized-dark "color-theme-solarized"
                                                "color-theme: solarized-dark" t)))
))

(global-set-key (kbd "ESC <up>") 'enlarge-window-horizontally)
(global-set-key (kbd "ESC <down>") 'shrink-window-horizontally)
(global-set-key (kbd "C-x f") 'find-file-in-project)
(global-set-key (kbd "C-c b") 'buffer-menu-other-window)
(global-set-key (kbd "C-x =") 'er/expand-region)
(global-set-key (kbd "C-x :") 'goto-last-change)

(global-set-key (kbd "C-c g") 'lgrep)
(global-set-key (kbd "C-c b") 'magit-blame)
(global-set-key (kbd "C-c x") 'magit-reset-hard)


(global-set-key (kbd "C-c r") 'replace-string)

(global-set-key (kbd "C-c m") 'mc/mark-all-in-region)
(global-set-key (kbd "C-c M-m") 'mc/mark-all-in-region-regexp)


;; my packages
(setq volnt-packages
      (append
       ;; list of packages we use straight from official recipes
       '(multiple-cursors anything magit magit-view-file emacs-goodies-el
	 crontab-mode nginx-mode
         python-mode jedi elpy rope flycheck
	 php-mode-improved rainbow-delimiters markdown-mode
	 protobuf-mode paredit)
       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))

(el-get 'sync volnt-packages)

;; remove magit warning message
(setq magit-last-seen-setup-instructions "1.4.0")

;; activate ido-mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; use only spaces for indentation
(setq-default indent-tabs-mode nil)

;; set html-ident to 4
(setq-default sgml-basic-offset 4)

;; init global-rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'python-mode-hook #'rainbow-delimiters-mode)

(undo-tree-mode 1)

;; init flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; init python mode
(setq flycheck-flake8-maximum-line-length 120)

;; column number mode
(setq column-number-mode t)

;; delete trailing whitespaces on save
(setq show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; theme
(load-theme 'solarized t)

;; remove that fucking bell
(setq visible-bell 1)

;; sudo-save
(defun sudo-save ()
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; custom vars
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(flycheck-flake8rc "~/.flake8rc"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(provide '.emacs)
;;; .emacs ends here
(put 'upcase-region 'disabled nil)
