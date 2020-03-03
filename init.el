;; Melpa
(require 'package)
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; --------------------------- Packages initialization ---------------------------
;; list the packages to initialize
(setq package-list '(python-environment deferred epc ivy fzf flycheck jedi doom doom-themes elpy
                                        php-mode counsel yasnippet highlight-indentation projectile
                                        sql-indent sql auto-complete magit minimap popup undo-tree))
;; activate all the packages
(package-initialize)
;; fetch the list of packages available
(unless package-archive-contents (package-refresh-contents))
;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; --------------------------- Layout ---------------------------
;; Remove menu bar
(menu-bar-mode -1)
;; Theme
(load-theme 'doom-one t)

;; --------------------------- Files Binding ---------------------------
;; Use php-mode for `.inc` and `.php`
(add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;; --------------------------- Hooks ---------------------------
;; Trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; --------------------------- Packages configuractions ---------------------------
;; Ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

;;; undo-tree
(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-lazy-drawing nil)
(setq undo-tree-auto-save-history t)
(let ((undo-dir (expand-file-name "undo" user-emacs-directory)))
  (setq undo-tree-history-directory-alist (list (cons "." undo-dir))))

;; 4 spaces tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; --------------------------- Key Bindings ---------------------------
;; Swiper
(global-set-key (kbd "C-s") 'Swiper)
;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (elisp-format undo-tree counsel ivy fzf projectile-speedbar
                                                  projectile python-mode php-mode doom-themes
                                                  doom)))
 '(safe-local-variable-values (quote ((magit-todos-exclude-globs "*.json")))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
