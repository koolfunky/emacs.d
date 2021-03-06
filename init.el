;; Melpa
(require 'package)
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; --------------------------- Packages initialization ---------------------------
;; list the packages to initialize
(setq package-list '(python-environment deferred epc ivy fzf flycheck jedi doom doom-themes elpy
                                        php-mode counsel yasnippet highlight-indentation projectile
                                        sql-indent sql auto-complete magit minimap popup undo-tree
                                        json-mode yaml-mode auto-package-update phpactor
                                        company-phpactor terraform-mode terraform-doc))
;; activate all the packages
(package-initialize)
;; fetch the list of packages available
(unless package-archive-contents (package-refresh-contents))
;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
;; Updates packages
(auto-package-update-maybe)
(add-hook 'auto-package-update-before-hook (lambda ()
                                             (message "I will update packages now")))

;; --------------------------- General ---------------------------
;; Set user venv
(setq user (getenv "USER"))
;; Kill buffer message
(kill-buffer "*Messages*")
;; Smooth scrolling
(setq scroll-step 1)

;; --------------------------- Layout ---------------------------
;; Remove menu bar
(menu-bar-mode -1)
;; Delete when writing over selection
(delete-selection-mode 1)
;; Auto close bracket insertion.
(electric-pair-mode 1)
;; Theme
(load-theme 'doom-one t)
;; Show time in the mode line.
(setq display-time-24hr-format 1)
;; UTF-8
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;; Smooth scrolling
(setq scroll-step 1)
;; Highlight selected region.
(transient-mark-mode 1)

;; --------------------------- Files Binding ---------------------------
;; Use php-mode for `.inc` and `.php`
(add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;; --------------------------- Hooks ---------------------------
;; Trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; --------------------------- Packages configuractions ---------------------------
;; 4 spaces tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Auto-complete
(global-auto-complete-mode t)

;; Ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

;; Phpactor
(with-eval-after-load 'php-mode (define-key php-mode-map (kbd "M-.") #'phpactor-goto-definition)
                      (define-key php-mode-map (kbd "M-?") #'phpactor-find-references))

;; Undo-tree
(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-lazy-drawing nil)
(setq undo-tree-auto-save-history t)
(let ((undo-dir (expand-file-name "undo" user-emacs-directory)))
  (setq undo-tree-history-directory-alist (list (cons "." undo-dir))))

;; --------------------------- Key Bindings ---------------------------
;; Swiper
(global-set-key (kbd "C-s") 'swiper)
;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
;; fzf
(global-set-key (kbd "C-x p f") 'fzf)
;; projectile-ag
(global-set-key (kbd "C-x p a") 'projectile-ag)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ag highlight-parentheses terraform-doc terraform-mode auto-package-update json-mode elisp-format undo-tree counsel ivy fzf projectile-speedbar projectile python-mode php-mode doom-themes doom)))
 '(safe-local-variable-values (quote ((magit-todos-exclude-globs "*.json")))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
