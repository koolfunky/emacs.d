;;; init.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Emacs!!!

;;; Code:

(setq inhibit-startup-message t)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(tooltip-mode -1)       ; Disable tooltip

(menu-bar-mode -1)      ; Disable menu bar (except if its a fish)

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;;; Modules directory
(push (concat user-emacs-directory "modules") load-path)

(require 'init-interface)
(require 'init-doom-theme)
(require 'init-git)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom (
	   (projectile-completion-system 'ivy)
	   (projectile-globally-ignored-directories '(".git"
						      "cache"
						      "coverage"
						      "elpa"
						      "elpaca"
						      "node_modules"
						      "tmp"))
	   )
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package vundo
  :bind ("C-x u" . vundo))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package company
  ;; Navigate in completion minibuffer with `C-n` and `C-p`.
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  ;; Provide instant autocompletion.
  (setq company-idle-delay 0.0)

  ;; Use company mode everywhere.
  (global-company-mode t))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; if you use typescript-mode
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))


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

;; if you use typescript-mode
(add-hook 'typescript-mode-hook #'setup-tide-mode)


(use-package docker-compose-mode)

;; Temporarily remove warning about docker-tramp deprecation in Emacs 29.x.
(when (not (version< emacs-version "29.0"))
  (with-eval-after-load 'tramp-compat
    (assq-delete-all 'docker-tramp after-load-alist)))

(use-package docker
  :defer t)

(use-package dotenv-mode
  :mode ("\\.env\\'" . dotenv-mode))

(use-package kubel
  :after (vterm)
  :config (kubel-vterm-setup))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" default))
 '(package-selected-packages
   '(dotenv-mode doom-themes kubel code-review tide vundo rainbow-delimiters docker docker-compose-mode auto-package-update typescript-mode counsel-projectile forge magit doom-modeline counsel ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
