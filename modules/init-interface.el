;;; init-interface.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

(use-package ivy
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-j" . ivy-next-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("C-x b" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-osx-app-format-function #'counsel-osx`-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

;; Trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Auto close bracket insertion.
(electric-pair-mode 1)

;; Smooth scrolling
(setq scroll-step 1)

(provide 'init-interface)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; init-interface.el ends here
