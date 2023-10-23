;;; init-doom-theme.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

(use-package doom-themes
  :ensure t
  :bind
  ("C-x t d" . dark-theme)
  ("C-x t s" . semi-dark-theme)
  ("C-x t l" . light-theme)

  :init
  (defun light-theme ()
    "Activate light colortheme"
    (interactive)
    (load-theme 'doom-one-light)
    (delete-selection-mode 1)
    )

  (defun dark-theme ()
    "Activate dark colortheme"
    (interactive)
    (load-theme 'doom-molokai)
    (delete-selection-mode 1)
    ;; Invoke customcolors
    (darkcolor)
    )

  (defun semi-dark-theme ()
    "Activate semi-dark colortheme"
    (interactive)
    (load-theme 'doom-molokai)
    (delete-selection-mode 1)
    ;; Invoke customcolors
    (semidarkcolor)
    )

  ;; Invoke theme
  (load-theme 'doom-molokai t) ;; global

  :config
  (defun darkcolor ()
    "Simple dark for theme."
    (set-cursor-color "#2979FF")
    (set-face-background 'highlight "#2979FF")
    (set-background-color "#101418")

    ;; Modeline
    (set-face-background 'mode-line "#0C0E10")
    (set-face-background 'modeline-inactive "#333333")
    ;; (set-face-foreground 'mode-line "#FFFFFF")

    ;; Fix linum current-line highlight
    (defface my-linum-hl
      '((t :background "#0C0E10" :foreground "gold"))
      "Face for the currently active Line number"
      :group 'linum)
    )

  (defun semidarkcolor ()
    "Simple semidarkcolor for theme."
    (set-cursor-color "#2979FF")
    (set-face-background 'highlight "#2979FF")
    (set-background-color "#1C1E1F")

    ;; Modeline
    (set-face-background 'mode-line "#2D2E2E")
    (set-face-background 'mode-line-inactive "#333333")
    ;; (set-face-foreground 'mode-line "#FFFFFF")

    ;; Fix linum current-line highlight
    (defface my-linum-hl
      '((t :background "gray20" :foreground "gold"))
      "Face for the currently active Line number"
      :group 'linum)
    )

  ;; Invoke color
  (darkcolor) ;; default
  )

(provide 'init-doom-theme)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; init-doom-theme.el ends here
