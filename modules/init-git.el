;;; init-git.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after magit)

(use-package emacsql-sqlite)

(use-package code-review
  :ensure t
  :after forge
  :bind (:map forge-topic-mode-map
              ("C-c r" . 'code-review-forge-pr-at-point)
              :map code-review-feedback-section-map
              ("C-k" . 'code-review-section-delete-comment)
              :map code-review-local-comment-section-map
              ("C-k" . 'code-review-section-delete-comment)
              :map code-review-reply-comment-section-map
              ("C-k" . 'code-review-section-delete-comment)
              :map code-review-mode-map
              ("C-c C-n" . 'code-review-comment-jump-next)
              ("C-c C-p" . 'code-review-comment-jump-previous))
  :custom
  (code-review-auth-login-marker 'forge))

(provide 'init-git)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; init-git.el ends here
