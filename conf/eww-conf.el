;;; package --- Summary
;;; Commentary:
;;; EWW (text browser) settings
;;; Code:

(require 'eww)
(define-key eww-mode-map "c 0" 'eww-copy-page-url)
(define-key eww-mode-map "p" 'scroll-page-url)
(define-key eww-mode-map "n" 'scroll-up)

(defvar eww-disable-colorize t)
(defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
  (unless eww-disable-colorize
    (funcall orig start end fg)))
(advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
(advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)
(defun eww-disable-color ()
  "Disable text color of the eww buffer."
  (interactive)
  (setq-local eww-disable-colorize t)
  (eww-reload))
(defun eww-enable-color ()
  "Enable text color of the eww buffer."
  (setq-local eww-disable-colorize nil)
  (eww-reload))

(defun eww-disable-images ()
  "To show images in the eww buffer."
  (interactive)
  (setq-local shr-put-image-function 'shr-put-image-alt)
  (eww-reload))
(defun eww-enable-images ()
  "Not to show images in the eww buffer."
  (interactive)
  (setq-local shr-put-image-function 'shr-put-image)
  (eww-reload))

(defun shr-put-image-alt (spec alt &optional flags)
  (insert alt))
(defun eww-mode-hook--disable-image ()
  (setq-local shr-put-image-function 'shr-put-image-alt))
(add-hook 'eww-mode-hook 'eww-mode-hook--disable-image)

(setq-default eww-search-prefix "https://www.google.co.jp/search?q=")
;;; eww-conf.el ends here
