;;; package --- Summary
;;; Commentary:
;;; Look & Feel
;;; whitespace check
(load-theme 'tango-dark)

;;; Code:

(require 'whitespace)
(setq whitespace-style '(face
                         trailing
                         tabs
                         empty
                         space-mark
                         tab-mark
                         ))
;;; show white space
(setq whitespace-display-mappings
      '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
(global-whitespace-mode 1)

;;; colors to check the white space (always it's a gomi)
(let ((bg-color nil))
  (set-face-attribute 'whitespace-trailing nil
                      :background bg-color
                      :foreground "DeepPink"
                      :underline t)
  (set-face-attribute 'whitespace-tab nil
                      :background bg-color
                      :foreground "LightSkyBlue"
                      :underline t)
  (set-face-attribute 'whitespace-space nil
                      :background bg-color
                      :foreground "LightSkyBlue")
  (set-face-attribute 'whitespace-empty nil
                      :background bg-color))

;;; appearance: remove unnecessary junks
(if window-system (progn
                    (set-frame-parameter nil 'alpha 95)
                    (tool-bar-mode 0) ; toolbar (upper) disable
                    (scroll-bar-mode 0) ; scrollbar disable
                    (horizontal-scroll-bar-mode 0) ; also horizontal one
                    (global-set-key (kbd "C-z") #'eshell)
                    (set-face-attribute 'mode-line nil
                                        :box nil ; disable 3d effect
                                        :foreground "black"
                                        :background "#65ace4"
                                        :overline nil
                                        :underline nil)
                    (set-face-attribute 'mode-line-inactive nil
                                        :box nil
                                        :foreground "grey30"
                                        :background "pink"
                                        :overline nil
                                        :underline nil)))
(menu-bar-mode 0)
;;; Mode Line Setting
(setq-default mode-line-format (list "%b %3l行:%3c列 も〜ど: %m"))

(provide 'face)
;;; face.el ends here
