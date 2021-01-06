;;; package --- Summary
;;; Commentary:
;;; setting key bindings

;;; Code:

(keyboard-translate ?\C-h ?\C-?)
(global-set-key (kbd "C-h") nil)
(global-set-key (kbd "C-m") #'newline-and-indent)
(global-set-key (kbd "C-o") #'other-window)
(global-set-key (kbd "C-r") #'replace-string)
(global-set-key (kbd "C-u") #'undo)
(global-set-key (kbd "C-x g") #'magit-status)
(global-set-key (kbd "C-t") nil)
(global-set-key (kbd "M-p") nil)
(global-set-key (kbd "M-/") #'comment-line)
(global-set-key (kbd "C-y") (lambda ()
                              (interactive)
                              (yank)
                              (indent-region (region-beginning) (region-end))))
(global-set-key (kbd "C-c :") #'org-caldav-sync)
(global-set-key (kbd "C-c g") #'gnus)

(provide 'key-binds)
;;; key-binds.el ends here
