;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cl-lib)
(require 'epg)

;; load-path config
(defun add-to-load-path (&rest paths)
  (dolist (path paths)
    (let ((dirname (expand-file-name (concat user-emacs-directory path))))
      (add-to-list 'load-path dirname))))

(add-to-load-path "elisp" "conf")

;; load configuration file
(load "face")
(load "key-binds")
(load "eshell-conf")
(load "eww-conf")
(load "package-conf")

;; linum
(global-linum-mode t)

;; pare completion
(electric-pair-mode 1)

;; save current buffers for emacs restart
(desktop-save-mode 1)

;; remove whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)

;; show white space
(setq whitespace-display-mappings
      '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

(global-whitespace-mode 1)
(setq-default tab-width 2 indent-tabs-mode nil)

;; bell disable
(setq ring-bell-function #'ignore)

;; for Cocoa Emacs
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)

;; UTF-8 settings (making any sense?)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(setq inhibit-startup-message t) ; disable startup-message

;; Hide advertisement from minibuffer
(defun display-startup-echo-area-message () (message ""))

;; Disable the default page and run eshell
(add-hook 'after-init-hook (lambda () (eshell)))

;; PATH configuration
(dolist (dir (list
              "/sbin"
              "/usr/sbin"
              "/bin"
              "/usr/bin"
              "/opt/local/bin"
              "/sw/bin"
              "/usr/local/bin"
              "/usr/local/texlive/2013/bin/x86_64-darwin/"
              (expand-file-name "~/bin")
              (expand-file-name "~/.emacs.d/bin")))
  (when (and (file-exists-p dir) (not (member dir exec-path)))
    (setenv "PATH" (concat dir ":" (getenv "PATH")))
    (setq exec-path (append (list dir) exec-path))))

;;Backup file ~
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backup"))
            backup-directory-alist))
(show-paren-mode 1)
(find-function-setup-keys)
(transient-mark-mode 1)

;; only use c++ mode
(add-to-list 'auto-mode-alist '("\\.c?\\'"  . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h?\\'"  . c++-mode))
(add-to-list 'auto-mode-alist '("\\.pzc?\\'" . c++-mode))

;; cc-mode indent depth
(setq-default my-indent 2)
(add-hook 'c-mode-hook (lambda () (setq c-basic-offset my-indent)))
(add-hook 'c++-mode-hook (lambda () (setq c-basic-offset my-indent)))

;; toggle emacs
(defun my-indent-toggle ()
  (interactive)
  (if (equal my-indent 4) (setq my-indent 2)
    (setq my-indent 4))
  (princ (format "toggle to %d" my-indent)))

;; VC-mode
(defadvice vc-mode-line (after strip-backend () activate)
  (when (stringp vc-mode)
    (let ((noback (replace-regexp-in-string
                   (format "^ %s" (vc-backend buffer-file-name))
                   " " vc-mode)))
      (setq vc-mode noback))))

;; gtags
(package-install-with-refresh 'gtags)
(require 'gtags)
(setq gtags-mode-hook
      '(lambda ()
         (local-set-key "\M-t" 'gtags-find-tag)
         (local-set-key "\M-r" 'gtags-find-rtag)
         (local-set-key "\M-s" 'gtags-find-symbol)
         (local-set-key "\C-t" 'gtags-pop-stack)))

;; Auto-Complete
(package-install-with-refresh 'auto-complete)
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(ac-config-default)

;; twittering-mode
(package-install-with-refresh 'twittering-mode)
(require 'twittering-mode)
(setq twittering-icon-mode t)
(setq twittering-use-master-password t) ; need GnuPG
(setq twittering-status-format "%i《%S(%s)》 %@\n『%t』")

;; paraedit
(package-install-with-refresh 'paredit)
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil ejacs go-mode tabbar elscreen magit tuareg web-mode twittering-mode paredit bbdb auto-complete))))

;; web-mode
(package-install-with-refresh 'web-mode)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.php?\\'"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'"  . web-mode))

;; magit
(package-install-with-refresh 'magit)
(require 'magit)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
