;;; package --- Summary
;;; Commentary:
;;; Added by Package.el.  This must come before configurations of
;;; installed packages.  Don't delete this line.  If you don't want it,
;;; just comment it out by adding a semicolon to the start of the line.
;;; You may delete these explanatory comments.

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-refresh-contents)
(package-initialize)
(package-install 'use-package)

;; load-path config
(defun add-to-load-path (&rest paths)
  "Add each directory in PATHS to load path from which Emacs Lisps are loading."
  (dolist (path paths)
    (let ((dirname (expand-file-name (concat user-emacs-directory path))))
      (add-to-list 'load-path dirname))))

(add-to-load-path "elisp" "conf" "verilog-mode")
(add-hook 'after-init-hook (lambda () (add-to-load-path "elisp" "conf" "verilog-mode")))

;; load configuration file
(load "face")
(load "key-binds")
(load "eshell-conf")
(load "eww-conf")
(load "gnus-conf")

;; move home directory
(cd "~/")

;; save current buffers for emacs restart
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(desktop-dirname "~/.emacs.d" t)
 '(desktop-path (list "~/.emacs.d/"))
 '(desktop-restore-frames t)
 '(desktop-save t)
 '(package-selected-packages
   '(use-package twittering-mode company-irony ggtags company-quickhelp company flycheck company-lsp lsp-ui lsp-mode company-flx magit tuareg alert web-mode paredit bbdb))
 '(verilog-align-ifelse t)
 '(verilog-auto-arg-format 'single)
 '(verilog-auto-arg-sort t)
 '(verilog-auto-delete-trailing-whitespace t)
 '(verilog-auto-inst-param-value t)
 '(verilog-auto-inst-vector nil)
 '(verilog-auto-lineup 'declarations)
 '(verilog-auto-newline nil)
 '(verilog-auto-save-policy nil)
 '(verilog-auto-template-warn-unused t)
 '(verilog-case-fold t)
 '(verilog-case-indent 2)
 '(verilog-cexp-indent 2)
 '(verilog-highlight-grouping-keywords t)
 '(verilog-highlight-modules t)
 '(verilog-indent-level 2)
 '(verilog-indent-level-behavioral 2)
 '(verilog-indent-level-declaration 2)
 '(verilog-indent-level-module 2)
 '(verilog-tab-to-comment nil)
 '(verilog-typedef-regexp "_t$"))

;; linum
(global-linum-mode t)

;; saving emacs sessions
(desktop-save-mode 1)

;; pare completion
(electric-pair-mode 1)

;; remove whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)

;; bell disable
(setq ring-bell-function #'ignore)

;; UTF-8 settings (making any sense?)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8-unix)
(setq inhibit-startup-message t) ; disable startup-message

;; Hide advertisement from minibuffer
(defun display-startup-echo-area-message ()
  "Hide default minibuffer message." (message ""))

;; Disable the default page and run eshell
(add-hook 'after-init-hook (lambda () (eshell)))

;; PATH configuration
;; for mac
(when (equal system-type 'darwin)
  (dolist (dir (list
                "/sbin"
                "/usr/sbin"
                "/bin"
                "/usr/bin"
                "/opt/local/bin"
                "/sw/bin"
                "/usr/local/bin"
                (expand-file-name "~/bin")
                (expand-file-name "~/.emacs.d/bin")))
    (when (and (file-exists-p dir) (not (member dir exec-path)))
      (setenv "PATH" (concat dir ":" (getenv "PATH")))
      (setq exec-path (append (list dir) exec-path)))))

;; for windows
(setq exec-path (add-to-list 'exec-path "c:/Program Files (x86)/OpenSSH-Win64"))
(setq exec-path (add-to-list 'exec-path "c:/Program Files/Git/bin"))

(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backup"))
            backup-directory-alist))
(show-paren-mode 1)
(find-function-setup-keys)
(transient-mark-mode 1)

;; only use c++ mode
(add-to-list 'auto-mode-alist '("\\rc?\\'"  . sh-mode))
(add-to-list 'auto-mode-alist '("\\.c?\\'"  . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cl?\\'"  . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h?\\'"  . c++-mode))
(add-to-list 'auto-mode-alist '("\\.pzc?\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cl?\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.td?\\'" . c++-mode))

;; tab & indent
(setq-default tab-width 2 indent-tabs-mode nil)
(add-hook 'c-mode-hook (lambda () (setq-default c-basic-offset tab-width)))
(add-hook 'c++-mode-hook (lambda () (setq-default c-basic-offset tab-width)))

;; toggle tab width
(defun toggle-tab-width ()
  "Set the tab width 2 or 4."
  (interactive)
  (setq tab-width (if (equal tab-width 2) 4 2))
  (princ (format "toggle to %d" tab-width)))

;; gtags
(use-package ggtags
  :defer t
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode)
                (ggtags-mode 1)))))

;; twittering-mode
(use-package twittering-mode
  :after epg
  :disabled t
  :commands twittering-mode
  :config
  (setq twittering-icon-mode t)
  (setq twittering-use-master-password t) ; need GnuPG
  (setq twittering-connection-type-order '(wget curl))
  (setq twittering-connection-type-table
        '((wget
           (check . twittering-start-http-session-wget-p)
           (https . t)
           (send-http-request . twittering-send-http-request-wget)
           (pre-process-buffer . twittering-pre-process-buffer-wget))
          (curl
           (check . twittering-start-http-session-curl-p)
           (https . twittering-start-http-session-curl-https-p)
           (send-http-request . twittering-send-http-request-curl)
           (pre-process-buffer . twittering-pre-process-buffer-curl))))
  (setq twittering-allow-insecure-server-cert t)
  (setq twittering-cert-file nil)
  (setq twittering-status-format "%i《%S(%s)》 %@\n『%t』"))

;; paraedit
(use-package paredit
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  ;; interactive emacs lisp mode
  (add-hook 'ielm-mode-hook 'enable-paredit-mode))

;; magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; web-mode
(use-package web-mode
  :ensure t
  :mode (("\\.php?\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.css?\\'" . web-mode)
         ("\\.js?\\'" . web-mode)
         ("\\.ts?\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;; alert
(use-package alert
  :defer t
  :disabled t
  :config
  (setq alert-default-style 'toaster)
  ;; if there is no png file, toast notification does not work
  (setq alert-toaster-default-icon "~/.emacs.d/emacs.png"))

;; TeX spel check
(setq-default ispell-program-name "aspell")

;; org-caldav
(use-package org-caldav
  :disabled t
  :config
  (setq org-caldav-url  "https://example.example/remote.php/dav/calendars/user_id")
  (setq org-caldav-calendar-id "personal")
  (setq org-caldav-inbox "~/.emacs.d/cal_inbox.org")
  (setq org-caldav-files '("~/.emacs.d/cal_inbox.org"))
  (setq org-agenda-files '("~/.emacs.d/cal_inbox.org"))
  (setq org-default-priority 68) ; make default priority the lowest
  (setq org-icalendar-include-todo t
        org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due)
        org-icalendar-use-scheduled '(event-if-todo event-if-not-todo todo-start)
        org-icalendar-with-timestamps t))

;; latex
(require 'org)
(require 'ox-latex)

(when (equal system-type 'darwin)
  (setq org-latex-pdf-process
        '("/usr/local/texlive/2016basic/bin/x86_64-darwin/latex %f"
          "/usr/local/texlive/2016basic/bin/x86_64-darwin/latex %f"
          "/usr/local/texlive/2016basic/bin/x86_64-darwin/dvipdfmx %b"
          "/usr/local/texlive/2016basic/bin/x86_64-darwin/dvipdfmx %b")))


(make-face 'emphasis-face-red)
(set-face-foreground 'emphasis-face-red "red")
(font-lock-add-keywords 'org-mode
                        '(
                          ("\\\\begin{.*}" . 'emphasis-face-red)
                          ("\\\\end{.*}" . 'emphasis-face-red)
                          ))

(setq org-latex-hyperref-template nil)

(add-to-list 'org-latex-classes
             '("report"
               "\\documentclass[11pt,a4paper,uplatex]{jsarticle}
[NO-PACKAGES]
[NO-DEFAULT-PACKAGES]
\\usepackage[dvipdfmx]{graphicx}
\\usepackage{amsmath,amssymb}
\\usepackage{bm}
\\usepackage[compact]{titlesec}
\\usepackage{url}
\\makeatletter
\\renewcommand\\maketitle[0]{
\\begin{center}
\\@title
\\end{center}
}
\\makeatother
\\setlength{\\textwidth}{\\fullwidth}
\\setlength{\\textheight}{\\textheight}
\\addtolength{\\textheight}{40\\baselineskip}
\\setlength{\\voffset}{-0.2in}
\\setlength{\\topmargin}{0pt}
\\setlength{\\headheight}{0pt}
\\setlength{\\headsep}{0pt}
"))

(add-to-list 'org-latex-classes '("normal" "
\\documentclass[11pt,a4paper]{article}
[NO-PACKAGES]
[NO-DEFAULT-PACKAGES]
\\usepackage[dvipdfmx]{graphicx}
\\usepackage{amsmath,amssymb}
\\usepackage{bm}
\\usepackage{ulem}
\\usepackage{url}
\\usepackage{geometry}
\\geometry{left=25mm,right=25mm,top=30mm,bottom=30mm}
\\makeatletter
\\renewcommand\\maketitle[0]{
\\begin{center}
{\\bf \\@title}
\\end{center}
\\rightline{
\\today
}
}
\\makeatother
"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package irony
  :commands irony-mode
  :init
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  :config
  ;; set compile options for c/c++
  (add-hook 'c-mode-hook
            '(lambda ()
               (setq irony-additional-clang-options '("-std=c11" "-Wall" "-Wextra"))))
  (add-hook 'c++-mode-hook
            '(lambda ()
               (setq irony-additional-clang-options '("-std=c++17" "-Wall" "-Wextra"))))
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  ;; Windows環境でパフォーマンスを落とす要因を回避.
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0))
  ;; バッファサイズ設定(default:4KB -> 64KB)
  (when (boundp 'w32-pipe-buffer-size)
    (setq irony-server-w32-pipe-buffer-size (* 64 1024))))

(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode t))

(use-package company
  :ensure t
  :defer t
  :init (global-company-mode t)
  :config
  ;; Company Irony for C++
  (use-package company-irony
    :ensure t
    :disabled t
    :after company
    :config
    (add-to-list 'company-backends '(company-irony-c-headers company-irony)))
  ;; Company Flx adds fuzzy matching to company, powered by the sophisticated
  ;; sorting heuristics  in =flx=
  (use-package company-flx
    :ensure t
    :after company
    :init (company-flx-mode t))
  ;; Company Quickhelp
  ;; When idling on a completion candidate the documentation for the
  ;; candidate will pop up after `company-quickhelp-delay' seconds.
  (use-package company-quickhelp
    :after company
    :ensure t
    ;; :init (company-quickhelp-mode t)
    :hook (prog-mode . (lambda ()
                         (when (window-system)
                           (company-quickhelp-local-mode))))
    :config
    (setq company-quickhelp-delay 0.2
          company-quickhelp-max-lines nil)))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom ((lsp-auto-configure t)
           (lsp-inhibit-message t)
           (lsp-auto-guess-root t)
           (lsp-completion-enable t)
           (lsp-enable-xref t)
           (lsp-response-timeout 10)
           (lsp-restart 'auto-restart)
           (lsp-keep-workspace-alive t)
           (lsp-eldoc-render-all nil)
           (lsp-enable-snippet nil)
           (lsp-enable-folding t))
  :hook (prog-major-mode . lsp-prog-major-mode-enable))

(use-package lsp-ui
  :after lsp-mode
  :ensure t
  :custom ((lsp-ui-sideline-ignore-duplicate t)
           (lsp-ui-sideline-delay 0.5)
           (lsp-ui-sideline-show-symbol t)
           (lsp-ui-sideline-show-hover t)
           (lsp-ui-sideline-show-diagnostics t)
           (lsp-ui-sideline-show-code-actions t)
           (lsp-ui-peek-always-show t)
           (lsp-ui-doc-use-childframe t))
  :hook (lsp-mode . lsp-ui-mode))

;; Company Backends for Language Server Protocol
(use-package company-lsp
  :ensure t
  :after (lsp-mode company)
  :defines company-backends
  :commands company-lsp
  :custom ((company-lsp-cache-candidates t)
           (company-lsp-enable-recompletion t)
           (company-lsp-enable-snippet t)
           (company-lsp-async t))
  :hook (lsp-after-open . (lambda()
                            (add-to-list (make-local-variable 'company-backends) 'company-lsp))))

(use-package verilog-mode
  :after lsp-mode
  :ensure t
  :commands verilog-mode
  :functions (lsp-register-client make-lsp-client lsp-stdio-connection)
  :config
  (lsp-register-client
         (make-lsp-client :new-connection (lsp-stdio-connection '("svls"))
                          :major-modes '(verilog-mode)
                          :priority -1))
  :hook (verilog-mode . (lambda()
                          (lsp)
                          (flycheck-mode t)
                          (add-to-list 'lsp-language-id-configuration '(verilog-mode . "verilog")))))

(provide 'init)
;;; init.el ends here
