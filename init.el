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

(require 'cl-lib)

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
 '(desktop-restore-frames nil)
 '(desktop-save t)
 '(package-selected-packages
   (quote
    (ggtags company-quickhelp company flycheck company-lsp lsp-ui lsp-mode company-flx magit tuareg org-caldav alert web-mode twittering-mode paredit bbdb))))
(desktop-save-mode 1)

;; linum
(global-linum-mode t)

;; pare completion
(electric-pair-mode 1)

;; remove whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)

;; show white space
(setq whitespace-display-mappings
      '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

(global-whitespace-mode 1)
(setq-default tab-width 2 indent-tabs-mode nil)

;; bell disable
(setq ring-bell-function #'ignore)

;; UTF-8 settings (making any sense?)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8-unix)
(setq inhibit-startup-message t) ; disable startup-message

;; Hide advertisement from minibuffer
(defun display-startup-echo-area-message () (message ""))

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

(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backup"))
            backup-directory-alist))
(show-paren-mode 1)
(find-function-setup-keys)
(transient-mark-mode 1)

;; only use c++ mode
(add-to-list 'auto-mode-alist '("\\.c?\\'"  . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cl?\\'"  . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h?\\'"  . c++-mode))
(add-to-list 'auto-mode-alist '("\\.pzc?\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cl?\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.td?\\'" . c++-mode))

;; web-mode
(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts?\\'" . web-mode))

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
(package-install 'ggtags)
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (ggtags-mode 1))))

;; twittering-mode
(require 'epg)
(package-install 'twittering-mode)
(require 'twittering-mode)
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
(setq twittering-status-format "%i《%S(%s)》 %@\n『%t』")

;; paraedit
(package-install 'paredit)
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)



;; magit
(package-install 'magit)
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; web-mode
(package-install 'web-mode)
(require 'web-mode)
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.php?\\'"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'"  . web-mode))

;; alert
(package-install 'alert)
(require 'alert)
(setq alert-default-style 'toaster)
;; if there is no png file, toast notification does not worv
(setq alert-toaster-default-icon "~/.emacs.d/emacs.png")

;; configure gnus-settings which use bbdb
(load "gnus-conf")

;; TeX spel check
(setq-default ispell-program-name "aspell")

(defun my-packages-init ()
  ;; why...
  (add-to-load-path "elisp" "conf"))

(add-hook 'after-init-hook 'my-packages-init)

;; org-caldav
(package-install 'org-caldav)
(require 'org-caldav)
(setq tls-checktrust 'ask)
(setq org-caldav-url  "https://example.example/remote.php/dav/calendars/user_id")
(setq org-caldav-calendar-id "personal")
(setq org-caldav-inbox "~/.emacs.d/cal_inbox.org")
(setq org-caldav-files '("~/.emacs.d/cal_inbox.org"))
(setq org-agenda-files '("~/.emacs.d/cal_inbox.org"))
(setq org-default-priority 68) ; make default priority the lowest
(setq org-icalendar-include-todo t
      org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due)
      org-icalendar-use-scheduled '(event-if-todo event-if-not-todo todo-start)
      org-icalendar-with-timestamps t)

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

(setq org-latex-with-hyperref nil)

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

(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode t))

(use-package company
  :ensure t
  :defer t
  :init (global-company-mode t)
  :config
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
  :defer t
  :ensure t
  :commands lsp
  :config
  (setq lsp-log-io nil
        lsp-auto-configure t
        lsp-auto-guess-root t
        lsp-enable-completion-at-point t
        lsp-enable-xref t
        lsp-enable-indentation t
        lsp-response-timeout 5
        lsp-restart 'auto-restart
        lsp-keep-workspace-alive t
        lsp-eldoc-render-all nil
        lsp-enable-snippet nil
        lsp-enable-folding t)
   ;;; lsp-ui gives us the blue documentation boxes and the sidebar info
  (use-package lsp-ui
    :defer t
    :ensure t
    :after lsp
    :commands lsp-ui-mode
    :config
    (setq lsp-ui-sideline-ignore-duplicate t
          lsp-ui-sideline-delay 0.5
          lsp-ui-sideline-show-symbol t
          lsp-ui-sideline-show-hover t
          lsp-ui-sideline-show-diagnostics t
          lsp-ui-sideline-show-code-actions t
          lsp-ui-peek-always-show t
          lsp-ui-doc-use-childframe t)
    :bind
    (:map lsp-ui-mode-map
          ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
          ([remap xref-find-references] . lsp-ui-peek-find-references))
    :hook
    ((lsp-mode . lsp-ui-mode)
     (lsp-after-open . (lambda ()
                         (lsp-flycheck-enable t)
                         (lsp-ui-sideline-enable t)
                         (lsp-ui-imenu-enable t)
                         (lsp-lens-mode t)
                         (lsp-ui-peek-enable t)
                         (lsp-ui-doc-enable t)))))
  ;;; company lsp
  ;; install LSP company backend for LSP-driven completion
  (use-package company-lsp
    :defer t
    :ensure t
    :after company
    :commands company-lsp
    :config
    (setq company-lsp-cache-candidates t
          company-lsp-enable-recompletion t
          company-lsp-enable-snippet t
          company-lsp-async t)
    ;; avoid, as this changes it globally do it in the major mode instead (push
    ;; 'company-lsp company-backends) better set it locally
    :hook (lsp-after-open . (lambda()
                              (add-to-list (make-local-variable 'company-backends)
                                           'company-lsp)))))

(use-package verilog-mode
  :defer t
  :config
  (require 'lsp)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("svls"))
                    :major-modes '(verilog-mode)
                    :priority -1
                    ))
  :hook (verilog-mode . (lambda()
                          (lsp)
                          (flycheck-mode t)
                          (add-to-list 'lsp-language-id-configuration '(verilog-mode . "verilog")))))

(provide 'init)
;;; init.el
