(require 'cl-lib)
(require 'gnus)
(require 'nnimap)
(require 'smtpmail)
(require 'starttls)

(package-install 'bbdb)
(require 'bbdb)

;;;;;;;;;;;;;;;;;;;;;;;;;;MEMO;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Format of `smtp-accounts'
;; (`protocol' `e-mail' `server' `port')
;;
;; User defined key (Actually, I want to use BBDB ...)
;; `user-mail-address'
;; `user-full-name'

(setq smtp-accounts
      '((starttls "your_mail" "mail_server 1" 587)
        (starttls "another_your_mail" "mail_server 2" 587)
        (starttls "yet_another_your_mail" "mail_server 3" 587)))

(setq user-mail-address "your_mail")
(setq user-full-name "your_name")

(setq read-mail-command 'gnus
      mail-user-agent 'gnus-user-agent)

;; encrypted mail
(setq mm-encrypt-option 'guided)
(setq mm-sign-option 'guided)

;; IMAP settings
(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-authinfo-file "~/.emacs.d/.authinfo")
               (nnimap-stream ssl)))

;; SMTP settingsb
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      message.default-mail-headers "Cc: \nBcc:\n")

(defun set-smtp (authmech server port)
  "Set the proper protocol, server and port for sending mail"
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-starttls-credentials '(("smtp.gmail.com" 25 nil nil))
        smtpmail-auth-credentials '(("smtp.gmail.com" 25 "your_gmail@gmail.com" nil))
        smtpmail-default-smtp-server server
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 25
        smtpmail-local-domain "localhost")
  (cond ((eql authmech 'starttls)
         (setq smtpmail-stream-type 'starttls)
         (message (format "starttls %s:%s" server port)))
        ((eql authmech 'ssl)
         (setq smtpmail-stream-type 'ssl)
         (message (format "ssl %s:%s" server port)))
        (t
         (setq smtpmail-stream-type 'plain)
         (message (format "plain %s:%s" server port)))))

(defun message-send-around (f &rest args)
  "Set the smtp server when sending messages"
  (save-excursion
    (cl-loop with
             from = (save-restriction
                           (message-narrow-to-headers)
                           (message-fetch-field "from"))
             for (authmech address server port) in smtp-accounts
             when (string-match (format "%s <%s>" user-full-name address) from)
             do (cl-return (progn
                             (apply 'set-smtp (list authmech server port))
                             (apply f args)))
             finally (error  (format "There is no SMTP information. %s" from)))))

(advice-add #'message-send :around #'message-send-around)

;; address settings
(setq bbdb/news-auto-create-p t)
(bbdb-mail-aliases)
(bbdb-initialize 'gnus 'message 'aliases)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(add-hook 'message-setup-hook 'bbdb-insinuate-message)
(setq bbdb/news-auto-create-p 'bbdb-ignore-most-messages-hook)
(setq bbdb-send-mail-style 'compose-mail)
(setq bbdb-always-add-addresses t)
(setq bbdb-use-pop-up t)
