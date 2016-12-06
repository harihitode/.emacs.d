(require 'cl-lib)
(require 'gnus)
(require 'nnimap)
(require 'smtpmail)
(require 'bbdb)

(setq read-mail-command 'gnus
      mail-user-agent 'gnus-user-agent)

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
  "Set SMTP variables"
  (setq smtpmail-smtp-server server
        smtpmail-smtp-service port)
  (cond ((eql authmech 'ssl)
         (setq smtpmail-starttls-credentials '((server port nil nil)))
         (message (format "ssl %s:%s" server port)))
        (t
         (message (format "plain %s:%s" server port)))))

(defun change-smtp ()
  "Change the SMTP server according to the current \"From:\""
  (save-excursion
    (cl-loop with
             from = (save-restriction
                           (message-narrow-to-headers)
                           (message-fetch-field "from"))
             for (authmech address server port) in smtp-accounts
             when (string-match (format "%s <%s>" user-full-name address) from)
             do (cl-return (apply 'set-smtp (list authmech server port)))
             finally (error  (format "Cannot infer SMTP information. %s" from)))))

;; ad-hoc change account
(defadvice smtpmail-via-smtp
    (before smtpmail-via-smtp-ad-change-smtp (recipient smtpmail-text-buffer))
  "Call `change-smtp' before every `smtpmail-via-smtp'."
  (with-current-buffer smtpmail-text-buffer (change-smtp)))

(ad-activate 'smtpmail-via-smtp)

;; address settings
(setq bbdb/news-auto-create-p t)
(bbdb-initialize 'gnus 'message)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(add-hook 'message-setup-hook 'bbdb-insinuate-message)
(setq bbdb/news-auto-create-p 'bbdb-ignore-most-messages-hook)
(setq bbdb-send-mail-style 'compose-mail)
(setq bbdb-always-add-addresses t)
(setq bbdb-use-pop-up t)
