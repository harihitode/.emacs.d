;;; package --- Summary
;;; Commentary:
;;; Less command for eshell
;;; Code:

(require 'em-dirs)
(defun eshell/less (&rest args)
  "Invoke `view-file' on the file in ARGS.  For example, you can view at the line 42 of the file with calling \"less +42 foo\"."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (view-file file)
          (forward-line line))
      (view-file (pop args)))))

;; Remote cd command for eshell
(defun eshell/rcd (&optional hostname dir)
  "Change directory DIR of the remote host HOSTNAME."
  (let ((dir (if dir dir "~/")))
    (cond (hostname (eshell/cd (format "/%s:%s" hostname dir)))
          (t (eshell/cd dir)))))

;; PATH setting
(require 'em-alias)
(when (equal system-type 'darwin)
  (add-to-list 'exec-path "/usr/local/bin")
  (add-to-list 'exec-path "/usr/local/texlive/2016basic/bin/x86_64-darwin"))

(add-hook 'eshell-mode-hook
          (lambda()(dolist (path exec-path)
                     (let ((newpath (concat path ":")))
                       (setq eshell-path-env (concat newpath eshell-path-env))
                       (setenv "PATH" (concat newpath (getenv "PATH")))))))

(setq eshell-path-env exec-path)
(mapc (lambda (alias) (add-to-list 'eshell-command-aliases-list alias))
      `(("ll" "ls -ltr")
        ("la" "ls -a")
        ("emacs" "find-file $1")
        ("d" "dired .")
        ("less" "eshell/less $1")
        ("rcd" "eshell/rcd $1")
        ("javac" "javac -J-Dfile.encoding=UTF-8 $1")
        ("java" "java -Dfile.encoding=UTF-8 $1")))
(provide 'eshell-conf)
;;; eshell-conf.el ends here
