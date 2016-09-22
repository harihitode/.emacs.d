;; Less command for eshell
(defun eshell/less (&rest args)
  "Invoke `view-file' on the file.
\"less +42 foo\" also goes to line 42 in the buffer."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (view-file file)
          (goto-line line))
      (view-file (pop args)))))

;; Remote cd command for eshell
(defun eshell/rcd (&optional hostname dir)
  (let ((dir (if dir dir "~/")))
    (cond (hostname (eshell/cd (format "/%s:%s" hostname dir)))
          (t (eshell/cd dir)))))

;; PATH setting
(require 'em-alias)
(add-to-list 'exec-path "/usr/local/bin")
(mapc (lambda (alias) (add-to-list 'eshell-command-aliases-list alias))
      `(("ll" "ls -ltr")
        ("la" "ls -a")
        ("emacs" "find-file $1")
        ("d" "dired .")
        ("l" "eshell/less $1")
        ("rcd" "eshell/rcd $1")
        ("javac" "javac -J-Dfile.encoding=UTF-8 $1")
        ("java" "java -Dfile.encoding=UTF-8 $1")))
