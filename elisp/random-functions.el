(defun init-install-packages ()
  (interactive)
  (package-refresh-contents)
  (dolist (pkg init-package-list)
    (unless (package-installed-p pkg)
      (package-install pkg))))


(defun init-byte-compile-current ()
  (interactive)
  (byte-compile-file (buffer-file-name)))


(defun init-english ()
  (interactive)
  (ispell-change-dictionary "british"))

(defun init-deutsch ()
  (interactive)
  (ispell-change-dictionary "deutsch8"))
