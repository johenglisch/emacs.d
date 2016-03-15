(defun init-install-packages ()
  (interactive)
  (package-refresh-contents)
  (dolist (pkg init-package-list)
    (unless (package-installed-p pkg)
      (package-install pkg))))


(defun init-byte-compile-current ()
  (interactive)
  (byte-compile-file (buffer-file-name)))


(defun init-english-spelling ()
  (interactive)
  (ispell-change-dictionary "british")
  (flyspell-mode 1))

(defun init-german-spelling ()
  (interactive)
  (ispell-change-dictionary "german-old8")
  (flyspell-mode 1))
