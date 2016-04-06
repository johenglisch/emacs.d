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


(defun init-move-line-down (lines)
  (interactive "p")
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines lines))
    (forward-line lines)
    (move-to-column col)))

(defun init-move-line-up (lines)
  (interactive "p")
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines (- lines)))
    (forward-line -1)
    (move-to-column col)))


(defun init-goto-next-blank-line ()
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-forward " >")
    (unless (search-forward-regexp "^\\s *$" nil t)
      (goto-char (point-max)))))

(defun init-goto-previous-blank-line ()
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-backward " >")
    (unless (search-backward-regexp "^\\s *$" nil t)
      (goto-char (point-min)))))
