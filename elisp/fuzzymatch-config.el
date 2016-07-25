;;; Smex

(when (package-installed-p 'smex)
  (smex-initialize))

;;; Ido

(ido-mode)

;;; Flx-ido

(when (package-installed-p 'flx-ido)
  (flx-ido-mode))

;;; Projectile

(when (package-installed-p 'projectile)
  (projectile-global-mode)

  ;; projectile is lacking `other-frame` bindings

  (defun projectile-find-other-file-other-frame (&optional flex-matching)
    (interactive "P")
    (-if-let (other-files (projectile-get-other-files (buffer-file-name) (projectile-current-project-files) flex-matching))
        (if (= (length other-files) 1)
            (find-file-other-frame (expand-file-name (car other-files) (projectile-project-root)))
          (find-file-other-frame (expand-file-name (projectile-completing-read "Switch to: " other-files) (projectile-project-root))))
      (error "No other file found")))

  (defun projectile-switch-to-buffer-other-frame ()
    (interactive)
    (switch-to-buffer-other-frame
     (projectile-read-buffer-to-switch "Switch to buffer: ")))

  (defun projectile-find-dir-other-frame (&optional arg)
    (interactive "P")
    (when arg
      (projectile-invalidate-cache nil))
    (let ((dir (projectile-complete-dir)))
      (dired-other-frame (expand-file-name dir (projectile-project-root)))
      (run-hooks 'projectile-find-dir-hook)))

  (defun projectile-find-file-other-frame (&optional arg)
    (interactive "P")
    (projectile-maybe-invalidate-cache arg)
    (let ((file (projectile-completing-read "Find file: "
                                            (projectile-current-project-files))))
      (find-file-other-frame (expand-file-name file (projectile-project-root)))
      (run-hooks 'projectile-find-file-hook)))

  (defun projectile-find-file-dwim-other-frame (&optional arg)
    (interactive "P")
    (let* ((project-files (projectile-current-project-files))
           (files (projectile-select-files project-files arg)))
      (cond
       ((= (length files) 1)
        (find-file-other-frame (expand-file-name (car files) (projectile-project-root))))
       ((> (length files) 1)
        (find-file-other-frame (expand-file-name (projectile-completing-read "Switch to: " files) (projectile-project-root))))
       (t (find-file-other-frame (expand-file-name (projectile-completing-read "Switch to: " project-files) (projectile-project-root)))))
      (run-hooks 'projectile-find-file-hook)))

  (defun projectile-find-implementation-or-test-other-frame ()
    (interactive)
    (find-file-other-frame
     (projectile-find-implementation-or-test (buffer-file-name))))

  (let ((map 'projectile-command-map))
    (define-key map (kbd "5 a") #'projectile-find-other-file-other-frame)
    (define-key map (kbd "5 b") #'projectile-switch-to-buffer-other-frame)
    (define-key map (kbd "5 d") #'projectile-find-dir-other-frame)
    (define-key map (kbd "5 f") #'projectile-find-file-other-frame)
    (define-key map (kbd "5 g") #'projectile-find-file-dwim-other-frame)
    (define-key map (kbd "5 t") #'projectile-find-implementation-or-test-other-frame)))
