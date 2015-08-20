;;; Smex

(when (package-installed-p 'smex)
  (smex-initialize))

;;; Ido

(ido-mode)

;;; Flx-ido

(when (package-installed-p 'flx-ido)
  (flx-ido-mode))
