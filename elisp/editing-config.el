;;; Editing Settings

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)

(when (eq system-type 'windows-nt)
  (prefer-coding-system 'utf-8-unix))
