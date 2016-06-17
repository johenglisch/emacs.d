;;; Editing Settings

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)

(when (eq system-type 'windows-nt)
  (prefer-coding-system 'utf-8-unix))

(require 'saveplace)
(setq-default save-place t)

(when (require 'which-key nil t)
  (which-key-mode))

(when (require 'company nil t)
  (setq company-idle-delay nil)
  (add-hook 'after-init-hook 'global-company-mode))
