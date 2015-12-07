;;; Org-mode Settings

(require 'org)

(setq org-list-allow-alphabetical t)
(add-hook 'org-mode-hook #'font-lock-mode)
