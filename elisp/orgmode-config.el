;;; Org-mode Settings

(require 'org)

(defvar init-agenda-dir nil)
(setq init-agenda-dir
      (if (eq system-type 'windows-nt)
          "~/Documents/org-mode/"
        "~/.org-mode/"))

(setq org-todo-keywords
      '((sequence "TODO" "MIGHTDO" "WAITING" "|" "DONE")))

(when (file-directory-p init-agenda-dir)
  (setq org-agenda-files (list init-agenda-dir)))

(setq org-list-allow-alphabetical t)
(add-hook 'org-mode-hook #'font-lock-mode)
