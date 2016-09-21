;;; Org-mode Settings

;; The version of org-mode shipped with emacs doesn't know how to cider..
(let ((org-mode-dir (expand-file-name "org-mode/lisp" user-emacs-directory)))
  (if (file-directory-p org-mode-dir)
      (add-to-list 'load-path org-mode-dir)))

(require 'org)
(require 'ob-clojure)

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
