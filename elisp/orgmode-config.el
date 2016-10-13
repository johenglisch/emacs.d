;;; Org-mode Settings

;; The version of org-mode shipped with emacs doesn't know how to cider..
;; Git repo: git://orgmode.org/org-mode.git
(let ((org-mode-dir (expand-file-name "org-mode/lisp" user-emacs-directory))
      (contrib-dir  (expand-file-name "org-mode/contrib/lisp" user-emacs-directory)))
  (when (file-directory-p org-mode-dir)
    (add-to-list 'load-path org-mode-dir)
    (add-to-list 'load-path contrib-dir)))

(require 'org)
(require 'ob-clojure)

(defvar init-agenda-dir nil)
(setq init-agenda-dir
      (if (eq system-type 'windows-nt)
          "~/Documents/org-mode/"
        "~/.org-mode/"))

(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "|" "DONE")))

(when (file-directory-p init-agenda-dir)
  (setq org-agenda-files (list init-agenda-dir)))

(setq org-list-allow-alphabetical t)
(add-hook 'org-mode-hook #'font-lock-mode)

(setq org-src-fontify-natively nil)
