;;; Org-mode Settings

;; The version of org-mode shipped with emacs doesn't know how to cider..
;; Git repo: git://orgmode.org/org-mode.git
(let ((org-mode-dir (expand-file-name "org-mode-src/lisp" user-emacs-directory)))
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
      '((sequence "TODO" "WAITING" "|" "DONE")))

(setq org-log-done 'time)

(when (file-directory-p init-agenda-dir)
  (setq org-agenda-files (list init-agenda-dir)))

(setq org-list-allow-alphabetical t)

(add-hook 'org-mode-hook #'font-lock-mode)
(setq org-src-fontify-natively nil)


;; Latex export

(require 'ox-latex)

(if (eq system-type 'gnu/linux)
    (add-to-list 'org-file-apps '("\\.pdf" . "evince %s")))

(add-to-list 'org-latex-classes
             '("scrartcl"
               "\\documentclass[a4paper]{scrartcl}\n[DEFAULT-PACKAGES]\n[PACKAGES]\n\\widowpenalty=10000\n\\clubpenalty=10000\n[EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-packages-alist
             '("" "booktabs" t))

(setq org-latex-tables-booktabs t)
(setq org-latex-caption-above nil)
(setq org-latex-image-default-width nil)

(setq org-entities-user '(("space" "\\ " nil " " " " " " " ")
                          ("sentend" "\\@" nil "" "" "" "")))
