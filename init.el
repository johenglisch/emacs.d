(require 'cl)

;;; Custom Vars ------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column 79))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;; Folders ----------------------------------------------------------

(defvar init-tmp-dir nil)
(setq init-tmp-dir
      (if (eq system-type 'windows-nt)
          "~/_cache/emacs/"
        "~/.cache/emacs/"))

(unless (file-exists-p init-tmp-dir)
  (make-directory init-tmp-dir t))

(setq backup-directory-alist         `((".*" . ,init-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,init-tmp-dir t)))
(setq auto-save-list-file-prefix     init-tmp-dir)
(setq package-user-dir               (expand-file-name "elpa/" init-tmp-dir))
(setq ido-save-directory-list-file   (expand-file-name "ido-last" init-tmp-dir))
(setq save-place-file                (expand-file-name "places" init-tmp-dir))
(setq eshell-directory-name          (expand-file-name "eshell/" init-tmp-dir))
(setq smex-save-file                 (expand-file-name "smex-items" init-tmp-dir))
(setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" init-tmp-dir))
(setq projectile-cache-file          (expand-file-name "projectile.cache" init-tmp-dir))

(add-to-list 'load-path (expand-file-name "elisp/" user-emacs-directory))

(if (eq system-type 'windows-nt)
    (setq default-directory "~/"))


;;; Package Management -----------------------------------------------

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(setq package-selected-packages
      '(flx-ido magit projectile smex
        flycheck paredit yasnippet
        auctex auctex-latexmk cider fountain-mode json-mode
        markdown-mode))


;;; Appearance -------------------------------------------------------

(defalias 'yes-or-no-p 'y-or-n-p)

(setq eol-mnemonic-dos "\\")
(setq eol-mnemonic-unix ":")
(setq eol-mnemonic-mac "/")
(setq eol-mnemonic-undecided ":")

(setq inhibit-startup-screen t)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(fringe-mode 0)

(column-number-mode t)

(when window-system
  (load-theme 'deeper-blue))

(setq frame-title-format "%b – Emacs")
(setq frame-resize-pixelwise t)

(cl-case system-type
  ('gnu/linux  (set-frame-font "Hack-10" nil t))
  ('windows-nt (set-frame-font "Consolas-10" nil t)))

(when (eq system-type 'windows-nt)
    (setq inhibit-compacting-font-caches t))

(global-font-lock-mode 0)
(add-hook 'term-mode-hook #'font-lock-mode)
(add-hook 'tex-mode-hook #'font-lock-mode)
(add-hook 'rst-mode-hook #'font-lock-mode)
(add-hook 'ag-mode-hook #'font-lock-mode)
(add-hook 'cider-repl-mode-hook #'font-lock-mode)
(add-hook 'fountain-mode-hook #'font-lock-mode)
(add-hook 'org-mode-hook #'font-lock-mode)


;;; Editing ----------------------------------------------------------

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)

(when (eq system-type 'windows-nt)
  (prefer-coding-system 'utf-8-unix))

(require 'saveplace)
(setq-default save-place t)


;;; Terminal emulator ------------------------------------------------

(add-hook 'comint-output-filter-functions
          #'shell-strip-ctrl-m nil t)
(add-hook 'comint-output-filter-functions
          #'comint-watch-for-password-prompt nil t)

(when (eq system-type 'gnu/linux)
  (setq explicit-shell-file-name "/bin/zsh")
  (setq shell-file-name explicit-shell-file-name))


;;; Plugins ----------------------------------------------------------

;; Smex

(when (package-installed-p 'smex)
  (smex-initialize))

;; Ido

(ido-mode)

;; Flx-ido

(when (package-installed-p 'flx-ido)
  (flx-ido-mode))

;; Projectile

(when (package-installed-p 'projectile)
  (projectile-global-mode))

;; Yasnippet

(when (require 'yasnippet nil t)
  (yas-global-mode 1))


;;; Filetypes --------------------------------------------------------

;; Elisp

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

;; Clojure

(when (package-installed-p 'cider)
  (setq org-babel-clojure-backend 'cider)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (setq nrepl-hide-special-buffers t)
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-repl-display-help-banner nil))

;; Markdown-mode

(when (package-installed-p 'markdown-mode)
  (if (eq system-type 'gnu/linux)
      (setq markdown-command "/usr/bin/env markdown"))

  (add-to-list 'auto-mode-alist '("\\.txt" . markdown-mode))
  (autoload 'markdown-mode "markdown-mode" "Major mode for Markdown files" t)
  (add-hook 'markdown-mode-hook #'font-lock-mode))

;; Haskell-mode

(when (package-installed-p 'haskell-mode)
  (add-hook 'haskell-mode-hook #'turn-on-haskell-indentation))

;; LaTeX/AucTeX

(setq TeX-auto-save nil)
(setq TeX-parse-self t)

(setq TeX-view-format "pdf")

(setq font-latex-fontify-script nil)
(setq font-latex-fontify-sectioning 1.0)

(when (package-installed-p 'auctex)
  (if (eq system-type 'windows-nt)
      (require 'tex-mik))

  (add-hook 'TeX-mode-hook #'font-lock-mode)

  (when (require 'auctex-latexmk nil t)
    (auctex-latexmk-setup)))


;;; Org-mode Settings ------------------------------------------------

(require 'org)

(setq org-list-allow-alphabetical t)
(setq org-src-fontify-natively nil)
(setq org-adapt-indentation nil)

;; Agenda

(defvar init-agenda-dir nil)
(setq init-agenda-dir "~/org/")

(setq org-log-done 'time)

(when (file-directory-p init-agenda-dir)
  (setq org-agenda-files
        `(,(expand-file-name "todo.org" init-agenda-dir)
          ,(expand-file-name "termine.org" init-agenda-dir)))

  (setq org-archive-location
        (concat (file-name-as-directory init-agenda-dir)
                "archiv.org::datetree/")))

;; Babel

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (clojure . t)))

;; Latex export

(require 'ox-latex)
(require 'ox-beamer)

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


;;; Elisp files ------------------------------------------------------

(load-library "random-functions")

(load-library "paredit-config")
(load-library "linter-config")
 
(load-library "key-bindings")

(load "local" t)
