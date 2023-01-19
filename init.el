;;; -*- lexical-binding: t -*-

;;; Custom Vars ------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0f2f1feff73a80556c8c228396d76c1a0342eb4eefd00f881b91e26a14c5b62a" default))
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
(setq mc/list-file                   (expand-file-name "mc-lists.el" init-tmp-dir))
(setq transient-levels-file          (expand-file-name "transient/levels.el" init-tmp-dir))
(setq transient-values-file          (expand-file-name "transient/values.el" init-tmp-dir))
(setq transient-history-file         (expand-file-name "transient/history.el" init-tmp-dir))
(setq kkc-init-file-name             (expand-file-name "kkcrc" init-tmp-dir))


(if (eq system-type 'windows-nt)
    (setq default-directory "~/"))


;;; Package Management -----------------------------------------------

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(setq package-selected-packages
      '(arc-dark-theme                  ; goes well with arc (obviously)
        ayu-theme                       ; reeeally vibrant shade of dark blue
        autumn-light-theme              ; not the worst shade of beige
        gotham-theme                    ; very dark night-ey
        green-phosphor-theme            ; really cool effect
        lavender-theme                  ; might be just a tad too purple
        lush-theme                      ; might replace 'wombat for me
        molokai-theme                   ; like the grey and occasional purple
        monokai-theme                   ; a bit brown but okay
        oceanic-theme                   ; nice dark teal
        plan9-theme                     ; nice but maybe a bit low on contrast
        professional-theme              ; more 
        flx-ido magit projectile smex
        command-log-mode
        flycheck multiple-cursors paredit yasnippet
        auctex auctex-latexmk cider elpher fountain-mode json-mode markdown-mode
        gnu-elpa-keyring-update))

(if (version< emacs-version "26.3")
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))


;;; Helper Functions -------------------------------------------------

(defun init-byte-compile-current ()
  (interactive)
  (byte-compile-file (buffer-file-name)))

(defun init-open-init.el ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))


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

(add-to-list 'default-frame-alist '(width . 83))
(add-to-list 'default-frame-alist '(height . 30))

(load-theme (if window-system
                (if (package-installed-p 'lush-theme)
                    'lush
                  'wombat)
              'wheatgrass))

(setq frame-title-format "%b – Emacs")
(setq frame-resize-pixelwise t)

(when (eq system-type 'gnu/linux)
  (set-frame-font "Monospace-12" nil t)
  (set-fontset-font "fontset-default" 'han "Source Han Sans Normal")
  (set-fontset-font "fontset-default" 'cjk-misc "Source Han Sans Normal")
  (set-fontset-font "fontset-default" 'kana "Source Han Sans Normal"))

(when (eq system-type 'windows-nt)
  (setq inhibit-compacting-font-caches t)
  (set-frame-font "Consolas-12" nil t))


(global-font-lock-mode 0)
(add-hook 'term-mode-hook #'font-lock-mode)
(add-hook 'tex-mode-hook #'font-lock-mode)
(add-hook 'rst-mode-hook #'font-lock-mode)
(add-hook 'ag-mode-hook #'font-lock-mode)
(add-hook 'cider-repl-mode-hook #'font-lock-mode)
(add-hook 'fountain-mode-hook #'font-lock-mode)
(add-hook 'org-mode-hook #'font-lock-mode)
(add-hook 'magit-mode-hook #'font-lock-mode)
(add-hook 'gud-mode-hook #'font-lock-mode)

;;; Editing ----------------------------------------------------------

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq-default c-basic-offset 4)
(setq-default c-default-style
              '((java-mode . "java")
                (akw-mode . "awk")
                (other . "bsd")))
(setq c-basic-offset 4)
(setq-default indent-tabs-mode nil)

(when (eq system-type 'windows-nt)
  (prefer-coding-system 'utf-8-unix))

(require 'saveplace)
(setq-default save-place t)

(setq completion-ignore-case t)


;;; Terminal Emulator ------------------------------------------------

(add-hook 'comint-output-filter-functions
          #'shell-strip-ctrl-m nil t)
(add-hook 'comint-output-filter-functions
          #'comint-watch-for-password-prompt nil t)

(when (eq system-type 'gnu/linux)
  (setq explicit-shell-file-name
        (or (executable-find "zsh")
            (executable-find "bash")
            "/bin/sh"))
  (setq shell-file-name explicit-shell-file-name))


;;; Minor Modes ------------------------------------------------------

;; Command-log-mode

(setq command-log-mode-key-binding-open-log (kbd "C-c d"))
(require 'command-log-mode nil t)
;; Reminder: to activate run:
;;  * M-x global-command-log-mode
;;  * M-x clm/command-log-buffer
;; TODO Make simple key-bindable command that
;;  1. starts global command log mode,
;;  2. opens a new window that contains nothing but the command log buffer


;; Smex

(when (package-installed-p 'smex)
  (smex-initialize))

;; Ido

(ido-mode)

;; Flx-ido

(when (package-installed-p 'flx-ido)
  (flx-ido-mode))

;; Flycheck

(when (and (package-installed-p 'flycheck)
           (package-installed-p 'flycheck-pos-tip))
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function
           #'flycheck-pos-tip-error-messages)))

;; Paredit

(when (package-installed-p 'paredit)
  (autoload 'enable-paredit-mode
    "paredit" "Turn on pseudo-structural editing in Lisp code" t)

  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook          #'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook       #'enable-paredit-mode))

;; Projectile

(when (package-installed-p 'projectile)
  (projectile-global-mode))

;; Yasnippet

(when (require 'yasnippet nil t)
  (yas-global-mode 1))


;;; Major Modes ------------------------------------------------------

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

  (setq bibtex-align-at-equal-sign nil)
  (setq bibtex-comma-after-last-field t)

  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'TeX-mode-hook #'font-lock-mode)
  (add-hook 'TeX-mode-hook #'turn-on-reftex)

  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)

  (when (require 'auctex-latexmk nil t)
    (auctex-latexmk-setup)))

;;; R

(when (require 'ess-r-mode nil t)
  (setq ess-indent-with-fancy-comments nil))


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


;;; Key Bindings -----------------------------------------------------

;; General

(global-set-key (kbd "C-c e") #'init-open-init.el)

(global-set-key (kbd "C-c f") #'find-file-at-point)

(global-set-key (kbd "C-c t") #'term)

(global-set-key (kbd "C-c w") #'make-frame)

(global-set-key (kbd "C-c c") #'fixup-whitespace)

;; Plugins

(global-set-key (kbd "C-c o l") #'org-store-link)
(global-set-key (kbd "C-c o c") #'org-capture)
(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o b") #'org-iswitchb)

;; for some reason, <menu> is called <apps> on windows...
(unless (package-installed-p 'smex)
  (global-set-key (kbd "<apps>") #'execute-extended-command))

(when (package-installed-p 'smex)
  (global-set-key (kbd "M-x") #'smex)
  (global-set-key (kbd "<menu>") #'smex)
  (global-set-key (kbd "<apps>") #'smex)
  (global-set-key (kbd "<execute>") #'smex))

(when (package-installed-p 'magit)
  (global-set-key (kbd "C-c g") #'magit-status))

(when (require 'multiple-cursors nil t)
  (global-set-key (kbd "C-c l") #'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c n") #'mc/mark-next-like-this)
  (global-set-key (kbd "C-c L") #'mc/skip-to-previous-like-this)
  (global-set-key (kbd "C-c N") #'mc/skip-to-next-like-this)
  (global-set-key (kbd "C-c m") #'mc/edit-lines)
  (global-set-key (kbd "C-c M") #'mc/mark-all-like-this))

(when (package-installed-p 'projectile)
  (global-set-key (kbd "C-c p") #'projectile-command-map))
