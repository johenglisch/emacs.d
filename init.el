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


;;; Elisp files ------------------------------------------------------

(load-library "random-functions")

(load-library "editing-config")
(load-library "fuzzymatch-config")
(load-library "orgmode-config")
(load-library "shell-config")
(load-library "filetype-config")
(load-library "paredit-config")
(load-library "linter-config")
 
(load-library "key-bindings")

(load "local" t)
