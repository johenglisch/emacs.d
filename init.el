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
        diminish which-key
        paredit yasnippet
        auctex auctex-latexmk cider fountain-mode haskell-mode json-mode
        markdown-mode rust-mode
        flycheck flycheck-haskell flycheck-rust
        evil evil-matchit evil-paredit evil-surround))


;;; Elisp files ------------------------------------------------------

(load-library "random-functions")

(load-library "style-config")
(load-library "editing-config")
(load-library "fuzzymatch-config")
(load-library "orgmode-config")
(load-library "shell-config")
(load-library "filetype-config")
(load-library "paredit-config")
(load-library "linter-config")

(load-library "the-dark-side-of-the-force")

(load-library "key-bindings")

(load "local" t)
