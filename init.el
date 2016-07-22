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
 '(info-menu-header ((t (:family "Aller"))))
 '(info-title-1 ((t (:family "Aller"))))
 '(info-title-2 ((t (:family "Aller"))))
 '(info-title-3 ((t (:family "Aller"))))
 '(info-title-4 ((t (:family "Aller")))))


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
(setq package-user-dir               (format "%s%s" init-tmp-dir "elpa/"))
(setq ido-save-directory-list-file   (format "%s%s" init-tmp-dir "ido-last"))
(setq save-place-file                (format "%s%s" init-tmp-dir "places"))
(setq smex-save-file                 (format "%s%s" init-tmp-dir "smex-items"))

(add-to-list 'load-path "~/.emacs.d/elisp/")

(if (eq system-type 'windows-nt)
    (setq default-directory "~/"))


;;; Package Management -----------------------------------------------

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(defvar init-package-list nil)
(setq init-package-list
      '(color-theme-solarized diminish
        evil evil-matchit evil-leader evil-paredit powerline-evil airline-themes
        relative-line-numbers
        flx-ido smex ace-jump-mode company which-key projectile
        magit paredit cider markdown-mode haskell-mode
        auctex auctex-latexmk
        flycheck flycheck-haskell flycheck-clojure))

(when (eq system-type 'windows-nt)
  (add-to-list 'init-package-list 'powershell))


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
