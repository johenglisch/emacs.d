;;; Code appearance

(column-number-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-font-lock-mode 0)
(add-hook 'term-mode-hook #'font-lock-mode)
(add-hook 'powershell-mode-hook #'font-lock-mode)
(add-hook 'tex-mode-hook #'font-lock-mode)
(add-hook 'rst-mode-hook #'font-lock-mode)
(add-hook 'ag-mode-hook #'font-lock-mode)
(add-hook 'cider-repl-mode-hook #'font-lock-mode)

(global-hl-line-mode 1)


;;; Colour theme

(when (package-installed-p 'color-theme-solarized)
  (load-theme 'solarized t t)

  (defun init-solarized-dark ()
    (interactive)
    (set-frame-parameter nil 'background-mode 'dark)
    (enable-theme 'solarized))

  (defun init-solarized-light ()
    (interactive)
    (set-frame-parameter nil 'background-mode 'light)
    (enable-theme 'solarized)))


;;; Frame Settings

(setq inhibit-startup-screen t)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(fringe-mode 0)

(when window-system
  (load-theme 'deeper-blue))

(setq frame-title-format "%b – Emacs")

(cl-case system-type
  ('gnu/linux  (set-frame-font "Hack-10" nil t))
  ('windows-nt (set-frame-font "Consolas-10" nil t)))

(when (eq system-type 'windows-nt)
    (setq inhibit-compacting-font-caches t))

(setq eol-mnemonic-dos "\\")
(setq eol-mnemonic-unix ":")
(setq eol-mnemonic-mac "/")
(setq eol-mnemonic-undecided ":")


;;; Modeline

(when (require 'diminish nil t)
  (eval-after-load "autorevert" '(diminish 'auto-revert-mode))
  (eval-after-load "company"    '(diminish 'company-mode))
  (eval-after-load "eldoc"      '(diminish 'eldoc-mode))
  (eval-after-load "paredit"    '(diminish 'paredit-mode))
  (eval-after-load "undo-tree"  '(diminish 'undo-tree-mode))
  (eval-after-load "which-key"  '(diminish 'which-key-mode))
  (eval-after-load "projectile" '(diminish 'projectile-mode))
  (eval-after-load "yasnippet"  '(diminish 'yas-minor-mode)))
