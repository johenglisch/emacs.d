;;; Appearance settings

(setq inhibit-startup-screen t)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(fringe-mode 0)

(column-number-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-font-lock-mode 0)
(add-hook 'term-mode-hook #'font-lock-mode)
(add-hook 'powershell-mode-hook #'font-lock-mode)
(add-hook 'tex-mode-hook #'font-lock-mode)
(add-hook 'rst-mode-hook #'font-lock-mode)

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

(when window-system
  (load-theme 'deeper-blue))

(cl-case system-type
  ('gnu/linux  (set-frame-font "Hack-10"))
  ('windows-nt (set-frame-font "Consolas-10")))

(when (require 'diminish nil t)
  (eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
  (eval-after-load "paredit" '(diminish 'paredit-mode)))
