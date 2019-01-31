;;; Code appearance

(column-number-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-font-lock-mode 0)
(add-hook 'term-mode-hook #'font-lock-mode)
(add-hook 'tex-mode-hook #'font-lock-mode)
(add-hook 'rst-mode-hook #'font-lock-mode)
(add-hook 'ag-mode-hook #'font-lock-mode)
(add-hook 'cider-repl-mode-hook #'font-lock-mode)
(add-hook 'fountain-mode-hook #'font-lock-mode)


;;; Frame Settings

(setq inhibit-startup-screen t)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(fringe-mode 0)

(when window-system
  (load-theme 'deeper-blue))

(setq frame-title-format "%b – Emacs")
(setq frame-resize-pixelwise t)

(cl-case system-type
  ('gnu/linux  (set-frame-font "Hack-10" nil t))
  ('windows-nt (set-frame-font "Consolas-10" nil t)))

(when (eq system-type 'windows-nt)
    (setq inhibit-compacting-font-caches t))

(setq eol-mnemonic-dos "\\")
(setq eol-mnemonic-unix ":")
(setq eol-mnemonic-mac "/")
(setq eol-mnemonic-undecided ":")
