;;; Code appearance

(column-number-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-font-lock-mode 0)
(add-hook 'term-mode-hook #'font-lock-mode)
(add-hook 'powershell-mode-hook #'font-lock-mode)
(add-hook 'tex-mode-hook #'font-lock-mode)
(add-hook 'rst-mode-hook #'font-lock-mode)
(add-hook 'ag-mode-hook #'font-lock-mode)

(when (require 'fill-column-indicator nil t)

  (setq fci-rule-color "#444444")

  (define-globalized-minor-mode global-fci-mode fci-mode
    (lambda ()
      (if buffer-file-name
          (fci-mode 1))))

  (global-fci-mode 1))


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


;;; Modeline

(when (require 'diminish nil t)
  (eval-after-load "autorevert" '(diminish 'auto-revert-mode))
  (eval-after-load "company"    '(diminish 'company-mode))
  (eval-after-load "eldoc"      '(diminish 'eldoc-mode))
  (eval-after-load "paredit"    '(diminish 'paredit-mode))
  (eval-after-load "undo-tree"  '(diminish 'undo-tree-mode))
  (eval-after-load "which-key"  '(diminish 'which-key-mode))
  (eval-after-load "projectile" '(diminish 'projectile-mode)))

(when (require 'powerline nil t)

  (if (require 'powerline-evil nil t)
      (powerline-evil-vim-color-theme)
    (powerline-default-theme))

  (setq powerline-text-scale-factor 0.65)

  (when (require 'airline-themes nil t)

    (setq airline-utf-glyph-separator-left     #xe0b0
          airline-utf-glyph-separator-right    #xe0b2
          airline-utf-glyph-subseparator-left  #xe0b1
          airline-utf-glyph-subseparator-right #xe0b3
          airline-utf-glyph-branch             #xe0a0
          airline-utf-glyph-readonly           #xe0a2
          airline-utf-glyph-linenumber         #xe0a1)

    (load-theme 'airline-hybridline t nil)))

(set-face-attribute 'mode-line                    nil :family "Aller")
(set-face-attribute 'mode-line-buffer-id          nil :family "Aller")
(set-face-attribute 'mode-line-buffer-id-inactive nil :family "Aller")
(set-face-attribute 'mode-line-emphasis           nil :family "Aller")
(set-face-attribute 'mode-line-highlight          nil :family "Aller")
(set-face-attribute 'mode-line-inactive           nil :family "Aller")
