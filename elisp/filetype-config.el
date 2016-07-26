;;; Elisp

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)


;;; Clojure

(when (package-installed-p 'cider)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (setq nrepl-hide-special-buffers t)
  (setq cider-repl-pop-to-buffer-on-connect nil))


;;; Markdown-mode

(when (package-installed-p 'markdown-mode)
  (if (eq system-type 'gnu/linux)
      (setq markdown-command "/usr/bin/env markdown"))

  (add-to-list 'auto-mode-alist '("\\.txt" . markdown-mode))
  (autoload 'markdown-mode "markdown-mode" "Major mode for Markdown files" t)
  (add-hook 'markdown-mode-hook #'font-lock-mode))


;;; Haskell-mode

(when (package-installed-p 'haskell-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))


;;; LaTeX/AucTeX

(setq TeX-auto-save nil)
(setq TeX-parse-self t)

(setq TeX-view-format "pdf")

(when (package-installed-p 'auctex)
  (if (eq system-type 'windows-nt)
      (require 'tex-mik))

  (add-hook 'TeX-mode-hook #'font-lock-mode)

  (when (require 'auctex-latexmk nil t)
    (auctex-latexmk-setup)))
