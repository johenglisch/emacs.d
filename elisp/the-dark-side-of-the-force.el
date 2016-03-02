(when (require 'evil nil t)

  (evil-mode 1)
  (load-library "evil-minions")

  (evil-set-initial-state 'term-mode 'emacs)

  ;;; Plugins

  (when (require 'evil-matchit nil t)
    (global-evil-matchit-mode 1))

  (when (package-installed-p 'evil-paredit)
    (add-hook 'paredit-mode-hook 'evil-paredit-mode))

  ;;; Keybindings

  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map " " nil)

  (vmap "C-g" 'evil-exit-visual-state)
  (imap "C-g" 'evil-normal-state)
  (rmap "C-g" 'evil-normal-state)
  (omap "C-g" 'evil-force-normal-state)

  (mmap "j" 'evil-next-visual-line)
  (mmap "k" 'evil-previous-visual-line)

  (mmap "H" 'evil-beginning-of-line)
  (nmap "L" 'evil-end-of-line)

  (nmap "Q" 'evil-fill-and-move)
  (vmap "Q" 'evil-fill-and-move)

  (nmap "ö" (kbd "C-w"))

  (nmap "," 'evil-ex)
  (vmap "," 'evil-ex)

  (nmap "ä" 'evil-forward-paragraph)
  (nmap "ü" 'evil-backward-paragraph)
  (nmap "Ä" 'evil-scroll-down)
  (nmap "Ü" 'evil-scroll-up)

  (nmap "´" 'evil-execute-macro)
  (nmap "#" 'evil-goto-mark-line)
  (nmap "'" 'evil-search-work-backward)

  (nmap "z g" 'ispell-word)

  (nmap "DEL" 'flycheck-mode)

  ;; "Leader" bindings (really just mappings starting with SPC)

  (nmap "SPC SPC" 'ace-jump-mode)

  (nmap "SPC e v" (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
  (nmap "SPC e c" (lambda () (interactive) (find-file "~/.when/calendar")))

  (nmap "SPC v e" (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

  (nmap "SPC f" 'ido-find-file)
  (nmap "SPC b" 'ido-switch-buffer)

  (nmap "SPC H" 'font-lock-mode)

  (nmap "SPC w" 'whitespace-cleanup)

  (if (package-installed-p 'smex)
      (nmap "SPC k" 'smex)
    (nmap "SPC k" 'execute-extended-command))

  (when (package-installed-p 'magit)
    (nmap "SPC g c" 'magit-commit)
    (nmap "SPC g d" 'magit-diff)
    (nmap "SPC g s" 'magit-status)))
