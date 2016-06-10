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

  (define-key evil-normal-state-map (kbd "RET") nil)
  (define-key evil-normal-state-map " " nil)

  (vmap "C-g" 'evil-exit-visual-state)
  (imap "C-g" 'evil-normal-state)
  (rmap "C-g" 'evil-normal-state)
  (omap "C-g" 'evil-force-normal-state)

  (mmap "j" 'evil-next-visual-line)
  (mmap "k" 'evil-previous-visual-line)

  (mmap "H" 'evil-beginning-of-line)
  (nmap "L" 'evil-end-of-line)

  (imap "C-e" 'evil-end-of-line)

  (nmap "Q" 'evil-fill-and-move)
  (vmap "Q" 'evil-fill-and-move)

  (nmap "ö" (kbd "C-w"))

  (nmap "," 'evil-ex)
  (vmap "," 'evil-ex)

  (nmap "ä" 'init-goto-next-blank-line)
  (nmap "ü" 'init-goto-previous-blank-line)
  (nmap "Ä" 'evil-scroll-down)
  (nmap "Ü" 'evil-scroll-up)

  (nmap "´" 'evil-execute-macro)
  (nmap "#" 'evil-goto-mark-line)
  (nmap "'" 'evil-search-work-backward)

  (nmap "-" 'init-move-line-up)
  (nmap "+" 'init-move-line-down)

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

  (if (package-installed-p 'relative-line-numbers)
      (nmap "SPC n" 'relative-line-numbers-mode))

  (when (package-installed-p 'magit)
    (nmap "SPC g" 'magit-status))

  ;; Paredit Bindings

  (when (package-installed-p 'paredit)
    (evil-define-key 'normal paredit-mode-map
      (kbd "> )") 'paredit-forward-slurp-sexp
      (kbd "< )") 'paredit-forward-barf-sexp
      (kbd "> (") 'paredit-backward-barf-sexp
      (kbd "< (") 'paredit-backward-slurp-sexp))

  ;; Clojure Bindings

  (when (package-installed-p 'cider)
    (evil-define-key 'normal clojure-mode-map
      (kbd "Ö K") 'cider-eval-buffer
      (kbd "Ö M") 'cider-test-run-ns-tests
      (kbd "RET") '(lambda ()
                     (interactive)
                     (cider-load-buffer)
                     (cider-test-run-ns-tests nil)))))
