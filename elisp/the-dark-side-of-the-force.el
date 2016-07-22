(when (require 'evil nil t)

  (global-evil-leader-mode 1)
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

  (nmap "ö" 'evil-window-map)

  (define-key evil-window-map "t" 'other-frame)
  (define-key evil-window-map "\C-t" 'other-frame)

  (nmap "," 'evil-ex)
  (vmap "," 'evil-ex)

  (nmap "ä" 'init-goto-next-blank-line)
  (nmap "ü" 'init-goto-previous-blank-line)
  (nmap "Ä" 'evil-scroll-down)
  (nmap "Ü" 'evil-scroll-up)

  (nmap "´" 'evil-execute-macro)
  (nmap "#" 'evil-goto-mark-line)
  (nmap "'" 'evil-search-word-backward)

  (nmap "-" 'init-move-line-up)
  (nmap "+" 'init-move-line-down)

  (nmap "z g" 'ispell-word)

  (nmap "DEL" 'flycheck-mode)

  ;; "Leader" bindings (really just mappings starting with SPC)

  (setq evil-leader/leader "<SPC>")

  (evil-leader/set-key
    evil-leader/leader 'ace-jump-mode

    "ev" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
    "ec" (lambda () (interactive) (find-file "~/.when/calendar"))
    "ve" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))

    "f"  'ido-find-file
    "b"  'ido-switch-buffer

    "H"  'font-lock-mode
    "w"  'whitespace-cleanup

    "k"  (if (package-installed-p 'smex)
             'smex
           'execute-extended-command))

  (if (package-installed-p 'relative-line-numbers)
      (evil-leader/set-key "n" 'relative-line-numbers-mode))

  (when (package-installed-p 'magit)
    (evil-leader/set-key "g" 'magit-status))

  (when (package-installed-p 'projectile)
    (evil-leader/set-key "p" 'projectile-command-map))

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
      (kbd "RET") '(lambda ()
                     (interactive)
                     (cider-load-buffer)
                     (cider-test-run-ns-tests nil)))))
