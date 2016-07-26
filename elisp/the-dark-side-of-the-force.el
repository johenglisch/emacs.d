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

  (v-map "C-g" 'evil-exit-visual-state)
  (i-map "C-g" 'evil-normal-state)
  (r-map "C-g" 'evil-normal-state)
  (o-map "C-g" 'evil-force-normal-state)

  (m-map "j" 'evil-next-visual-line)
  (m-map "k" 'evil-previous-visual-line)

  (m-map "H" 'evil-beginning-of-line)
  (n-map "L" 'evil-end-of-line)

  (i-map "C-e" 'evil-end-of-line)

  (n-map "Q" 'evil-fill-and-move)
  (v-map "Q" 'evil-fill-and-move)

  (n-map "ö" 'evil-window-map)

  (define-key evil-window-map "t" 'other-frame)
  (define-key evil-window-map "\C-t" 'other-frame)

  (n-map "," 'evil-ex)
  (v-map "," 'evil-ex)

  (n-map "ä" 'init-goto-next-blank-line)
  (n-map "ü" 'init-goto-previous-blank-line)
  (n-map "Ä" 'evil-scroll-down)
  (n-map "Ü" 'evil-scroll-up)

  (n-map "´" 'evil-execute-macro)
  (n-map "#" 'evil-goto-mark-line)
  (n-map "'" 'evil-search-word-backward)

  (n-map "-" 'init-move-line-up)
  (n-map "+" 'init-move-line-down)

  (n-map "z g" 'ispell-word)

  (n-map "DEL" 'flycheck-mode)

  ;; Leader bindings

  (setq evil-leader/leader "<SPC>")

  (evil-leader/set-key
    evil-leader/leader 'ace-jump-mode

    "ev" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
    "ec" (lambda () (interactive) (find-file "~/.when/calendar"))
    "ve" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))

    "f"  'ido-find-file
    "b"  'ido-switch-buffer

    "a"  'ag-project-regexp

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
