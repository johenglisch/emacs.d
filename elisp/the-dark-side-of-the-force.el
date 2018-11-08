(when (require 'evil nil t)

  (evil-mode 1)
  (load-library "evil-minions")

  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'Info-mode 'emacs)
  (evil-set-initial-state 'cider-repl-mode 'emacs)

  ;;; Plugins

  (when (require 'evil-matchit nil t)
    (global-evil-matchit-mode 1))

  (when (package-installed-p 'evil-paredit)
    (add-hook 'paredit-mode-hook #'evil-paredit-mode))

  (when (require 'evil-surround nil t)
    (global-evil-surround-mode 1))

  ;;; Better dot operator

  (evil-declare-motion #'init-goto-next-blank-line)
  (evil-declare-motion #'init-goto-previous-blank-line)

  ;;; Keybindings

  (nm-map "RET" nil)
  (nm-map "SPC" nil)

  (v-map "C-g" #'evil-exit-visual-state)
  (i-map "C-g" #'evil-normal-state)
  (r-map "C-g" #'evil-normal-state)
  (o-map "C-g" #'evil-force-normal-state)

  (n-map "j" #'evil-next-visual-line)
  (v-map "j" #'evil-next-visual-line)
  (n-map "k" #'evil-previous-visual-line)
  (v-map "k" #'evil-previous-visual-line)

  (nm-map "H" #'evil-beginning-of-line)
  (nm-map "L" #'evil-end-of-line)

  (nm-map "ä" #'init-goto-next-blank-line)
  (nm-map "ü" #'init-goto-previous-blank-line)
  (nm-map "Ä" #'evil-scroll-down)
  (nm-map "Ü" #'evil-scroll-up)

  (nm-map "ö" #'evil-window-map)
  (define-key evil-window-map "t" #'other-frame)
  (define-key evil-window-map "\C-t" #'other-frame)

  (n-map "Q" #'evil-execute-macro)
  ;; TODO Make QQ repeat the last macro

  (nm-map "," #'evil-ex)
  (v-map "," #'evil-ex)

  (nm-map "#" #'evil-goto-mark)
  (nm-map "'" #'evil-search-word-backward)

  (n-map "-" #'init-move-line-up)
  (n-map "+" #'init-move-line-down)

  (n-map "z g" #'ispell-word)

  (n-map "DEL" #'flycheck-mode)

  (i-map "C-e" #'evil-end-of-line)

  ;; Leader bindings

  (n-map "SPC SPC" (lambda () (interactive) (switch-to-buffer nil)))

  (n-map "SPC e v" (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
  (n-map "SPC v e" (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
  (n-map "SPC e t" (lambda () (interactive) (find-file (expand-file-name "todo.org" init-agenda-dir))))

  (n-map "SPC f" #'ido-find-file)
  (n-map "SPC b" #'ido-switch-buffer)

  (n-map "SPC a" #'ag-project-regexp)

  (n-map "SPC H" #'font-lock-mode)
  (n-map "SPC w" #'whitespace-cleanup)

  (n-map "SPC k"  (if (package-installed-p 'smex)
                      #'smex
                    #'execute-extended-command))

  (when (package-installed-p 'magit)
    (n-map "SPC g" #'magit-status))

  (when (package-installed-p 'projectile)
    (n-map "SPC p" #'projectile-command-map))

  ;; Org-mode Bindings

  (evil-define-key 'normal org-mode-map
    (kbd "RET") #'org-export-dispatch)

  ;; Paredit Bindings

  (when (package-installed-p 'paredit)
    (evil-define-key 'normal paredit-mode-map
      (kbd "> )") #'paredit-forward-slurp-sexp
      (kbd "< )") #'paredit-forward-barf-sexp
      (kbd "> (") #'paredit-backward-barf-sexp
      (kbd "< (") #'paredit-backward-slurp-sexp))

  ;; Clojure Bindings

  (when (package-installed-p 'cider)
    (evil-define-key 'normal clojure-mode-map
      (kbd "RET") (lambda ()
                    (interactive)
                    (cider-load-buffer)
                    (cider-test-run-ns-tests nil)))))
