;;; General

(global-set-key (kbd "C-c e")
                (lambda () (interactive) (find-file "~/.emacs.d/init.el")))


;;; Plugins

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; for some reason, <menu> is called <apps> on windows...
(unless (package-installed-p 'smex)
  (global-set-key (kbd "<apps>") 'execute-extended-command))

(when (package-installed-p 'ace-jump-mode)
  (global-set-key (kbd "C-c SPC") 'ace-jump-mode))

(when (package-installed-p 'smex)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "<menu>") 'smex)
  (global-set-key (kbd "<apps>") 'smex)
  (global-set-key (kbd "<execute>") 'smex))

(when (package-installed-p 'magit)
  (global-set-key (kbd "C-c g") 'magit-status))


;;; Platform-Dependent

(when (eq system-type 'windows-nt)
  (global-set-key (kbd "C-c t") 'powershell))

(when (eq system-type 'gnu/linux)
  (global-set-key (kbd "C-c t") 'term))
