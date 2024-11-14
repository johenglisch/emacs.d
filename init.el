;;; -*- lexical-binding: t -*-

;;; Folders ----------------------------------------------------------

(defvar init-cache-dir
  (if (eq system-type 'windows-nt)
      "~/_cache"
    (or (getenv "XDG_CACHE_HOME")
        "~/.cache")))

(when (eq system-type 'windows-nt)
  (setq default-directory "~/"))

(setq custom-file (expand-file-name "custom.el" init-config-folder))

(add-to-list 'load-path (expand-file-name "lisp" init-config-folder))


;;; Package Management -----------------------------------------------

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(setq package-selected-packages
      '(ayu-theme autumn-light-theme green-phosphor-theme lavender-theme
        lush-theme professional-theme
        magit flx-ido smex command-log-mode
        ;; Am I ever using these?
        ;; exec-path-from-shell
        flycheck multiple-cursors paredit yasnippet
        auctex auctex-latexmk bqn-mode cider csv-mode elpher ess fountain-mode
        json-mode markdown-mode nov geiser geiser-chez geiser-guile
        geiser-gambit
        gnu-elpa-keyring-update))


;;; Helper Functions -------------------------------------------------

(defun init-byte-compile-current ()
  "Compile current elisp file."
  (interactive)
  (byte-compile-file (buffer-file-name)))

(defun init-open-init.el ()
  "Open init.el file in the current window."
  (interactive)
  (find-file (expand-file-name "init.el" init-config-folder)))

(defun init-pop-out-window ()
   "Pop out the current window into a new frame."
   (interactive)
   (let ((windows (window-list nil 'no-minibufs)))
     (cond ((null windows)
            (message "Frame has no windows."))
           ((null (cdr windows))
            (message "Frame has only one window."))
           (t
            (let ((current-window (selected-window)))
              (if (window-minibuffer-p current-window)
                  (message "Won't pop out minibuffer")
                (and (make-frame)
                     (delete-window current-window))))))))

(defvar init-repos-folder (expand-file-name "repos" "~"))

(defun init-goto-repo-folder ()
  (interactive)
  ;; TODO: move into a better location
  ;; TODO: prompt for url (default to ‘thing-at-point’)
  (let ((url (thing-at-point 'url)))
    (if (null url)
        (message "No url found at point.")
      (let* ((suffix (replace-regexp-in-string
                      ".*?\\([^:/]+/[^/]+\\)$" "\\1" url))
             (folder (expand-file-name suffix init-repos-folder)))
        (if (file-directory-p folder)
            (dired folder)
          (and (yes-or-no-p
                (format "%s does not exist. Clone repo?" folder))
               (= 0 (shell-command (format "git clone '%s' '%s'" url folder)))
               (dired folder)))))))

(defun init-wrap-region (region-start region-end left right)
  (let ((left-len  (if (characterp left)  1 (length left)))
        (right-len (if (characterp right) 1 (length right)))
        (start (if (region-active-p) region-start (point)))
        (end   (if (region-active-p) region-end   (point))))
    (goto-char end)
    (insert right)
    (goto-char start)
    (insert left)
    (if (= start end)
        (goto-char (+ start left-len))
      (goto-char (+ end left-len right-len)))))

(defun init-single-quote-region (region-start region-end)
  (interactive "r")
  (init-wrap-region region-start region-end ?‘ ?’))

(defun init-double-quote-region (region-start region-end)
  (interactive "r")
  (init-wrap-region region-start region-end ?“ ?”))

(defun init-kraut-quote-region (region-start region-end)
  (interactive "r")
  (init-wrap-region region-start region-end ?» ?«))

(defun init-kraut-single-quote-region (region-start region-end)
  (interactive "r")
  (init-wrap-region region-start region-end ?› ?‹))

(defun init-kraut-quote-region-2 (region-start region-end)
  (interactive "r")
  (init-wrap-region region-start region-end ?„ ?“))

(defun init-kraut-single-quote-region-2 (region-start region-end)
  (interactive "r")
  (init-wrap-region region-start region-end ?‚ ?‘))

;; ;; TODO: function for setting this key bind to an interactive funtion
;; (global-set-key (kbd "C-c d") #'init-goto-repo-folder)


;;; Appearance -------------------------------------------------------

(defalias 'yes-or-no-p #'y-or-n-p)

(setq eol-mnemonic-dos "\\")
(setq eol-mnemonic-unix ":")
(setq eol-mnemonic-mac "/")
(setq eol-mnemonic-undecided ":")

(setq inhibit-startup-screen t)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(fringe-mode 0)

(column-number-mode t)

(add-to-list 'default-frame-alist '(width . 83))
(add-to-list 'default-frame-alist '(height . 35))

;; (load-theme (if window-system
;;                 'wheatgrass
;;               'wheatgrass))

(setq frame-title-format "%b – Emacs")
(setq frame-resize-pixelwise t)

(when (eq system-type 'gnu/linux)
  (add-to-list 'initial-frame-alist '(font . "Monospace-12"))
  (add-to-list 'default-frame-alist '(font . "Monospace-12")))

(when (eq system-type 'windows-nt)
  (setq inhibit-compacting-font-caches t)
  (add-to-list 'initial-frame-alist '(font . "Consolas-12"))
  (add-to-list 'default-frame-alist '(font . "Consolas-12")))

(defun init-set-font-fallback (frame)
  (when (display-graphic-p frame)
    (set-fontset-font t 'han "Source Han Sans Normal" frame 'prepend)
    (set-fontset-font t 'cjk-misc "Source Han Sans Normal" frame 'preprend)
    (set-fontset-font t 'kana "Source Han Sans Normal" frame 'prepend)
    (set-fontset-font t 'shavian "Fairfax HD" frame 'prepend)))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (init-set-font-fallback frame)))
  (init-set-font-fallback nil))

(setq font-lock-maximum-decoration 0)
(global-font-lock-mode 0)
(dolist (mode-hook '(ag-mode-hook
                     cider-repl-mode-hook
                     dired-mode-hook
                     fountain-mode-hook
                     gud-mode-hook
                     magit-mode-hook
                     markdown-mode-hook
                     org-mode-hook
                     racket-repl-mode-hook
                     rst-mode-hook
                     term-mode-hook
                     TeX-mode-hook
                     tex-mode-hook))
  (add-hook mode-hook #'font-lock-mode))


;;; Editing ----------------------------------------------------------

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq-default c-basic-offset 4)
(setq-default c-default-style
              '((java-mode . "java")
                (awk-mode . "awk")
                (other . "bsd")))
(setq c-basic-offset 4)
(setq-default indent-tabs-mode nil)

(when (eq system-type 'windows-nt)
  (prefer-coding-system 'utf-8-unix))

(require 'saveplace)
(setq-default save-place t)

(setq completion-ignore-case t)


;;; Prepare PATH -----------------------------------------------------

(when (and (not (eq system-type 'windows-nt))
           (require 'exec-path-from-shell nil t))
  (exec-path-from-shell-initialize))


;;; Terminal Emulator ------------------------------------------------

(add-hook 'comint-output-filter-functions
          #'shell-strip-ctrl-m nil t)
(add-hook 'comint-output-filter-functions
          #'comint-watch-for-password-prompt nil t)

(when (eq system-type 'gnu/linux)
  (setq explicit-shell-file-name
        (or (executable-find "zsh")
            (executable-find "bash")
            "/bin/sh"))
  (setq shell-file-name explicit-shell-file-name))


;;; Minor Modes ------------------------------------------------------

;; LSP-mode

(when (package-installed-p 'lsp-mode)
  (setq lsp-keymap-prefix "C-c v")
  (require 'lsp-mode nil t))

;; Command-log-mode

(setq command-log-mode-key-binding-open-log (kbd "C-c d"))
(require 'command-log-mode nil t)
;; Reminder: to activate run:
;;  * M-x global-command-log-mode
;;  * M-x clm/command-log-buffer
;; TODO Make simple key-bindable command that
;;  1. starts global command log mode,
;;  2. opens a new window that contains nothing but the command log buffer

;; Smex

(when (package-installed-p 'smex)
  (smex-initialize))

;; Ido

(ido-mode)

;; Flx-ido

(when (package-installed-p 'flx-ido)
  (flx-ido-mode))

;; Flyspell

(when (executable-find "hunspell")
  ;; nicked from https://200ok.ch/posts/2020-08-22_setting_up_spell_checking_with_multiple_dictionaries.html
  (setq ispell-personal-dictionary (expand-file-name "hunspell-personal-dict" init-cache-dir))
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0))

  (with-eval-after-load "ispell"
    (setq ispell-program-name "hunspell")
    (setq ispell-dictionary "en_GB,de_DE")
    (ispell-set-spellchecker-params)
    (ispell-hunspell-add-multi-dic "en_GB,de_DE"))

  (add-hook 'text-mode-hook #'flyspell-mode))

;; Flycheck

(when (and (package-installed-p 'flycheck)
           (package-installed-p 'flycheck-pos-tip))
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function
           #'flycheck-pos-tip-error-messages)))

;; God-mode

(when (require 'god-mode nil t)
  (defun init-god-mode-update-cursor-type ()
    (let ((cursor (if (or god-local-mode
                          buffer-read-only)
                      'box
                    'bar)))
      (setq cursor-type cursor)))
  (add-hook 'post-command-hook #'init-god-mode-update-cursor-type))

;; Paredit

(when (package-installed-p 'paredit)
  (autoload 'enable-paredit-mode
    "paredit" "Turn on pseudo-structural editing in Lisp code" t)

  (dolist (mode-hook '(emacs-lisp-mode-hook
                       lisp-mode-hook
                       lisp-interaction-mode-hook
                       racket-mode-hook
                       scheme-mode-hook
                       clojure-mode-hook))
    (add-hook mode-hook #'enable-paredit-mode)))

;; Projectile

(when (package-installed-p 'projectile)
  (projectile-mode))

;; Yasnippet

(when (require 'yasnippet nil t)
  (setq yas-snippet-dirs
        (list (expand-file-name "snippets" init-config-folder)))
  (yas-global-mode 1))


;;; Major Modes ------------------------------------------------------

;; Elisp

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

(define-key emacs-lisp-mode-map (kbd "C-c C-e") #'eval-print-last-sexp)
;; scratch buffer doesn't pick up the changed mode map
(with-current-buffer "*scratch*"
  (local-set-key (kbd "C-c C-e") #'eval-print-last-sexp))

;; BQN

(when (require 'bqn-mode nil t)
  (define-key bqn-mode-map (kbd "C-c C-c") #'bqn-comint-send-dwim))

;; Clojure

(when (package-installed-p 'cider)
  (setq org-babel-clojure-backend 'cider)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (setq nrepl-hide-special-buffers t)
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-repl-display-help-banner nil))

;; Forth

(autoload 'forth-mode "gforth.el")
(setq auto-mode-alist (cons '("\\.fs\\'" . forth-mode)
                            auto-mode-alist))

(autoload 'forth-block-mode "gforth.el")
(setq auto-mode-alist (cons '("\\.fb\\'" . forth-block-mode)
                            auto-mode-alist))

(defun init-gforth-setup ()
  (setq forth-indent-level 2)
  (define-key forth-mode-map (kbd "C-c C-c") 'forth-send-paragraph))
(add-hook 'forth-mode-hook #'init-gforth-setup)

;; Julia-mode

(when (and (require 'julia-mode nil t)
           (require 'julia-repl nil t))
  (setq julia-repl-pop-to-buffer nil)
  (add-hook 'julia-mode-hook #'julia-repl-mode)

  (defvar init-julia-lsp-so (expand-file-name "lsp-julia/julia-lsp.so"
                                              init-cache-dir))
  (when (file-exists-p init-julia-lsp-so)
    ;; Recommendation according to https://github.com/gdkrmr/lsp-julia
    ;;
    ;; julia> using Pkg
    ;; julia> Pkg.add("PackageCompiler")
    ;; julia> using PackageCompiler
    ;; julia> Pkg.add("LanguageServer")
    ;; julia> create_sysimage(:LanguageServer, sysimage_path="/path/to/languageserver.so")
    (setq lsp-julia-package-dir nil)
    (setq lsp-julia-flags (list (format "-J%s" init-julia-lsp-so))))
  (when (and (functionp 'lsp-mode) (require 'lsp-julia nil t))
    (add-hook 'julia-mode-hook #'lsp-mode)))

;; Markdown-mode

(when (package-installed-p 'markdown-mode)
  (if (eq system-type 'gnu/linux)
      (setq markdown-command "/usr/bin/env markdown"))

  (add-to-list 'auto-mode-alist '("\\.txt" . markdown-mode))
  (autoload 'markdown-mode "markdown-mode" "Major mode for Markdown files" t))

;; Nov

(when (package-installed-p 'nov)
  (setq nov-text-width 80)

  ;; (defun my-nov-font-setup ()
  ;;   (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
  ;;                            :height 1.0))
  ;; (add-hook 'nov-mode-hook 'my-nov-font-setup)

  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; Haskell-mode

(when (package-installed-p 'haskell-mode)
  (add-hook 'haskell-mode-hook #'turn-on-haskell-indentation))

;; LaTeX/AucTeX

(setq TeX-auto-save nil)
(setq TeX-parse-self t)

(setq TeX-view-format "pdf")

(setq font-latex-fontify-script nil)
(setq font-latex-fontify-sectioning 1.0)

(when (package-installed-p 'auctex)
  (when (eq system-type 'windows-nt)
    (require 'tex-mik))

  (setq bibtex-align-at-equal-sign nil)
  (setq bibtex-comma-after-last-field t)

  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'TeX-mode-hook #'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)

  (when (require 'auctex-latexmk nil t)
    (auctex-latexmk-setup)))

;;; R

(when (require 'ess-r-mode nil t)
  (setq ess-indent-with-fancy-comments nil)
  (setq ess-use-eldoc t)
  (defun init-setup-ess ()
    (ess-set-style 'RStudio 'quiet))
  (add-hook 'ess-mode-hook #'init-setup-ess))

;; Racket-mode

(when (require 'racket-mode nil t)
  (require 'racket-xp)
  (define-key racket-repl-mode-map (kbd "C-c C-q") #'racket-repl-exit)
  (add-hook 'racket-mode-hook #'racket-xp-mode))

;; Slime/sly

(when (or (package-installed-p 'slime)
          (package-installed-p 'sly))
  (let ((slime-helper "~/.quicklisp/slime-helper.el"))
    (when (file-exists-p slime-helper)
      (load slime-helper)))
  (setq inferior-lisp-program "sbcl"))


;;; Org-mode Settings ------------------------------------------------

(require 'org)

(setq org-list-allow-alphabetical t)
(setq org-src-fontify-natively nil)
(setq org-adapt-indentation nil)

;; Agenda

(defvar init-agenda-dir "~/org/")

(setq org-log-done 'time)

(when (file-directory-p init-agenda-dir)
  (setq org-agenda-files
        (list (expand-file-name "todo.org" init-agenda-dir)
              (expand-file-name "termine.org" init-agenda-dir)))

  (setq org-archive-location
        (concat (file-name-as-directory init-agenda-dir)
                "archiv.org::datetree/")))

;; Babel

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (clojure . t)))

;; Latex export

(require 'ox-latex)
(require 'ox-beamer)

(when (eq system-type 'gnu/linux)
  (add-to-list 'org-file-apps '("\\.pdf" . "okular %s")))

(add-to-list 'org-latex-classes
             '("scrartcl"
               "\\documentclass[a4paper]{scrartcl}\n[DEFAULT-PACKAGES]\n[PACKAGES]\n\\widowpenalty=10000\n\\clubpenalty=10000\n[EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-packages-alist
             '("" "booktabs" t))

(setq org-latex-tables-booktabs t)
(setq org-latex-caption-above nil)
(setq org-latex-image-default-width nil)

(setq org-entities-user '(("space" "\\ " nil " " " " " " " ")
                          ("sentend" "\\@" nil "" "" "" "")))


;;; Input methods ----------------------------------------------------

(require 'quail)
(quail-set-keyboard-layout "pc102-de")

(register-input-method
 "english-shavian" "English" 'quail-use-package
 "𐑠𐑷" "English input method for Shavian (Imperial)"
 "shavian-input-method")

(register-input-method
 "english-shavian-qwerty" "English" 'quail-use-package
 "𐑠𐑷(Q)" "English input method for Shavian (QWERTY)"
 "shavian-input-method")


;;; Key Bindings -----------------------------------------------------

;; General

(when (daemonp)
  (defun init-modified-buffer-p (buf)
    (and (buffer-file-name buf)
         (buffer-modified-p buf)))

  (defun init-save-buffers-close-frame (&optional frame)
    (interactive)
    (save-some-buffers nil t)
    (when (or (not (memq t (mapcar #'init-modified-buffer-p (buffer-list))))
              (yes-or-no-p "Modified buffers exist; close frame anyway? "))
      (delete-frame frame t)))

  (defun init-handle-close-button (event)
    (interactive "e")
    (let (frame (posn-window (event-start event)))
      (init-save-buffers-close-frame frame)))

  (define-key special-event-map [delete-frame] #'init-handle-close-button)
  (global-set-key (kbd "C-x C-c") #'init-save-buffers-close-frame))

(global-set-key (kbd "C-c e") #'init-open-init.el)

(global-set-key (kbd "C-c f") #'find-file-at-point)

(global-set-key (kbd "C-c t") #'term)

(global-set-key (kbd "C-c q q") #'init-single-quote-region)
(global-set-key (kbd "C-c q Q") #'init-double-quote-region)
(global-set-key (kbd "C-c q d") #'init-kraut-quote-region)
(global-set-key (kbd "C-c q D") #'init-kraut-single-quote-region)
(global-set-key (kbd "C-c q k") #'init-kraut-quote-region-2)
(global-set-key (kbd "C-c q K") #'init-kraut-single-quote-region-2)

(global-set-key (kbd "C-c w w") #'make-frame)
(global-set-key (kbd "C-c w 1") #'delete-other-frames)
(global-set-key (kbd "C-c w p") #'init-pop-out-window)

;; never used this
(global-set-key (kbd "C-c c") #'fixup-whitespace)

;; Plugins

(global-set-key (kbd "C-c o l") #'org-store-link)
(global-set-key (kbd "C-c o c") #'org-capture)
(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o b") #'org-iswitchb)

;; for some reason, <menu> is called <apps> on windows...
(unless (package-installed-p 'smex)
  (global-set-key (kbd "<apps>") #'execute-extended-command))

(when (package-installed-p 'smex)
  (global-set-key (kbd "M-x") #'smex)
  (global-set-key (kbd "<menu>") #'smex)
  (global-set-key (kbd "<apps>") #'smex)
  (global-set-key (kbd "<execute>") #'smex))

(when (package-installed-p 'magit)
  (global-set-key (kbd "C-c g") #'magit-status))

(when (require 'multiple-cursors nil t)
  (defun init-adhoc-cursor-spawning-mode ()
    "Ad-hoc key map for spawning multiple cursors.

I am lazy and don't want to hit my prefix key every bloody time I
want to add a cursor so I just nicked the 'temporary key map'
idea from ‘text-scale-adjust’.

You invoke the mode with the following key presses, which invoke
the corresponding functions from ‘multiple-cursors-mode’.

C-c l ‘mc/mark-previous-like-this’
C-c n ‘mc/mark-next-like-this’
C-c L ‘mc/skip-to-previous-like-this’
C-c N ‘mc/skip-to-next-like-this’

Then you can keep just pressing l, n, L, or N to spawn more
cursors using the same functions."
    (interactive)
    (let ((ev last-command-event)
          (echo-keystrokes nil))
      (cond ((eq ev ?l) (mc/mark-previous-like-this 1))
            ((eq ev ?n) (mc/mark-next-like-this 1))
            ((eq ev ?L) (mc/skip-to-previous-like-this))
            ((eq ev ?N) (mc/skip-to-next-like-this)))
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (dolist (key '("l" "n" "L" "N"))
           (define-key map key #'init-adhoc-cursor-spawning-mode))
         map)
       nil nil)))
  ;; "Use %k to spawn more cursors."

  (dolist (key-str '("C-c l" "C-c n" "C-c L" "C-c N"))
    (global-set-key (kbd key-str) #'init-adhoc-cursor-spawning-mode))
  (global-set-key (kbd "C-c m") #'mc/edit-lines)
  (global-set-key (kbd "C-c M") #'mc/mark-all-like-this))

(when (package-installed-p 'projectile)
  (global-set-key (kbd "C-c p") #'projectile-command-map))

(when (package-installed-p 'god-mode)
  ;; get in with C-c c
  (global-set-key (kbd "C-c c") #'god-local-mode)
  ;; get out with escape
  (define-key god-local-mode-map (kbd "<escape>") #'god-local-mode))
