;;; Flycheck

(when (package-installed-p 'flycheck)
  (when (package-installed-p 'flycheck-clojure)
    (eval-after-load 'flycheck
      '(flycheck-clojure-setup)))

  (when (package-installed-p 'flycheck-pos-tip)
    (eval-after-load 'flycheck
      '(setq flycheck-display-errors-function
             #'flycheck-pos-tip-error-messages))))
