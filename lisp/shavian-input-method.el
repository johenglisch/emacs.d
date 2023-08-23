;;; shavian-input-method.el --- Input methods for the Shavian script.

;;; Commentary:

;; This package adds two input methods for the Shavian script:
;; 1) The Shaw Imperial layout
;; 2) The QWERTY layout

;;; Code:

(require 'quail)

(quail-define-package
 "english-shavian" "English" "𐑠𐑷"
 t
 "English input method using the Shavian script.

Uses the Shaw Imperial keyboard layout."
 nil t t t t nil nil nil nil nil t)

;; FIXME no \
;; FIXME no |
(quail-define-rules
  ("´" ?·) ("1" ?𐑶) ("2" ?𐑬) ("3" ?𐑫) ("4" ?𐑜) ("5" ?𐑖) ("6" ?𐑗) ("7" ?𐑙)
  ("8" ?𐑘) ("9" ?𐑡) ("0" ?𐑔) ("-" ?-) ("=" ?=)

  ("q" ?𐑭) ("w" ?𐑷) ("e" ?𐑵) ("r" ?𐑱) ("t" ?𐑳) ("y" ?𐑓) ("u" ?𐑞) ("i" ?𐑤)
  ("o" ?𐑥) ("p" ?𐑒) ("[" ?𐑢) ("]" ?𐑣) ("\\" ?𐑠)

  ("a" ?𐑪) ("s" ?𐑨) ("d" ?𐑦) ("f" ?𐑩) ("g" ?𐑧) ("h" ?𐑐) ("j" ?𐑯) ("k" ?𐑑)
  ("l" ?𐑮) (";" ?𐑕) ("'" ?𐑛)

  ("z" ?𐑾) ("x" ?𐑲) ("c" ?𐑴) ("v" ?𐑰) ("b" ?𐑚) ("n" ?𐑝) ("m" ?𐑟) ("," ?,)
  ("." ?.) ("/" ?/)

  ("~" ?⸰) ("!" ?1) ("@" ?2) ("#" ?3) ("$" ?4) ("%" ?5) ("^" ?6) ("&" ?7)
  ("*" ?8) ("(" ?9) (")" ?0) ("_" ?_) ("+" ?+)

  ("Q" ?𐑸) ("W" ?𐑹) ("E" ?𐑿) ("R" ?𐑺) ("T" ?𐑻) ("Y" ?¥) ("U" ?€) ("I" ?‽)
  ("O" ?°) ("P" ?%) ("{" ?\;) ("}" ?') ("|" ?/)

  ("A" ?@) ("S" ?*) ("D" ?$) ("F" ?𐑼) ("G" ?£) ("H" ?#) ("J" ?…) ("K" ?\«)
  ("L" ?\») (";" ?:) ("\"" ?\")

  ("Z" ?𐑽) ("X" ?!) ("C" ?^) ("V" ?\() ("B" ?\)) ("N" ?–) ("M" ?—) ("<" ?‹)
  (">" ?›) ("?" ??))


(quail-define-package
 "english-shavian-qwerty" "English" "𐑠𐑷(Q)" t
 "English input method using the Shavian script.

Familiar QWERTY layout for the uninitiated."
 nil t t t t nil nil nil nil nil t)

;; Note:
;;  * replaced the en-dash – with a hyphen -
;;  * replaced the em-dash — with an en-dash –
(quail-define-rules
  ("´" ?~) ("1" ?1) ("2" ?2) ("3" ?3) ("4" ?4) ("5" ?5) ("6" ?6) ("7" ?7)
  ("8" ?8) ("9" ?9) ("0" ?0) ("-" ?-) ("=" ?=)

  ("q" ?𐑶) ("w" ?𐑢) ("e" ?𐑧) ("r" ?𐑮) ("t" ?𐑑) ("y" ?𐑭) ("u" ?𐑳) ("i" ?𐑦)
  ("o" ?𐑪) ("p" ?𐑐) ("[" ?\[) ("]" ?\]) ("\\" ?\\)

  ("a" ?𐑩) ("s" ?𐑕) ("d" ?𐑛) ("f" ?𐑓) ("g" ?𐑜) ("h" ?𐑣) ("j" ?𐑘) ("k" ?𐑒)
  ("l" ?𐑤) (";" ?\;) ("'" ?')

  ("z" ?𐑟) ("x" ?𐑻) ("c" ?𐑗) ("v" ?𐑝) ("b" ?𐑚) ("n" ?𐑯) ("m" ?𐑥) ("," ?,)
  ("." ?.) ("/" ?/)

  ("~" ?‽) ("!" ?!) ("@" ?@) ("#" ?#) ("$" ?$) ("%" ?%) ("^" ?^) ("&" ?&)
  ("*" ?*) ("(" ?\() (")" ?\)) ("_" ?–) ("+" ?+)

  ("Q" ?𐑬) ("W" ?𐑾) ("E" ?𐑱) ("R" ?𐑸) ("T" ?𐑔) ("Y" ?𐑷) ("U" ?𐑫) ("I" ?𐑰)
  ("O" ?𐑴) ("P" ?𐑹) ("{" ?{) ("}" ?}) ("|" ?|)

  ("A" ?𐑨) ("S" ?𐑖) ("D" ?𐑼) ("F" ?𐑲) ("G" ?·) ("H" ?𐑞) ("J" ?𐑡) ("K" ?\«)
  ("L" ?\») (";" ?:) ("\"" ?\")

  ("Z" ?𐑠) ("X" ?𐑺) ("C" ?𐑽) ("V" ?𐑿) ("B" ?⸰) ("N" ?𐑙) ("M" ?𐑵) ("<" ?\‹)
  (">" ?\›) ("?" ??))


(provide 'shavian-input-method)
;;; shavian-input-method.el ends here
