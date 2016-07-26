;;; -*- lexical-binding: t -*-

;;; Helper Functions for Evil-mode

(defun i-map (key function)
  (define-key evil-insert-state-map (kbd key) function))

(defun n-map (key function)
  (define-key evil-normal-state-map (kbd key) function))

(defun v-map (key function)
  (define-key evil-visual-state-map (kbd key) function))

(defun o-map (key function)
  (define-key evil-operator-state-map (kbd key) function))

(defun m-map (key function)
  (define-key evil-motion-state-map (kbd key) function))

(defun r-map (key function)
  (define-key evil-replace-state-map (kbd key) function))
