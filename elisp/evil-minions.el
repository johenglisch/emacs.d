;;; -*- lexical-binding: t -*-

;;; Helper Functions for Evil-mode

(defun imap (key function)
  (define-key evil-insert-state-map (kbd key) function))

(defun nmap (key function)
  (define-key evil-normal-state-map (kbd key) function))

(defun vmap (key function)
  (define-key evil-visual-state-map (kbd key) function))

(defun omap (key function)
  (define-key evil-operator-state-map (kbd key) function))

(defun mmap (key function)
  (define-key evil-motion-state-map (kbd key) function))

(defun rmap (key function)
  (define-key evil-replace-state-map (kbd key) function))
