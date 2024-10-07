;;; -*- lexical-binding: t -*-

;; symlink this file to .emacs

(defconst init-config-folder
  (let ((resolved (file-symlink-p load-file-name)))
    (when (not resolved)
      (error "this aint gonna work if %s is not a symlink into the config folder!"
             load-file-name))
    (let ((actual-file (if (file-name-absolute-p resolved)
                           resolved
                         (expand-file-name resolved
                                           (file-name-directory load-file-name)))))
      (file-name-directory actual-file)))
  "Folder that contains all the lisp code for configuring emacs.")

(load (expand-file-name "init.el" init-config-folder))
