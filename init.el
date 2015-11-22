;;; init.el --- user init file      -*- no-byte-compile: t -*-
;; Profiling of emacs init process (see https://github.com/dholm/benchmark-init-el)
;(add-to-list 'load-path "~/.emacs.d/el-get/benchmark-init/")
;(require 'benchmark-init)
;(let ((benchmark-init.el "~/.emacs.d/el-get/benchmark-init/benchmark-init.el"))
;  (when (file-exists-p benchmark-init.el)
;    (load benchmark-init.el)))

;; Test: proves that this init file is being found 
;(tool-bar-mode -1)

;; Emacs startup file.

;; define dotfiles-dir where all the files live.
(setq dotfiles-dir (file-name-directory
		    (or load-file-name (buffer-file-name))))

;; Add load paths
; TODO use dotfiles-dir
(add-to-list 'load-path "~/.emacs.d/lisp/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp/org-mode/contrib/lisp")

;; Load up Org Mode and Babel
(require 'org)
(require 'ob-tangle)

;; Correct exec path before loading the rest (only on Mac)
;; Fails the first time round because we do not yet have exec-path-from-shell installed
;; This is installed through use-package in the emacs.org
(when (featurep 'exec-path-from-shell)
      (when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)))

;; load up the main file
(org-babel-load-file (expand-file-name "emacs.org" dotfiles-dir))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default))))
