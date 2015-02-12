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

;; load up the main file
(org-babel-load-file (expand-file-name "emacs.org" dotfiles-dir))
