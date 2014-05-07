
;; Test: proves that this init file is being found 
(tool-bar-mode -1)

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
 
