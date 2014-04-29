;; Remove unused UI elements
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-startup-message t)

;; shhht, give me some time to think, don't blink
(blink-cursor-mode 0)

;; show matching parens
(show-paren-mode 1)

;; take the short answer, y/n is yes/no
(defalias 'yes-or-no-p 'y-or-n-p)
