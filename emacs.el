;; Add MELPA to packages sources
;; Inspired by http://melpa.milkbox.net/#/getting-started
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; On older emacsen also add the base library
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))  

;; Initialize packages
(package-initialize)

(global-set-key (kbd "C-x g") 'magit-status) 
(global-set-key (kbd "<f12>") 'magit-status)

(setenv "GIT_ASKPASS" "git-gui--askpass")

(if (eq system-type 'windows-nt)
   (progn
     (setq exec-path (add-to-list 'exec-path "C:/Users/jpg/Progs/Git/bin"))
    ))

(if (eq system-type 'windows-nt) ; Actually trying to detect my work pc, may need to change this later on
  (setq org-directory (substitute-in-file-name "$USERPROFILE/Dropbox/org/"))
)

(setq org-agenda-files "~/.emacs.d/org-agenda-files") ; Use a single file name, so lookup agenda files in that file (see help on org-agenda-files)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(add-to-list 'org-structure-template-alist (list "S" "#+BEGIN_SRC emacs-lisp?\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>" ))

(setq org-M-RET-may-split-line nil)

(setq org-todo-keywords
      '((sequence "B(b)" "A(a)" "C(c)" "WAITING(w)" "|" "DONE(d)" )))

(setq org-stuck-projects '("Project/-DONE" nil ("NEXT") ""))

(if (not (boundp 'org-agenda-custom-commands)) (set 'org-agenda-custom-commands ()) )

  (push 
    '("A" "Joris' daily agenda view" (
      (todo "A" ((org-agenda-overriding-header "Wat moet er vandaag in ieder geval gebeuren:"))) 
      (tags "NEXT" ((org-agenda-overriding-header "Volgende acties voor de projecten:")))
      (stuck "" nil)) 
     ((org-agenda-prefix-format 
       '((agenda . " %i %-12:c%?-12t% s")
       (timeline . "  % s")
;       (todo . "%(concat \"[\" (format \"%-15s\" (org-format-outline-path (org-get-outline-path) 13)) \"] \")")
       (todo . "%(concat \"[\" (format \"%-15s\" (or (car (last (org-get-outline-path)))) \"\") \"] \")")
;       (tags . "%(concat \"[\" (format \"%-15s\" (org-format-outline-path (org-get-outline-path) 13)) \"] \")")
       (tags . "%(concat \"[\" (format \"%-15s\" (or (car (last (org-get-outline-path) ))) \"\") \"] \")")
       (search . " %i %-8:c"))
      )) ())       
      org-agenda-custom-commands )

;; show matching parens
(show-paren-mode 1)

;; take the short answer, y/n is yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "RET") 'newline-and-indent)

(setq sentence-end-double-space nil)

(setq split-height-threshold 60)
(setq split-width-threshold 90)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(setq ido-create-new-buffer 'always)

(setq ido-ignore-extensions t)

(global-set-key
   "\M-x"
   (lambda ()
     (interactive)
     (call-interactively
      (intern
       (ido-completing-read
        "M-x "
        (all-completions "" obarray 'commandp))))))

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(global-set-key (kbd "M-j")
(lambda ()
(interactive)
(join-line -1)))

;; Remove unused UI elements
 (add-hook 'window-setup-hook (lambda () (tool-bar-mode -1))) 
;  (tool-bar-mode 0) Conflicts with maximization on windows, so need the hook above
  (menu-bar-mode 0)
  (scroll-bar-mode 0)
  (setq inhibit-startup-message t)
  
  ;; shhht, give me some time to think, don't blink
  (blink-cursor-mode 0)

;;    (require 'color-theme)
;;    (color-theme-initialize)
;;    (color-theme-kingsajz)
(load-theme 'zenburn t)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list" t)))

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(if (eq system-type 'windows-nt)
   (tool-bar-mode 1)
   (w32-send-sys-command 61488) ; Does not work with toolbar diabled, so put that on a hook above
  )

;   (add-hook 'after-init-hook '(lambda () (w32-send-sys-command #xf030))))

(setq initial-frame-alist (quote ((fullscreen . maximized))))

;    (find-file "~/personal/organizer.org")
;    (require 'org-compat)
    (run-at-time (format "%d sec" 1) nil '(lambda () (org-agenda nil "A")))
;    (add-hook 'after-init-hook '(lambda () (progn (org-agenda nil "A") (other-window 1))))
