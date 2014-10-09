(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-pragtich/recipes")
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(setq el-get-sources
    '((:name magit                          ;Adds a keybinding to standard magit setup
      :after (progn
        (global-set-key (kbd "C-x g") 'magit-status) 
        (global-set-key (kbd "<f12>") 'magit-status) ))
    (:name ido-vertical-mode                ;Show ido results vertically
     :after (progn
       (ido-mode 1) 
       (ido-vertical-mode 1)
       (setq ido-enable-flex-matching t)
       (setq ido-everywhere t)
       (setq ido-create-new-buffer 'always)  ;Prevent IDO from asking when I just want to make a scratch buffer.
       (setq ido-ignore-extensions t)        ;Ignore predefined useless extensions which are defined in =completion-ignored-extensions=.
  ; M-x mode
  ;; Reenable ido for M-x
  ))
  (:name ido-ubiquitous
   :features ido-ubiquitous
   :after (progn
     (ido-ubiquitous-mode 1)
     (setq ido-ubiquitous-command-overrides
       (cons '(enable exact "execute-extended-command") ido-ubiquitous-default-command-overrides)))
)))

(setq pragtich/packages
    (append 
      '( "cl-lib" "color-theme-zenburn" "el-get" "git-modes" "package")))
  ;; An add the customized packages too:
(setq pragtich/packages
      (append pragtich/packages
              (mapcar #'el-get-source-name el-get-sources)))

(el-get 'sync pragtich/packages)

(setenv "GIT_ASKPASS" "git-gui--askpass")

(if (eq system-type 'windows-nt)
   (progn
     (setq exec-path (add-to-list 'exec-path "C:/Users/jpg/Progs/Git/bin"))
    ))

(when (eq system-type 'darwin)
  ; Start the emacs server with a predictable pipe name
  (setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))
  (server-start))
  ; And let me quit with C-c C-c when editing in server mode
  (add-hook 'server-switch-hook '(lambda ()
                                  (local-set-key [(control c) (control c)]
                                                 (lambda ()
                                                   (interactive)
                                                   (save-buffer)
                                                   (server-edit)))))

(if (eq system-type 'windows-nt) ; Actually trying to detect my work pc, may need to change this later on
  (setq org-directory (substitute-in-file-name "$USERPROFILE/Dropbox/org/"))
  (setq org-directory "~/org/")
)

(setq org-agenda-files "~/.emacs.d/org-agenda-files") ; Use a single file name, so lookup agenda files in that file (see help on org-agenda-files)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(add-to-list 'org-structure-template-alist (list "S" "#+BEGIN_SRC emacs-lisp?\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>" ))

(setq org-M-RET-may-split-line nil)

(setq org-completion-use-ido t)

(setq org-hide-leading-stars t)

(setq org-src-fontify-natively t)

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

(setq org-todo-keywords
      '((sequence "B(b)" "A(a)" "C(c)" "WAITING(w)" "|" "DONE(d)" ))) 
(setq org-todo-keyword-faces
      '(("A" . "yellow")
        ("WAITING". "orange")))

(setq org-fast-tag-selection-single-key t)

(setq org-special-ctrl-a/e t)

(setq org-cycle-global-at-bob t)

(setq org-ellipsis "\u2026")    ;" \u22bf" )
;(setq org-ellipsis (quote org-tag))

;; show matching parens
(show-paren-mode 1)

;; take the short answer, y/n is yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "RET") 'newline-and-indent)

(setq sentence-end-double-space nil)

(setq split-height-threshold 60)
(setq split-width-threshold 90)

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

( setq delete-by-moving-to-trash t)

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

(when (eq system-type 'windows-nt)
   (tool-bar-mode 1)
   (w32-send-sys-command 61488) ; Does not work with toolbar diabled, so put that on a hook above
  )

;   (add-hook 'after-init-hook '(lambda () (w32-send-sys-command #xf030))))

(setq initial-frame-alist (quote ((fullscreen . maximized))))

;    (find-file "~/personal/organizer.org")
;    (require 'org-compat)
    (when (eq system-type 'windows-nt)  ;Only open file when at work: should use system-name or something
     ; Open file
     (find-file (expand-file-name "jpg.org" org-directory))
     ; run agenda command
     (run-at-time (format "%d sec" 1) nil '(lambda () (progn (org-agenda nil "A")) (other-window 1)))
    )
;    (add-hook 'after-init-hook '(lambda () (progn (org-agenda nil "A") (other-window 1))))

(if (eq system-type 'windows-nt)
    (progn (add-to-list 'load-path (substitute-in-file-name "C:/Users/jpg/Progs/VR-mode/"))

      (setq vr-command "C:\\Users\\jpg\\Progs\\VR-mode\\vr.exe")
      (setq vr-win-class "Emacs")
      (load "vr")
    ;(autoload 'vr-mode "C:/Users/jpg/Progs/VR-mode/vr" "" t nil) 
))
