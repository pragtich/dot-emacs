;;-*-coding: utf-8;-*-
(define-abbrev-table 'Buffer-menu-mode-abbrev-table '())

(define-abbrev-table 'Info-edit-mode-abbrev-table '())

(define-abbrev-table 'bibtex-mode-abbrev-table '())

(define-abbrev-table 'calendar-mode-abbrev-table '())

(define-abbrev-table 'change-log-mode-abbrev-table '())

(define-abbrev-table 'comint-mode-abbrev-table '())

(define-abbrev-table 'completion-list-mode-abbrev-table '())

(define-abbrev-table 'diff-mode-abbrev-table '())

(define-abbrev-table 'edit-abbrevs-mode-abbrev-table '())

(define-abbrev-table 'emacs-lisp-byte-code-mode-abbrev-table '())

(define-abbrev-table 'emacs-lisp-mode-abbrev-table '())

(define-abbrev-table 'epa-info-mode-abbrev-table '())

(define-abbrev-table 'epa-key-list-mode-abbrev-table '())

(define-abbrev-table 'epa-key-mode-abbrev-table '())

(define-abbrev-table 'fundamental-mode-abbrev-table '())

(define-abbrev-table 'git-commit-mode-abbrev-table '())

(define-abbrev-table 'git-rebase-mode-abbrev-table '())

(define-abbrev-table 'global-abbrev-table
  '(
    ("backwardchar" "" (lambda nil (deferred-execution (quote backward-char) t)) 0)
    ("backwardkillword" "" (lambda nil (deferred-execution (quote backward-kill-word))) 0)
    ("backwardword" "" (lambda nil (deferred-execution (quote backward-word) t)) 0)
    ("beginningofline" "" (lambda nil (deferred-execution (quote beginning-of-line) t)) 0)
    ("dash" "" (lambda nil (electric-erase-word-boundaries) (dash-function)) 0)
    ("deletechar" "" (lambda nil (deferred-execution (quote delete-char))) 0)
    ("endofline" "" (lambda nil (deferred-execution (quote end-of-line) t)) 0)
    ("expandword" "" (lambda nil (deferred-execution (quote expand-a-word))) 0)
    ("forwardchar" "" (lambda nil (deferred-execution (quote forward-char) t)) 0)
    ("forwardword" "" (lambda nil (deferred-execution (quote forward-word) t)) 0)
    ("killline" "" (lambda nil (deferred-execution (quote kill-line))) 0)
    ("killword" "" (lambda nil (deferred-execution (quote kill-word))) 0)
    ("nextline" "" (lambda nil (deferred-execution (quote next-line) t)) 0)
    ("previousline" "" (lambda nil (deferred-execution (quote previous-line) t)) 0)
    ("templateparameter" "" (lambda nil (deferred-execution (quote c++-template-parameter))) 0)
   ))

(define-abbrev-table 'grep-mode-abbrev-table '())

(define-abbrev-table 'help-mode-abbrev-table '())

(define-abbrev-table 'lisp-mode-abbrev-table '())

(define-abbrev-table 'log-edit-mode-abbrev-table '())

(define-abbrev-table 'magit-branch-manager-mode-abbrev-table '())

(define-abbrev-table 'magit-cherry-mode-abbrev-table '())

(define-abbrev-table 'magit-commit-mode-abbrev-table '())

(define-abbrev-table 'magit-diff-mode-abbrev-table '())

(define-abbrev-table 'magit-log-mode-abbrev-table '())

(define-abbrev-table 'magit-mode-abbrev-table '())

(define-abbrev-table 'magit-process-mode-abbrev-table '())

(define-abbrev-table 'magit-reflog-mode-abbrev-table '())

(define-abbrev-table 'magit-status-mode-abbrev-table '())

(define-abbrev-table 'magit-wazzup-mode-abbrev-table '())

(define-abbrev-table 'message-mode-abbrev-table '())

(define-abbrev-table 'messages-buffer-mode-abbrev-table '())

(define-abbrev-table 'occur-edit-mode-abbrev-table '())

(define-abbrev-table 'occur-mode-abbrev-table '())

(define-abbrev-table 'org-mode-abbrev-table '())

(define-abbrev-table 'outline-mode-abbrev-table '())

(define-abbrev-table 'package-menu-mode-abbrev-table '())

(define-abbrev-table 'process-menu-mode-abbrev-table '())

(define-abbrev-table 'prog-mode-abbrev-table '())

(define-abbrev-table 'shell-mode-abbrev-table '())

(define-abbrev-table 'special-mode-abbrev-table '())

(define-abbrev-table 'tabulated-list-mode-abbrev-table '())

(define-abbrev-table 'text-mode-abbrev-table '())

(define-abbrev-table 'vc-git-log-edit-mode-abbrev-table '())

(define-abbrev-table 'vc-git-log-view-mode-abbrev-table '())

