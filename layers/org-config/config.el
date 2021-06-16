;; Refile
;;; define the refile targets
(defvar org-agenda-dir "" "gtd org files location")
(setq-default org-agenda-dir "~/org/agenda")
(setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
(setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
(setq org-agenda-file-inbox (expand-file-name "inbox.org" org-agenda-dir))
(setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
(setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
(setq org-default-notes-file (expand-file-name "inbox.org" org-agenda-dir))
(setq org-agenda-files (list org-agenda-dir))

;;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
;;; Use IDO for both buffer and file completion and ido-everywhere to t
;;;; (setq org-completion-use-ido t)
;;;; (setq ido-everywhere t)
;;;; (setq ido-max-directory-size 100000)
;;;; (ido-mode (quote both))
;;; Use the current window when visiting files and buffers with ido
;;;; (setq ido-default-file-method 'selected-window)
;;;; (setq ido-default-buffer-method 'selected-window)
;;; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)


;;; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

;;; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

;;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;;;; Refile settings
;;;; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)


;; Org Clock
;;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

;; Other

;;; org mode truncate-lines
(add-hook 'org-mode-hook
          (lambda()
            (setq truncate-lines nil)))

;;; Todo Dependency
(setq org-enforce-todo-dependencies t)
