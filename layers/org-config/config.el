;; Todo
(setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "UNCLEAR(u)" "ASK(a)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING" ))))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("UNCLEAR" :foreground "light blue" :weight bold)
                ("ASK" :foreground "blue" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("MEETING" :foreground "forest green" :weight bold)
                ("PHONE" :foreground "forest green" :weight bold))))
  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("WAITING" ("WAITING" . t))
                ("HOLD" ("WAITING") ("HOLD" . t))
                (done ("WAITING") ("HOLD"))
                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

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
;;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "Todo" entry (file org-agenda-file-inbox)
               "* TODO [#B] %? \nSCHEDULED: <%(org-read-date nil nil \"\")>\n%U\n%a\n\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file org-agenda-file-inbox)
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file+headline org-agenda-file-note "Notes")
               "* %? :NOTE:\n%U\n%a\n\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree org-agenda-file-journal)
               "* %?\n%U\n\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file org-agenda-file-inbox)
               "* TODO Review %c\n%U\n\n" :immediate-finish t)
              ("m" "Meeting" entry (file+datetree org-agenda-file-journal)

               "* MEETING with %? :MEETING:\n%U\n" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file+datetree org-agenda-file-journal)
               "* PHONE %? :PHONE:\n%U\n" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file org-agenda-file-inbox)
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n\n"))))

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

;; Agenda
;;; time grid
(defun my:org-agenda-time-grid-spacing ()
    "Set different line spacing w.r.t. time duration."
    (save-excursion
      (let* ((background (alist-get 'background-mode (frame-parameters)))
             (background-dark-p (string= background "dark"))
             (colors (if background-dark-p
                         (list "#aa557f" "DarkGreen" "DarkSlateGray" "DarkSlateBlue")
                       (list "#F6B1C3" "#FFFF9D" "#BEEB9F" "#ADD5F7")))
             pos
             duration)
        (nconc colors colors)
        (goto-char (point-min))
        (while (setq pos (next-single-property-change (point) 'duration))
          (goto-char pos)
          (when (and (not (equal pos (point-at-eol)))
                     (setq duration (org-get-at-bol 'duration)))
            (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
                  (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
              (overlay-put ov 'face `(:background ,(car colors)
                                                  :foreground
                                                  ,(if background-dark-p "black" "white")))
              (setq colors (cdr colors))
              (overlay-put ov 'line-height line-height)
              (overlay-put ov 'line-spacing (1- line-height))))))))
  (add-hook 'org-agenda-finalize-hook #'my:org-agenda-time-grid-spacing)

;; Org Column
;;; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%38ITEM(Details) %TAGS(Context) %7TODO(To Do) %3PRIORITY %5Effort(Time){:} %6CLOCKSUM{:}")
