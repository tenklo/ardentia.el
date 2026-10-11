;;; ardentia-dashboard.el --- Dashboard and review inbox for ardentia -*- lexical-binding: t -*-

;; Copyright (C) 2026 Karsten Klöss

;; Author: Karsten Klöss
;; URL: https://github.com/tenklo/ardentia.el
;; Keywords: outlines, tools
;; Package-Requires: ((emacs "30.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `ardentia-dashboard' lists the agent queues (trouble, review, running, ready,
;; later) in a `tabulated-list-mode' buffer.  The trouble group holds the
;; open tasks tagged for errors or sync problems and the running Syncthing
;; merges; it is absent when there is nothing in it.
;; `ardentia-review-inbox' is the same view filtered to the review queue.
;; Refreshing is asynchronous.
;;
;; The header line shows host, timer, refresh time and usage.  In a narrow
;; window the timer times are shortened and what still does not fit moves
;; into lines at the top of the buffer.

;;; Code:

(require 'tabulated-list)
(require 'seq)
(require 'subr-x)
(require 'ardentia-core)
(require 'ardentia-actions)
(require 'ardentia-review)
(require 'ardentia-session)
(require 'ardentia-run)
(require 'ardentia-usage)

(declare-function ardentia "ardentia" ())
(declare-function ardentia-completed "ardentia-completed" ())
(declare-function dired "dired" (dirname &optional switches))

(defvar-local ardentia-dashboard--filter nil
  "Queue symbol this dashboard buffer is restricted to, or nil for all.")

(defun ardentia-dashboard--short-time (stamp &optional now)
  "Shorten the systemd time STAMP to `HH:MM' or `Ddd HH:MM'.
STAMP looks like \"Sun 2026-10-11 08:45:17 CEST\".  The date part is
dropped when it is the local date of NOW (default: the current time).
Anything else, including nil, is returned unchanged."
  (if (and (stringp stamp)
           (string-match
            "\\`\\([A-Za-z]+\\) \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) \\([0-9]\\{2\\}:[0-9]\\{2\\}\\)"
            stamp))
      (let ((day (match-string 1 stamp))
            (date (match-string 2 stamp))
            (hm (match-string 3 stamp)))
        (if (equal date (format-time-string "%F" now))
            hm
          (concat day " " hm)))
    stamp))

;;;; Colors

(defcustom ardentia-dashboard-colors t
  "Non-nil means the dashboard uses faces for queue, status, flags and more.
The faces are in the group `ardentia-faces' and inherit theme faces.
Nil shows plain text.  Takes effect on the next refresh."
  :type 'boolean
  :group 'ardentia)

(defgroup ardentia-faces nil
  "Faces of the ardentia dashboard."
  :group 'ardentia)

(defface ardentia-queue-trouble
  '((t :inherit error :weight normal))
  "Queue column of a task in trouble."
  :group 'ardentia-faces)

(defface ardentia-queue-review
  '((t :inherit font-lock-keyword-face :weight normal))
  "Queue column of a task waiting for review."
  :group 'ardentia-faces)

(defface ardentia-queue-running
  '((t :inherit font-lock-string-face :weight normal))
  "Queue column of a running task."
  :group 'ardentia-faces)

(defface ardentia-queue-ready
  '((t nil))
  "Queue column of a ready task."
  :group 'ardentia-faces)

(defface ardentia-queue-later
  '((t :inherit shadow))
  "Queue column of a task that is not yet due."
  :group 'ardentia-faces)

(defface ardentia-status-completed
  '((t :inherit success :weight normal))
  "Status column of a completed task."
  :group 'ardentia-faces)

(defface ardentia-status-partial
  '((t :inherit warning :weight normal))
  "Status column of a partially completed task."
  :group 'ardentia-faces)

(defface ardentia-status-blocked
  '((t :inherit error :weight normal))
  "Status column of a blocked task."
  :group 'ardentia-faces)

(defface ardentia-priority-highest
  '((t :inherit warning :weight normal))
  "Priority token of the highest priority at the start of a title."
  :group 'ardentia-faces)

(defface ardentia-priority
  '((t :inherit font-lock-constant-face :weight normal))
  "Priority token of the other priorities at the start of a title."
  :group 'ardentia-faces)

(defface ardentia-flag-attention
  '((t :inherit warning :weight normal))
  "Flags that ask for attention, and an attempt count ending in \"!\"."
  :group 'ardentia-faces)

(defface ardentia-flag-trouble
  '((t :inherit error :weight normal))
  "Trouble kind, merging flag and error text in the header line."
  :group 'ardentia-faces)

(defface ardentia-flag-info
  '((t :inherit shadow))
  "Informational flags: optional follow-up, due time, refreshing."
  :group 'ardentia-faces)

(defface ardentia-dashboard-meta
  '((t :inherit shadow))
  "Host, age, cost and file columns."
  :group 'ardentia-faces)

(defvar org-priority-highest)

(defun ardentia-dashboard--add-face (string face &optional start end)
  "Add FACE to STRING between START and END (default all) and return STRING."
  (add-face-text-property (or start 0) (or end (length string)) face nil string)
  string)

(defun ardentia-dashboard--maybe-face (string face)
  "Return STRING with FACE when `ardentia-dashboard-colors' is non-nil."
  (if ardentia-dashboard-colors
      (propertize string 'face face)
    string))

(defun ardentia-dashboard--colorize-flags (flags trouble)
  "Return a copy of the FLAGS string with faces.
TROUBLE non-nil means FLAGS starts with the trouble kind."
  (let ((s (copy-sequence flags))
        (pos 0))
    (when (and trouble (string-match "\\`[^ ]+" s))
      (ardentia-dashboard--add-face s 'ardentia-flag-trouble 0 (match-end 0))
      (setq pos (match-end 0)))
    (while (string-match
            (concat "\\(?:\\`\\| \\)"
                    "\\(?:\\(you!\\|limit\\|stale\\?\\)\\|\\(merging\\)"
                    "\\|\\(opt\\|due \\(?:in [0-9a-z]+\\|[0-9:-]+\\)\\)\\)"
                    "\\(?: \\|\\'\\)")
            s pos)
      (let ((face (cond ((match-beginning 1) 'ardentia-flag-attention)
                        ((match-beginning 2) 'ardentia-flag-trouble)
                        (t 'ardentia-flag-info)))
            (beg (or (match-beginning 1) (match-beginning 2)
                     (match-beginning 3)))
            (end (or (match-end 1) (match-end 2) (match-end 3))))
        (ardentia-dashboard--add-face s face beg end)
        (setq pos end)))
    s))

(defun ardentia-dashboard--colorize-row (task row _now)
  "Return a copy of ROW for TASK with faces.  The text stays the same.
Strings of ROW and TASK are never modified.  Without
`ardentia-dashboard-colors' ROW is returned as it is."
  (if (not ardentia-dashboard-colors)
      row
    (let* ((out (copy-sequence row))
           (put (lambda (i face)
                  (unless (string-empty-p (aref out i))
                    (aset out i (ardentia-dashboard--add-face
                                 (copy-sequence (aref out i)) face)))))
           (queue (aref row 0))
           (status (aref row 1))
           (att (aref row 3))
           (title (aref row 8)))
      (when-let* ((face (pcase queue
                          ("trouble" 'ardentia-queue-trouble)
                          ("review" 'ardentia-queue-review)
                          ("running" 'ardentia-queue-running)
                          ("ready" 'ardentia-queue-ready)
                          ("later" 'ardentia-queue-later))))
        (funcall put 0 face))
      (when-let* ((face (pcase status
                          ("completed" 'ardentia-status-completed)
                          ("partial" 'ardentia-status-partial)
                          ("blocked" 'ardentia-status-blocked))))
        (funcall put 1 face))
      (dolist (i '(2 4 6 7))
        (funcall put i 'ardentia-dashboard-meta))
      (when (string-suffix-p "!" att)
        (funcall put 3 'ardentia-flag-attention))
      (unless (string-empty-p (aref row 5))
        (aset out 5 (ardentia-dashboard--colorize-flags
                     (aref row 5)
                     (or (plist-get task :trouble) (ardentia--merge-p task)))))
      (when (and (plist-get task :priority)
                 (string-match "\\`\\[#\\(.\\)\\]" title))
        (let ((highest (char-to-string
                        (if (boundp 'org-priority-highest)
                            org-priority-highest
                          ?A))))
          (aset out 8 (ardentia-dashboard--add-face
                       (copy-sequence title)
                       (if (equal (match-string 1 title) highest)
                           'ardentia-priority-highest
                         'ardentia-priority)
                       0 (match-end 0)))))
      out)))

(defun ardentia-dashboard--info-segments (&optional state compact now)
  "Return the info line from STATE as a list of strings.
The first is \" Host: X\", every further one starts with \" | \".  With
COMPACT the timer times are shortened (see
`ardentia-dashboard--short-time', relative to NOW).  STATE defaults to
`ardentia--state'."
  (let* ((st (or state ardentia--state))
         (timer (plist-get st :timer))
         (time (plist-get st :time))
         (last (if st (or (plist-get timer :last) "unknown") "unknown"))
         (next (cond ((plist-get timer :running) "after the current run")
                     (st (or (plist-get timer :next) "unknown"))
                     (t "unknown")))
         (usage (ardentia-usage-header-string)))
    (when compact
      (setq last (ardentia-dashboard--short-time last now)
            next (ardentia-dashboard--short-time next now)))
    (append
     (list (format " Host: %s" (or (plist-get st :host) "unknown"))
           (format " | Timer last: %s next: %s" last next)
           (format " | Refreshed: %s"
                   (if time (format-time-string "%H:%M:%S" time) "never")))
     (and (plist-get st :error)
          (list (ardentia-dashboard--maybe-face
                 (format " | ERROR: %s" (plist-get st :error))
                 'ardentia-flag-trouble)))
     (and (plist-get st :refreshing)
          (list (ardentia-dashboard--maybe-face " | refreshing..."
                                                'ardentia-flag-info)))
     (mapcar (lambda (part) (concat " | " part))
             (split-string usage " | " t)))))

(defun ardentia-dashboard--info-line (&optional state)
  "Return the full header line text from STATE (default `ardentia--state')."
  (apply #'concat (ardentia-dashboard--info-segments state)))

(defun ardentia-dashboard--layout (width &optional state now)
  "Decide how the info line fits into WIDTH columns.
Return (HEADER-COUNT COMPACT OVERFLOW-LINES).  When the full line fits
or WIDTH is nil, all segments go into the header line.  Otherwise the
timer times are shortened, the header line takes as many leading
segments as fit, and the rest is wrapped into OVERFLOW-LINES.  STATE and
NOW are as for `ardentia-dashboard--info-segments'."
  (let ((full (ardentia-dashboard--info-segments state nil now)))
    (if (or (null width)
            (<= (string-width (apply #'concat full)) width))
        (list (length full) nil nil)
      (let* ((segs (ardentia-dashboard--info-segments state t now))
             (count 1)
             (used (string-width (car segs)))
             (rest (cdr segs))
             lines line)
        (while (and rest (<= (+ used (string-width (car rest))) width))
          (setq used (+ used (string-width (pop rest))))
          (setq count (1+ count)))
        (dolist (seg rest)
          (let ((piece (if line
                           seg
                         (concat " " (string-remove-prefix " | " seg)))))
            (if (and line (> (+ (string-width line) (string-width piece))
                             width))
                (progn (push line lines)
                       (setq line (concat
                                   " " (string-remove-prefix " | " seg))))
              (setq line (if line (concat line piece) piece)))))
        (when line (push line lines))
        (list count t (nreverse lines))))))

(defvar-local ardentia-dashboard--layout-cache nil
  "Cached (HEADER-COUNT . COMPACT) for the header line, or nil.")

(defvar-local ardentia-dashboard--overflow-overlay nil
  "Overlay at the top of the buffer showing the overflow lines, or nil.")

(defun ardentia-dashboard--escape-percent (string)
  "Return STRING with every `%' doubled, keeping text properties.
The header line is a mode line construct, where `%' is special."
  (let ((out nil)
        (i 0)
        (len (length string)))
    (while (< i len)
      (let ((ch (substring string i (1+ i))))
        (push (if (equal ch "%") (concat ch ch) ch) out))
      (setq i (1+ i)))
    (apply #'concat (nreverse out))))

(defun ardentia-dashboard--header-line ()
  "Return the header line text of the current dashboard, `%' escaped.
Uses the cached layout; the segments themselves are fresh."
  (let* ((segs (ardentia-dashboard--info-segments
                nil (cdr ardentia-dashboard--layout-cache)))
         (count (car ardentia-dashboard--layout-cache)))
    (ardentia-dashboard--escape-percent
     (apply #'concat (if count (seq-take segs count) segs)))))

(defun ardentia-dashboard--width ()
  "Return the columns usable by the info line, or nil when not displayed.
This is the smallest window width over all windows showing the buffer."
  (let ((windows (get-buffer-window-list (current-buffer) nil t)))
    (when windows
      (apply #'min (mapcar (lambda (w) (1- (window-body-width w)))
                           windows)))))

(defun ardentia-dashboard--update-layout ()
  "Recompute header line cache and overflow lines of the current buffer."
  (let* ((layout (ardentia-dashboard--layout
                  (ardentia-dashboard--width)))
         (lines (nth 2 layout))
         (text (and lines (concat (string-join lines "\n") "\n"))))
    (setq ardentia-dashboard--layout-cache
          (cons (nth 0 layout) (nth 1 layout)))
    (if (not text)
        (when (overlayp ardentia-dashboard--overflow-overlay)
          (delete-overlay ardentia-dashboard--overflow-overlay)
          (setq ardentia-dashboard--overflow-overlay nil))
      (setq text (copy-sequence text))
      (add-face-text-property 0 (length text) 'header-line t text)
      (if (overlayp ardentia-dashboard--overflow-overlay)
          (move-overlay ardentia-dashboard--overflow-overlay
                        (point-min) (point-min))
        (setq ardentia-dashboard--overflow-overlay
              (make-overlay (point-min) (point-min) nil t)))
      (overlay-put ardentia-dashboard--overflow-overlay
                   'before-string text))
    (force-mode-line-update)))

(defun ardentia-dashboard--window-size-change (window)
  "Update the layout of the buffer shown in WINDOW; never signal."
  (with-demoted-errors "ardentia layout: %S"
    (when (window-live-p window)
      (with-current-buffer (window-buffer window)
        (when (derived-mode-p 'ardentia-dashboard-mode)
          (ardentia-dashboard--update-layout))))))

(defcustom ardentia-dashboard-show-later t
  "Non-nil means the dashboard lists ready tasks that are not yet due.
Those are :aiready: tasks whose SCHEDULED time still lies ahead; the
runner leaves them alone until then.  They show up as queue \"later\".
The key `l' toggles this per dashboard buffer."
  :type 'boolean
  :group 'ardentia)

(defvar ardentia-dashboard--refresh-timer nil
  "The one global timer that refreshes visible dashboards, or nil.")

(defun ardentia-dashboard--refresh-timer-restart ()
  "Cancel the auto-refresh timer; start it again when a dashboard is live."
  (when (timerp ardentia-dashboard--refresh-timer)
    (cancel-timer ardentia-dashboard--refresh-timer))
  (setq ardentia-dashboard--refresh-timer nil)
  (when (ardentia-dashboard--live-buffers)
    (ardentia-dashboard--refresh-timer-start)))

(defcustom ardentia-dashboard-refresh-interval 60
  "Seconds between automatic refreshes of the dashboards; nil or 0 disables.
Only dashboards shown in a window on a visible frame are refreshed, and
never while the minibuffer is active or a refresh is still running.  A
refresh starts an `org-tool' process over all org files (a second or
so of CPU), so nothing is spent while nobody looks.  The timer
starts when a dashboard is opened and stops once the last one is killed."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Disabled" nil))
  :set (lambda (sym val)
         (set-default sym val)
         (when (timerp ardentia-dashboard--refresh-timer)
           (ardentia-dashboard--refresh-timer-restart)))
  :group 'ardentia)

(defvar-local ardentia-dashboard--show-later 'default
  "Buffer-local override of `ardentia-dashboard-show-later'.
The symbol `default' means: use the option.")

(defun ardentia-dashboard--show-later-p ()
  "Return non-nil when this buffer shows later tasks."
  (if (eq ardentia-dashboard--show-later 'default)
      ardentia-dashboard-show-later
    ardentia-dashboard--show-later))

(defvar-local ardentia-dashboard--due-display 'default
  "Buffer-local override of `ardentia-due-display'.
The symbol `default' means use the option.")

(defun ardentia-dashboard--due-display ()
  "Return `relative' or `absolute' for this buffer."
  (if (eq ardentia-dashboard--due-display 'default)
      ardentia-due-display
    ardentia-dashboard--due-display))

(defun ardentia-dashboard--entries (tasks filter now &optional hide-later merges)
  "Return `tabulated-list-entries' for TASKS restricted to queue FILTER at NOW.
With HIDE-LATER, ready tasks that are not yet due are left out, trouble
tasks never.  MERGES, the running merge rows, are added when FILTER is nil."
  (mapcar (lambda (task)
            (list task (ardentia-dashboard--colorize-row
                        task
                        (ardentia--task-row
                         task now
                         (and (eq (plist-get task :queue) 'review)
                              (ardentia-review-for-you-class task)))
                        now)))
          (ardentia--sort-tasks
           (append
            (seq-filter (lambda (tk)
                          (and (or (null filter)
                                   (eq (plist-get tk :queue) filter))
                               (not (and hide-later
                                         (not (plist-get tk :trouble))
                                         (ardentia--task-later-p tk now)))))
                        tasks)
            (and (null filter) merges))
           now)))

(ardentia-define-keys-and-menu
    ardentia-dashboard-mode-map ardentia-dashboard-menu
  :keymap-doc "Keymap for `ardentia-dashboard-mode'."
  :menu-doc "Keys of the ardentia dashboard.
Also bound: S-<up> and S-<down> (priority up and down)."
  :groups
  (("Task at point"
    ("RET" "Open" ardentia-dashboard-enter)
    ("o" "Visit heading" ardentia-dashboard-visit)
    ("t" "Transcript" ardentia-dashboard-transcript)
    ("A" "Agent shell" ardentia-dashboard-agent-shell))
   ("Review"
    ("a" "Accept" ardentia-dashboard-accept)
    ("s" "Send back" ardentia-dashboard-send-back)
    ("l" "Toggle later" ardentia-dashboard-toggle-later))
   ("Run"
    ("x" "Run now" ardentia-dashboard-run)
    ("k" "Stop" ardentia-dashboard-stop))
   ("Priority"
    ("," "Set" ardentia-dashboard-priority)
    ("+" "Up (S-<up>)" ardentia-dashboard-priority-up)
    ("-" "Down (S-<down>)" ardentia-dashboard-priority-down))
   ("View"
    ("g" "Refresh" ardentia-dashboard-refresh)
    ("d" "Toggle due date/time left" ardentia-dashboard-toggle-due)
    ("c" "Completed" ardentia-completed)
    ("u" "Usage" ardentia-usage-refresh)
    ("q" "Quit" quit-window)))
  :alternates
  (("S-<up>" . ardentia-dashboard-priority-up)
   ("S-<down>" . ardentia-dashboard-priority-down)))

(add-hook 'ardentia-usage-updated-hook #'ardentia-dashboard--render-all)

(define-derived-mode ardentia-dashboard-mode tabulated-list-mode "Ardentia"
  "Major mode listing the agent team's queues.

\\{ardentia-dashboard-mode-map}"
  (setq tabulated-list-format
        [("Queue" 8 nil) ("Status" 10 nil) ("Host" 10 nil) ("Att" 4 nil)
         ("Age" 5 nil) ("Flags" 16 nil) ("Cost" 7 nil :right-align t)
         ("File" 18 nil) ("Title" 0 nil)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key nil)
  (setq ardentia--current-task-function #'ardentia-dashboard--task-at-point)
  ;; The info line owns the header line; the column titles go into the buffer.
  (setq tabulated-list-use-header-line nil)
  (tabulated-list-init-header)
  (setq header-line-format
        '(:eval (ardentia-dashboard--header-line)))
  (add-hook 'window-size-change-functions
            #'ardentia-dashboard--window-size-change nil t)
  (ardentia-dashboard--refresh-timer-start))

(defun ardentia-dashboard--task-at-point ()
  "Return the task plist of the row at point, or nil."
  (tabulated-list-get-id))

(defun ardentia-dashboard--row-key (id)
  "Return the stable key of the row ID, a task or merge plist, or nil.
Unlike the plist itself the key survives edits of the task, such as a
shifted line number or a changed property."
  (cond ((null id) nil)
        ((plist-get id :merge) (list :merge (plist-get id :dir)))
        (t (ardentia--task-key id))))

(defun ardentia-dashboard--mark-at (pos)
  "Return the row key, column and line number at POS of the current buffer."
  (save-excursion
    (goto-char pos)
    (list (ardentia-dashboard--row-key (tabulated-list-get-id))
          (current-column) (line-number-at-pos))))

(defun ardentia-dashboard--remember-positions ()
  "Return the marks of point and of every window showing the current buffer.
`tabulated-list-print' erases the buffer, which resets the point and
start of all its windows, so they are put back by
`ardentia-dashboard--restore-positions'.  The result is a cons of the
mark of point and a list of (WINDOW MARK SCROLL-LINES), SCROLL-LINES
being the number of lines the window start lies above the window point."
  (cons (ardentia-dashboard--mark-at (point))
        (mapcar (lambda (win)
                  (let ((pt (window-point win)))
                    ;; Count to the start of the line: `count-lines' adds one
                    ;; when PT is past it, which would scroll up every render.
                    (list win (ardentia-dashboard--mark-at pt)
                          (count-lines (window-start win)
                                       (save-excursion
                                         (goto-char pt)
                                         (line-beginning-position))))))
                (get-buffer-window-list (current-buffer) nil t))))

(defun ardentia-dashboard--goto-mark (mark)
  "Move point to the row of MARK, a list (KEY COLUMN LINE).
When the row is gone, go to the same line number, clamped to the last row."
  (pcase-let ((`(,key ,column ,line) mark))
    (goto-char (point-min))
    (unless (and key
                 (catch 'found
                   (while (not (eobp))
                     (when (equal key (ardentia-dashboard--row-key
                                       (tabulated-list-get-id)))
                       (throw 'found t))
                     (forward-line 1))))
      (goto-char (point-min))
      (forward-line (1- line))
      (when (and (eobp) (not (bobp)))
        (forward-line -1)))
    (move-to-column column)))

(defun ardentia-dashboard--restore-positions (marks)
  "Put point and the windows back as recorded in MARKS.
MARKS is the result of `ardentia-dashboard--remember-positions'."
  (save-excursion
    (dolist (entry (cdr marks))
      (pcase-let ((`(,win ,mark ,scroll) entry))
        (when (window-live-p win)
          (ardentia-dashboard--goto-mark mark)
          (set-window-point win (point))
          (forward-line (- scroll))
          (set-window-start win (point) t)))))
  (ardentia-dashboard--goto-mark (car marks)))

(defun ardentia-dashboard--render ()
  "Fill the current dashboard buffer from `ardentia--state'."
  (setq tabulated-list-entries
        (let ((ardentia--due-display-override (ardentia-dashboard--due-display)))
          (ardentia-dashboard--entries (plist-get ardentia--state :tasks)
                                       ardentia-dashboard--filter
                                       (current-time)
                                       (not (ardentia-dashboard--show-later-p))
                                       (plist-get ardentia--state :merges))))
  (let ((marks (ardentia-dashboard--remember-positions)))
    (tabulated-list-print)
    (ardentia-dashboard--update-layout)
    (ardentia-dashboard--restore-positions marks)))

(defun ardentia-dashboard--live-buffers ()
  "Return the live dashboard buffers."
  (seq-filter (lambda (b) (with-current-buffer b
                            (derived-mode-p 'ardentia-dashboard-mode)))
              (buffer-list)))

(defun ardentia-dashboard--render-all ()
  "Render all live dashboard buffers."
  (dolist (buf (ardentia-dashboard--live-buffers))
    (with-current-buffer buf (ardentia-dashboard--render))))

(defun ardentia-dashboard-refresh-all ()
  "Refresh all dashboard buffers asynchronously."
  (ardentia-refresh #'ardentia-dashboard--render-all)
  ;; Not part of the join in `ardentia-refresh': a slow network must not
  ;; delay the task list.  Throttled; re-renders through the hook.
  (ardentia-usage-maybe-refresh)
  (ardentia-dashboard--render-all))

(defun ardentia-dashboard--visible-p ()
  "Return non-nil when a dashboard buffer is shown on a visible frame."
  (seq-some (lambda (b) (get-buffer-window b 'visible))
            (ardentia-dashboard--live-buffers)))

(defun ardentia-dashboard--refresh-tick ()
  "Timer function: refresh visible dashboards; never signal an error."
  (condition-case err
      (cond
       ((not (ardentia-dashboard--live-buffers))
        (when (timerp ardentia-dashboard--refresh-timer)
          (cancel-timer ardentia-dashboard--refresh-timer))
        (setq ardentia-dashboard--refresh-timer nil))
       ((plist-get ardentia--state :refreshing))
       ((active-minibuffer-window))
       ((ardentia-dashboard--visible-p)
        (ardentia-dashboard-refresh-all)))
    (error (message "ardentia: dashboard timer: %s" (error-message-string err)))))

(defun ardentia-dashboard--refresh-timer-start ()
  "Start the auto-refresh timer unless disabled or already running."
  (when (and (integerp ardentia-dashboard-refresh-interval)
             (> ardentia-dashboard-refresh-interval 0)
             (not (timerp ardentia-dashboard--refresh-timer)))
    (setq ardentia-dashboard--refresh-timer
          (run-at-time ardentia-dashboard-refresh-interval
                       ardentia-dashboard-refresh-interval
                       #'ardentia-dashboard--refresh-tick))))

(defun ardentia-dashboard-refresh ()
  "Refresh the dashboard asynchronously; Emacs stays responsive."
  (interactive)
  (ardentia-dashboard-refresh-all))

(defun ardentia-dashboard--open (name filter)
  "Show the dashboard buffer NAME restricted to queue FILTER and refresh it."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'ardentia-dashboard-mode)
        (ardentia-dashboard-mode))
      (setq ardentia-dashboard--filter filter)
      (ardentia-dashboard--render))
    ;; The mode only starts the timer for a new buffer; a buried one, or a
    ;; buffer that predates loading this code, gets it here.
    (ardentia-dashboard--refresh-timer-start)
    (pop-to-buffer-same-window buf)
    (ardentia-dashboard-refresh-all)))

;;;###autoload
(defun ardentia-dashboard ()
  "Show the dashboard of all agent queues."
  (interactive)
  (ardentia-dashboard--open "*ardentia*" nil))

;;;###autoload
(defun ardentia-review-inbox ()
  "Show the dashboard restricted to tasks waiting for review."
  (interactive)
  (ardentia-dashboard--open "*ardentia-review*" 'review))

(defun ardentia-dashboard--require-task (&optional review)
  "Return the task at point; with REVIEW require the review queue.
The row of a running merge is not a task and signals a `user-error'."
  (let ((task (or (ardentia-dashboard--task-at-point) (user-error "No task here"))))
    (when (plist-get task :merge)
      (user-error "Not a task: a running merge"))
    (when (and review (not (eq (plist-get task :queue) 'review)))
      (user-error "Task is not waiting for review"))
    task))

(defvar org-priority-highest)
(defvar org-priority-lowest)

(defun ardentia-dashboard--require-ready-task ()
  "Return the task at point; require the ready queue (later included)."
  (let ((task (ardentia-dashboard--require-task)))
    (unless (eq (plist-get task :queue) 'ready)
      (user-error "Priority can only be changed on ready or later tasks"))
    task))

(defun ardentia-dashboard--set-priority (action)
  "Apply priority ACTION to the ready task at point.
The asynchronous refresh re-renders the dashboard afterwards."
  (let ((task (ardentia-dashboard--require-ready-task)))
    (ardentia-set-priority-1 (plist-get task :file) (plist-get task :id)
                             action (plist-get task :line)
                             (plist-get task :title))))

(defun ardentia-dashboard-priority ()
  "Set the priority of the ready or later task at point, like org-agenda.
Prompt for a letter; SPC removes the cookie."
  (interactive)
  (ardentia-dashboard--require-ready-task)
  (let ((c (read-char-exclusive
            (format "Priority %c-%c, SPC to remove: "
                    org-priority-highest org-priority-lowest))))
    (cond ((eq c ?\s) (ardentia-dashboard--set-priority 'remove))
          ((and (>= (upcase c) org-priority-highest)
                (<= (upcase c) org-priority-lowest))
           (ardentia-dashboard--set-priority (upcase c)))
          (t (user-error "Priority must be between `%c' and `%c'"
                         org-priority-highest org-priority-lowest)))))

(defun ardentia-dashboard-priority-up ()
  "Raise the priority of the ready or later task at point."
  (interactive)
  (ardentia-dashboard--set-priority 'up))

(defun ardentia-dashboard-priority-down ()
  "Lower the priority of the ready or later task at point."
  (interactive)
  (ardentia-dashboard--set-priority 'down))

(defun ardentia-dashboard-review ()
  "Open the review buffer of the task at point."
  (interactive)
  (ardentia-review-task (ardentia-dashboard--require-task t)))

;;;###autoload
(defun ardentia-dashboard-enter ()
  "Open the task at point: review buffer, or session viewer when running.
Other queues signal a `user-error'; `t' shows their last transcript.
On a running merge open its directory with dired."
  (interactive)
  (if (plist-get (ardentia-dashboard--task-at-point) :merge)
      (ardentia-dashboard--open-merge)
    (ardentia-dashboard--enter-task)))

(defun ardentia-dashboard--open-merge ()
  "Open the directory of the running merge at point with dired."
  (dired (plist-get (ardentia-dashboard--task-at-point) :dir)))

(defun ardentia-dashboard--enter-task ()
  "Open the task at point, see `ardentia-dashboard-enter'."
  (let ((task (ardentia-dashboard--require-task)))
    (pcase (plist-get task :queue)
      ('review (ardentia-review-task task))
      ('running (ardentia-session-open-viewer task))
      (_ (user-error
          "Task is neither waiting for review nor running (t: last transcript)")))))

(defun ardentia-dashboard-transcript ()
  "Open the latest session transcript of the task at point, in any queue."
  (interactive)
  (ardentia-session-open (ardentia-dashboard--require-task)))

(defun ardentia-dashboard-agent-shell ()
  "Open the latest session of the task at point.
A running task whose session accepts live input opens in the current
window, ready for sending messages into it; anything else is resumed
in agent-shell."
  (interactive)
  (let ((task (ardentia-dashboard--require-task)))
    (if (eq (plist-get task :queue) 'running)
        (ardentia-session-attach task)
      (ardentia-session-open-agent-shell task))))

(defun ardentia-dashboard-toggle-later ()
  "Hide or show ready tasks that are not yet due in this dashboard buffer."
  (interactive)
  (setq ardentia-dashboard--show-later
        (not (ardentia-dashboard--show-later-p)))
  (ardentia-dashboard--render)
  (message "Later tasks: %s"
           (if (ardentia-dashboard--show-later-p) "shown" "hidden")))

(defun ardentia-dashboard-toggle-due ()
  "Switch the due flag of later tasks in this buffer between time left and date."
  (interactive)
  (setq ardentia-dashboard--due-display
        (if (eq (ardentia-dashboard--due-display) 'relative)
            'absolute
          'relative))
  (ardentia-dashboard--render)
  (message "Due flag: %s"
           (if (eq ardentia-dashboard--due-display 'relative)
               "time until due" "due date/time")))

(defun ardentia-dashboard-visit ()
  "Visit the heading of the task at point in its org file.
On a running merge open its directory with dired."
  (interactive)
  (if (plist-get (ardentia-dashboard--task-at-point) :merge)
      (ardentia-dashboard--open-merge)
    (ardentia-visit-task (ardentia-dashboard--require-task))))

(defun ardentia-dashboard-accept ()
  "Accept the review task at point."
  (interactive)
  (ardentia-accept (ardentia-dashboard--require-task t)))

(defun ardentia-dashboard-send-back ()
  "Send the review task at point back to the agents."
  (interactive)
  (ardentia-send-back (ardentia-dashboard--require-task t)))

(defun ardentia-dashboard-run ()
  "Run the ready (or later) task at point now."
  (interactive)
  (ardentia-run-task (ardentia-dashboard--require-task)))

(defun ardentia-dashboard-stop ()
  "Stop the running task at point."
  (interactive)
  (ardentia-stop-task (ardentia-dashboard--require-task)))

(provide 'ardentia-dashboard)
;;; ardentia-dashboard.el ends here
