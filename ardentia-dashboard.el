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

;; `ardentia-dashboard' lists the agent queues (review, running, ready, later)
;; in a `tabulated-list-mode' buffer.  `ardentia-review-inbox' is the same
;; view filtered to the review queue.  Refreshing is asynchronous.
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
          (list (format " | ERROR: %s" (plist-get st :error))))
     (and (plist-get st :refreshing) (list " | refreshing..."))
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

(defun ardentia-dashboard--entries (tasks filter now &optional hide-later)
  "Return `tabulated-list-entries' for TASKS restricted to queue FILTER at NOW.
With HIDE-LATER, ready tasks that are not yet due are left out."
  (mapcar (lambda (task) (list task (ardentia--task-row task now)))
          (ardentia--sort-tasks
           (seq-filter (lambda (tk)
                         (and (or (null filter)
                                  (eq (plist-get tk :queue) filter))
                              (not (and hide-later
                                        (ardentia--task-later-p tk now)))))
                       tasks)
           now)))

(defvar-keymap ardentia-dashboard-mode-map
  :doc "Keymap for `ardentia-dashboard-mode'."
  "g" #'ardentia-dashboard-refresh
  "RET" #'ardentia-dashboard-enter
  "t" #'ardentia-dashboard-transcript
  "A" #'ardentia-dashboard-agent-shell
  "o" #'ardentia-dashboard-visit
  "a" #'ardentia-dashboard-accept
  "s" #'ardentia-dashboard-send-back
  "l" #'ardentia-dashboard-toggle-later
  "x" #'ardentia-dashboard-run
  "," #'ardentia-dashboard-priority
  "+" #'ardentia-dashboard-priority-up
  "S-<up>" #'ardentia-dashboard-priority-up
  "-" #'ardentia-dashboard-priority-down
  "S-<down>" #'ardentia-dashboard-priority-down
  "q" #'quit-window
  "c" #'ardentia-completed
  "u" #'ardentia-usage-refresh
  "?" #'ardentia)

(add-hook 'ardentia-usage-updated-hook #'ardentia-dashboard--render-all)

(define-derived-mode ardentia-dashboard-mode tabulated-list-mode "Ardentia"
  "Major mode listing the agent team's queues.

\\{ardentia-dashboard-mode-map}"
  (setq tabulated-list-format
        [("Queue" 8 nil) ("Status" 10 nil) ("Host" 10 nil) ("Att" 4 nil)
         ("Age" 5 nil) ("Flags" 12 nil) ("Cost" 7 nil :right-align t)
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

(defun ardentia-dashboard--render ()
  "Fill the current dashboard buffer from `ardentia--state'."
  (setq tabulated-list-entries
        (ardentia-dashboard--entries (plist-get ardentia--state :tasks)
                                     ardentia-dashboard--filter
                                     (current-time)
                                     (not (ardentia-dashboard--show-later-p))))
  (tabulated-list-print t)
  (ardentia-dashboard--update-layout))

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
  "Return the task at point; with REVIEW require the review queue."
  (let ((task (or (ardentia-dashboard--task-at-point) (user-error "No task here"))))
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
Other queues signal a `user-error'; `t' shows their last transcript."
  (interactive)
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

(defun ardentia-dashboard-visit ()
  "Visit the heading of the task at point in its org file."
  (interactive)
  (ardentia-visit-task (ardentia-dashboard--require-task)))

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

(provide 'ardentia-dashboard)
;;; ardentia-dashboard.el ends here
