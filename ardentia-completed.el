;;; ardentia-completed.el --- Overview of completed agent tasks -*- lexical-binding: t -*-

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

;; `ardentia-completed' lists every task the agents handed back, accepted
;; or not: all headings tagged :humanreview:, open (still waiting for
;; review) and done (DONE or CANC; accepting keeps the tag).  The columns
;; show when the work finished, its status, the host, and the tokens and
;; cost the runner recorded; the header line sums them up.  The fetch is
;; asynchronous.  The functions in the "Pure parts" section have no side
;; effects and no processes, so they can be tested directly.

;;; Code:

(require 'tabulated-list)
(require 'seq)
(require 'subr-x)
(require 'ardentia-core)
(require 'ardentia-actions)
(require 'ardentia-review)
(require 'ardentia-session)

(declare-function ardentia "ardentia" ())

(defcustom ardentia-completed-period 'all
  "Period the completed view starts with, by the finish time AI_SESSION.
The key `p' cycles through the periods in the buffer."
  :type '(choice (const :tag "All" all)
                 (const :tag "Last 30 days" last-30-days)
                 (const :tag "Last 7 days" last-7-days)
                 (const :tag "Today" today))
  :group 'ardentia)

;;;; Pure parts

(defconst ardentia-completed--periods '(all last-30-days last-7-days today)
  "The periods `ardentia-completed-cycle-period' goes through, in order.")

(defun ardentia-completed--period-label (period)
  "Return the human readable name of PERIOD."
  (pcase period
    ('last-30-days "last 30 days")
    ('last-7-days "last 7 days")
    ('today "today")
    (_ "all")))

(defun ardentia-completed--next-period (period)
  "Return the period after PERIOD in `ardentia-completed--periods'."
  (let ((rest (cdr (memq period ardentia-completed--periods))))
    (if rest (car rest) (car ardentia-completed--periods))))

(defun ardentia-completed--period-start (period now)
  "Return the earliest finish time inside PERIOD at time NOW, or nil for all."
  (pcase period
    ('last-30-days (time-subtract now (* 30 86400)))
    ('last-7-days (time-subtract now (* 7 86400)))
    ('today (let ((d (decode-time now)))
              (encode-time (list 0 0 0 (decoded-time-day d)
                                 (decoded-time-month d) (decoded-time-year d)
                                 nil -1 nil))))
    (_ nil)))

(defun ardentia-completed--finished (task)
  "Return the time TASK finished (AI_SESSION) as a time value, or nil."
  (ardentia--parse-stamp (ardentia--task-prop task "AI_SESSION")))

(defun ardentia-completed--format-finished (task)
  "Return the finish time of TASK as \"YYYY-MM-DD HH:MM\", or \"\"."
  (let ((time (ardentia-completed--finished task)))
    (if time (format-time-string "%Y-%m-%d %H:%M" time) "")))

(defun ardentia-completed--in-period-p (task period now)
  "Return non-nil when TASK finished inside PERIOD at time NOW.
Every task is inside the period `all'; for the others a task without a
usable AI_SESSION is outside.  The start of the period is inside."
  (let ((start (ardentia-completed--period-start period now)))
    (or (null start)
        (let ((time (ardentia-completed--finished task)))
          (and time (not (time-less-p time start)))))))

(defun ardentia-completed--select (tasks period only-open now)
  "Return the TASKS inside PERIOD at time NOW.
With ONLY-OPEN, leave out the tasks that are done (already accepted)."
  (seq-filter (lambda (task)
                (and (ardentia-completed--in-period-p task period now)
                     (not (and only-open (plist-get task :done)))))
              tasks))

(defun ardentia-completed--sort (tasks)
  "Return TASKS, newest finish time first; tasks without one come last."
  (let ((keyed (mapcar (lambda (task)
                         (cons task (ardentia-completed--finished task)))
                       tasks)))
    (mapcar #'car
            (sort keyed
                  (lambda (a b)
                    (if (and (cdr a) (cdr b))
                        (time-less-p (cdr b) (cdr a))
                      (and (cdr a) t)))))))

(defun ardentia-completed--totals (tasks)
  "Return a plist with the totals over TASKS.
Keys: :count, :completed, :blocked, :partial (by AI_STATUS), :tokens and
:cost (sums over the tasks that have them) and :with-usage, the number of
tasks that carry any usage data."
  (let ((count 0) (completed 0) (blocked 0) (partial 0)
        (tokens 0) (cost 0) (with-usage 0))
    (dolist (task tasks)
      (setq count (1+ count))
      (pcase (ardentia--task-prop task "AI_STATUS")
        ("completed" (setq completed (1+ completed)))
        ("blocked" (setq blocked (1+ blocked)))
        ("partial" (setq partial (1+ partial))))
      (let ((tk (ardentia--task-tokens-total task))
            (co (ardentia--task-cost-total task)))
        (when (or tk co) (setq with-usage (1+ with-usage)))
        (when tk (setq tokens (+ tokens tk)))
        (when co (setq cost (+ cost co)))))
    (list :count count :completed completed :blocked blocked :partial partial
          :tokens tokens :cost cost :with-usage with-usage)))

(defun ardentia-completed--row (task)
  "Return the row vector for TASK: finished, state, status, host, runs,
tokens, cost, file, title.  Missing values give empty cells, never 0."
  (let ((runs (ardentia--parse-number (ardentia--task-prop task "AI_USAGE_RUNS"))))
    (vector (ardentia-completed--format-finished task)
            (or (ardentia--stringify (plist-get task :todo)) "")
            (or (ardentia--task-prop task "AI_STATUS") "")
            (or (ardentia--task-prop task "AI_CLAIMED_BY") "")
            (if runs (format "%d" (round runs)) "")
            (ardentia--format-tokens (ardentia--task-tokens-total task))
            (ardentia--format-cost (ardentia--task-cost-total task))
            (file-name-nondirectory (or (plist-get task :file) ""))
            (or (plist-get task :title) ""))))

(defun ardentia-completed--entries (tasks period only-open now)
  "Return `tabulated-list-entries' for TASKS inside PERIOD at time NOW.
ONLY-OPEN as in `ardentia-completed--select'.  Newest finish time first."
  (mapcar (lambda (task) (list task (ardentia-completed--row task)))
          (ardentia-completed--sort
           (ardentia-completed--select tasks period only-open now))))

(defun ardentia-completed--summary (shown)
  "Return the header line text for the SHOWN tasks: counts, tokens, cost."
  (let* ((tot (ardentia-completed--totals shown))
         (with (plist-get tot :with-usage)))
    (concat
     (format " %d task%s (%d completed, %d blocked, %d partial)"
             (plist-get tot :count) (if (= (plist-get tot :count) 1) "" "s")
             (plist-get tot :completed) (plist-get tot :blocked)
             (plist-get tot :partial))
     (if (= with 0)
         " | no usage data"
       (format " | %s tokens, %s (%d with usage data)"
               (let ((s (ardentia--format-tokens (plist-get tot :tokens))))
                 (if (string-empty-p s) "0" s))
               (ardentia--format-cost (plist-get tot :cost))
               with)))))

(defun ardentia-completed--info-line (tasks period only-open time error
                                            refreshing now)
  "Return the header line text.
TASKS are all fetched tasks, shown are those inside PERIOD (and, with
ONLY-OPEN, not done) at time NOW.  TIME is the last refresh, ERROR the
error text or nil, REFRESHING non-nil while a fetch runs."
  (concat
   (ardentia-completed--summary
    (ardentia-completed--select tasks period only-open now))
   (format " | Period: %s" (ardentia-completed--period-label period))
   (if only-open " | only waiting for review" "")
   (format " | Refreshed: %s" (if time (format-time-string "%H:%M:%S" time) "never"))
   (if error (format " | ERROR: %s" error) "")
   (if refreshing " | refreshing..." "")))

;;;; Fetch

(defun ardentia-completed--fetch (callback)
  "Fetch all :humanreview: tasks, done ones included, asynchronously.
CALLBACK receives the task list and an error string or nil."
  (let ((files (ardentia--org-files)))
    (if (null files)
        (funcall callback nil "no org files found")
      (ardentia--run-async
       ardentia-org-tool (append (list "find-tagged" "humanreview") files)
       (lambda (status out er)
         (if (not (eql status 0))
             (funcall callback nil
                      (format "org-tool humanreview failed: %s"
                              (string-trim (if (string-blank-p er) out er))))
           (pcase (condition-case e
                      (ardentia--dedupe
                       (ardentia--tasks-from-json (ardentia--parse-json out) t))
                    (error (error-message-string e)))
             ((and (pred stringp) msg)
              (funcall callback nil
                       (format "org-tool humanreview: bad JSON: %s" msg)))
             (tasks (funcall callback tasks nil)))))))))

;;;; Mode

(defvar-local ardentia-completed--tasks nil
  "All tasks fetched by the last refresh of this buffer.")

(defvar-local ardentia-completed--time nil
  "Time of the last successful or failed refresh of this buffer, or nil.")

(defvar-local ardentia-completed--error nil
  "Error text of the last refresh of this buffer, or nil.")

(defvar-local ardentia-completed--refreshing nil
  "Non-nil while this buffer waits for a fetch.")

(defvar-local ardentia-completed--period nil
  "Period filter of this buffer; initialised from `ardentia-completed-period'.")

(defvar-local ardentia-completed--only-open nil
  "Non-nil when this buffer shows only tasks still waiting for review.")

(defun ardentia-completed--number-less (getter)
  "Return a `tabulated-list' sort predicate comparing GETTER of the tasks.
GETTER maps a task to a number or nil; nil counts as smaller than any."
  (lambda (a b)
    (< (or (funcall getter (car a)) -1) (or (funcall getter (car b)) -1))))

(defun ardentia-completed--runs (task)
  "Return the number of runs of TASK (AI_USAGE_RUNS), or nil."
  (ardentia--parse-number (ardentia--task-prop task "AI_USAGE_RUNS")))

(ardentia-define-keys-and-menu
    ardentia-completed-mode-map ardentia-completed-menu
  :keymap-doc "Keymap for `ardentia-completed-mode'."
  :menu-doc "Keys of the ardentia completed-tasks list."
  :groups
  (("Task at point"
    ("RET" "Review" ardentia-completed-review)
    ("t" "Transcript" ardentia-completed-transcript)
    ("o" "Visit heading" ardentia-completed-visit))
   ("Filter"
    ("p" "Cycle period" ardentia-completed-cycle-period)
    ("r" "Toggle only open" ardentia-completed-toggle-open))
   ("View"
    ("g" "Refresh" ardentia-completed-refresh)
    ("q" "Quit" quit-window))))

(define-derived-mode ardentia-completed-mode tabulated-list-mode
  "Ardentia-Completed"
  "Major mode listing the tasks the agents completed, with tokens and cost.

\\{ardentia-completed-mode-map}"
  (setq tabulated-list-format
        (vector '("Finished" 16 t) '("State" 6 t) '("Status" 10 t)
                '("Host" 10 t)
                (list "Runs" 4 (ardentia-completed--number-less
                                #'ardentia-completed--runs)
                      :right-align t)
                (list "Tokens" 7 (ardentia-completed--number-less
                                  #'ardentia--task-tokens-total)
                      :right-align t)
                (list "Cost" 8 (ardentia-completed--number-less
                                #'ardentia--task-cost-total)
                      :right-align t)
                '("File" 18 t) '("Title" 0 t)))
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key nil)
  (setq ardentia-completed--period ardentia-completed-period)
  (setq ardentia--current-task-function #'ardentia-completed--task-at-point)
  ;; The info line owns the header line; the column titles go into the buffer.
  (setq tabulated-list-use-header-line nil)
  (tabulated-list-init-header)
  (setq header-line-format '(:eval (ardentia-completed--header))))

(defun ardentia-completed--header ()
  "Return the header line of the current completed buffer."
  (ardentia-completed--info-line
   ardentia-completed--tasks ardentia-completed--period
   ardentia-completed--only-open ardentia-completed--time
   ardentia-completed--error ardentia-completed--refreshing (current-time)))

(defun ardentia-completed--task-at-point ()
  "Return the task plist of the row at point, or nil."
  (tabulated-list-get-id))

(defun ardentia-completed--render ()
  "Fill the current buffer from the fetched tasks."
  (setq tabulated-list-entries
        (ardentia-completed--entries ardentia-completed--tasks
                                     ardentia-completed--period
                                     ardentia-completed--only-open
                                     (current-time)))
  (tabulated-list-print t)
  (force-mode-line-update))

(defun ardentia-completed-refresh ()
  "Fetch the completed tasks asynchronously; Emacs stays responsive."
  (interactive)
  (unless (derived-mode-p 'ardentia-completed-mode)
    (user-error "Not an ardentia completed buffer"))
  (unless ardentia-completed--refreshing
    (let ((buf (current-buffer)))
      (setq ardentia-completed--refreshing t)
      (ardentia-completed--fetch
       (lambda (tasks err)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (setq ardentia-completed--refreshing nil
                   ardentia-completed--time (current-time)
                   ardentia-completed--error err)
             ;; Keep the old list when the fetch failed.
             (unless err (setq ardentia-completed--tasks tasks))
             (ardentia-completed--render))))))
    (force-mode-line-update)))

;;;###autoload
(defun ardentia-completed ()
  "Show the overview of completed tasks, with tokens and cost."
  (interactive)
  (let ((buf (get-buffer-create "*ardentia-completed*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'ardentia-completed-mode)
        (ardentia-completed-mode))
      (ardentia-completed--render)
      (ardentia-completed-refresh))
    (pop-to-buffer-same-window buf)))

(defun ardentia-completed-cycle-period ()
  "Cycle the period filter: all, last 30 days, last 7 days, today."
  (interactive)
  (setq ardentia-completed--period
        (ardentia-completed--next-period ardentia-completed--period))
  (ardentia-completed--render)
  (message "Period: %s"
           (ardentia-completed--period-label ardentia-completed--period)))

(defun ardentia-completed-toggle-open ()
  "Toggle between all tasks and only those still waiting for review."
  (interactive)
  (setq ardentia-completed--only-open (not ardentia-completed--only-open))
  (ardentia-completed--render)
  (message "Showing %s"
           (if ardentia-completed--only-open
               "only tasks waiting for review" "all completed tasks")))

(defun ardentia-completed--require-task ()
  "Return the task at point or signal a `user-error'."
  (or (ardentia-completed--task-at-point) (user-error "No task here")))

(defun ardentia-completed-review ()
  "Open the review buffer of the task at point, accepted or not."
  (interactive)
  (ardentia-review-task (ardentia-completed--require-task)))

(defun ardentia-completed-transcript ()
  "Open the session transcript of the task at point."
  (interactive)
  (ardentia-session-open (ardentia-completed--require-task)))

(defun ardentia-completed-visit ()
  "Visit the heading of the task at point in its org file."
  (interactive)
  (ardentia-visit-task (ardentia-completed--require-task)))

(provide 'ardentia-completed)
;;; ardentia-completed.el ends here
