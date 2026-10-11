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

;; The completed view of the task list (see `ardentia-dashboard') lists every
;; task the agents handed back, accepted or not: all headings tagged
;; :humanreview:, open (still waiting for review) and done (DONE or CANC;
;; accepting keeps the tag).  The columns show when the work finished, its
;; status, the host, and the tokens and cost the runner recorded; the header
;; line sums them up.  This file holds what is specific to that data: the
;; period, state, sort and totals helpers, the row, the column format and
;; the asynchronous fetch.  The buffer, the keys and the filters are in
;; `ardentia-dashboard'.  The functions in the "Pure parts" section have no
;; side effects and no processes, so they can be tested directly.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'ardentia-core)

(defcustom ardentia-completed-period 'all
  "Period the completed view starts with, by the finish time AI_SESSION.
The key `P' cycles through the periods in the task list."
  :type '(choice (const :tag "All" all)
                 (const :tag "Last 30 days" last-30-days)
                 (const :tag "Last 7 days" last-7-days)
                 (const :tag "Today" today))
  :group 'ardentia)

;;;; Pure parts

(defconst ardentia-completed--periods '(all last-30-days last-7-days today)
  "The periods `ardentia-dashboard-cycle-period' goes through, in order.")

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

(defconst ardentia-completed--states '(all review accepted)
  "The state filters `ardentia-dashboard-cycle-state' goes through, in order.")

(defun ardentia-completed--state-label (state)
  "Return the human readable name of the state filter STATE."
  (pcase state
    ('review "waiting for review")
    ('accepted "accepted")
    (_ "all")))

(defun ardentia-completed--next-state (state)
  "Return the state filter after STATE in `ardentia-completed--states'."
  (let ((rest (cdr (memq state ardentia-completed--states))))
    (if rest (car rest) (car ardentia-completed--states))))

(defun ardentia-completed--in-state-p (task state)
  "Return non-nil when TASK passes the state filter STATE.
`review' lets the open tasks pass, `accepted' the done ones, anything
else (`all') every task."
  (pcase state
    ('review (not (plist-get task :done)))
    ('accepted (and (plist-get task :done) t))
    (_ t)))

(defun ardentia-completed--select (tasks period state now)
  "Return the TASKS inside PERIOD at time NOW that pass the STATE filter.
STATE is `all', `review' (only open tasks) or `accepted' (only done ones)."
  (seq-filter (lambda (task)
                (and (ardentia-completed--in-period-p task period now)
                     (ardentia-completed--in-state-p task state)))
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

(defun ardentia-completed--entries (tasks period state now)
  "Return `tabulated-list-entries' for TASKS inside PERIOD at time NOW.
STATE as in `ardentia-completed--select'.  Newest finish time first."
  (mapcar (lambda (task) (list task (ardentia-completed--row task)))
          (ardentia-completed--sort
           (ardentia-completed--select tasks period state now))))

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

;;;; View support

(defun ardentia-completed--number-less (getter)
  "Return a `tabulated-list' sort predicate comparing GETTER of the tasks.
GETTER maps a task to a number or nil; nil counts as smaller than any."
  (lambda (a b)
    (< (or (funcall getter (car a)) -1) (or (funcall getter (car b)) -1))))

(defun ardentia-completed--runs (task)
  "Return the number of runs of TASK (AI_USAGE_RUNS), or nil."
  (ardentia--parse-number (ardentia--task-prop task "AI_USAGE_RUNS")))

(defconst ardentia-completed--format
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
          '("File" 18 t) '("Title" 0 t))
  "The `tabulated-list-format' of the completed view of the task list.
Matches the rows of `ardentia-completed--row'.  Numbers sort numerically.
Treat it as read-only; buffers get a copy.")

(declare-function ardentia-dashboard--open "ardentia-dashboard" (view &optional name))

;;;###autoload
(defun ardentia-completed ()
  "Show the task list in its completed view, with tokens and cost.
This is the one `*ardentia*' buffer switched to the completed view."
  (interactive)
  (require 'ardentia-dashboard)
  (ardentia-dashboard--open 'completed))

(provide 'ardentia-completed)
;;; ardentia-completed.el ends here
