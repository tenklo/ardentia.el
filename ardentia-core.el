;;; ardentia-core.el --- Core of ardentia: data, parsing, async fetch -*- lexical-binding: t -*-

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

;; Shared layer of ardentia, a view and command layer for the org-mode
;; agent team (tasks tagged :aiready: -> :ai_in_progress: -> :humanreview:).
;;
;; Reading task state goes through the CLI `org-tool' (out of process,
;; asynchronous).  The functions in the "Pure parts" section have no side
;; effects and no processes, so they can be tested directly.

;;; Code:

(require 'json)
(require 'seq)
(require 'subr-x)

(defgroup ardentia nil
  "View and command layer for an Org agent team."
  :group 'tools
  :prefix "ardentia-")

(defcustom ardentia-org-directory "~/org/"
  "Directory with the org files that are scanned for agent tasks."
  :type 'directory)

(defcustom ardentia-org-tool
  (expand-file-name "ai/tools/org-tool" ardentia-org-directory)
  "Path of the `org-tool' command line program."
  :type 'file)

(defcustom ardentia-host-command
  (expand-file-name "ai/tools/aiready-host" ardentia-org-directory)
  "Program that prints the name of the local agent host."
  :type 'file)

(defcustom ardentia-hosts-file
  (expand-file-name "ai/aiready-hosts.conf" ardentia-org-directory)
  "Configuration file listing the agent hosts."
  :type 'file)

(defcustom ardentia-timer-unit "process-aiready.timer"
  "Name of the systemd user timer that runs the agents."
  :type 'string)

(defcustom ardentia-service-unit "process-aiready.service"
  "Name of the systemd user service the timer starts."
  :type 'string)

(defcustom ardentia-process-timeout 60
  "Seconds after which an external process of ardentia is killed."
  :type 'integer)

(defcustom ardentia-stale-minutes 90
  "Minutes after which a claimed (running) task is flagged as stale."
  :type 'integer)

(defcustom ardentia-default-priority ?B
  "Priority a task without a cookie counts as when sorting.
Keep it equal to the default priority of the runner (B)."
  :type 'character)

(defconst ardentia--queue-tags
  '(("humanreview" . review)
    ("ai_in_progress" . running)
    ("aiready" . ready))
  "Alist of org tag and queue symbol, most advanced queue first.")

;;;; Pure parts

(defun ardentia--val (value)
  "Return VALUE, or nil when it is nil, the string \"null\" or empty."
  (cond ((null value) nil)
        ((and (stringp value) (member value '("null" ""))) nil)
        (t value)))

(defun ardentia--parse-json (string)
  "Parse the JSON array STRING of `org-tool' into a list of alists.
Signal an error when STRING is not valid JSON."
  (if (string-blank-p string)
      nil
    (json-parse-string string :object-type 'alist :array-type 'list
                       :null-object nil :false-object nil)))

(defun ardentia--tag-queue (tags)
  "Return the queue symbol for the org tag list TAGS, or nil."
  (let (result)
    (dolist (pair ardentia--queue-tags)
      (when (and (not result) (member (car pair) tags))
        (setq result (cdr pair))))
    result))

(defun ardentia--stringify (value)
  "Return VALUE as a string, or nil when it is absent."
  (let ((v (ardentia--val value)))
    (cond ((null v) nil)
          ((stringp v) v)
          (t (format "%s" v)))))

(defun ardentia--task-from-json (alist &optional keep-done)
  "Convert one decoded `org-tool' element ALIST into a task plist.
Return nil when the element is done, unless KEEP-DONE is non-nil.  The
plist then also carries `:done', non-nil for a done element."
  (let* ((done (alist-get 'done alist))
         (tags (mapcar #'ardentia--stringify (ardentia--val (alist-get 'tags alist))))
         (props (mapcar (lambda (cell)
                          (cons (symbol-name (car cell))
                                (ardentia--stringify (cdr cell))))
                        (ardentia--val (alist-get 'properties alist)))))
    (unless (and done (not (eq done :false)) (not keep-done))
      (list :done (and done (not (eq done :false)) t)
            :file (alist-get 'file alist)
            :line (alist-get 'line alist)
            :level (alist-get 'level alist)
            :todo (ardentia--stringify (alist-get 'todo alist))
            :title (or (ardentia--stringify (alist-get 'title alist)) "")
            :priority (ardentia--stringify (alist-get 'priority alist))
            :tags tags
            :id (ardentia--stringify (alist-get 'id alist))
            :scheduled (ardentia--stringify (alist-get 'scheduled alist))
            :deadline (ardentia--stringify (alist-get 'deadline alist))
            :created (ardentia--stringify (alist-get 'created alist))
            :props props
            :queue (ardentia--tag-queue tags)))))

(defun ardentia--tasks-from-json (list &optional keep-done)
  "Convert the decoded `org-tool' LIST into task plists.
Done tasks are dropped unless KEEP-DONE is non-nil."
  (delq nil (mapcar (lambda (alist) (ardentia--task-from-json alist keep-done))
                    list)))

(defun ardentia--task-key (task)
  "Return the identity of TASK for deduplication."
  (list (plist-get task :file)
        (or (plist-get task :id) (plist-get task :line))))

(defun ardentia--dedupe (tasks)
  "Remove from TASKS the entries with an already seen file and id."
  (let ((seen (make-hash-table :test 'equal)) result)
    (dolist (task tasks)
      (let ((key (ardentia--task-key task)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push task result))))
    (nreverse result)))

(defun ardentia--task-prop (task name)
  "Return the property NAME (a string) of TASK, or nil."
  (ardentia--val (cdr (assoc name (plist-get task :props)))))

(defun ardentia--parse-stamp (string)
  "Parse STRING (\"YYYYMMDD-HHMM\" or \"YYYY-MM-DD HH:MM\") into a time value.
Return nil when STRING is absent or malformed."
  (let ((s (ardentia--val string)))
    (when (stringp s)
      (when (or (string-match
                 "\\`\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\'" s)
                (string-match
                 "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)[ T]\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)" s))
        (let ((n (lambda (i) (string-to-number (match-string i s)))))
          (encode-time (list 0 (funcall n 5) (funcall n 4) (funcall n 3)
                             (funcall n 2) (funcall n 1) nil -1 nil)))))))

;;;; Pure parts: usage (tokens and cost)

(defun ardentia--parse-number (value)
  "Return VALUE as a non-negative number, or nil.
VALUE may be a number or a string like \"2.19\" or \"5400000\"; nil, the
string \"null\", the empty string and any garbage give nil."
  (let ((v (ardentia--val value)))
    (cond ((and (numberp v) (>= v 0)) v)
          ((and (stringp v) (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" v))
           (string-to-number v))
          (t nil))))

(defun ardentia--format-tokens (value)
  "Format the token count VALUE compactly: 950, 12.3k, 5.4M.
Nil, \"null\" and garbage give the empty string, never \"0\"."
  (let ((n (ardentia--parse-number value)))
    (cond ((null n) "")
          ((< n 1000) (format "%d" (round n)))
          ((< n 999950) (format "%.1fk" (/ n 1000.0)))
          (t (format "%.1fM" (/ n 1000000.0))))))

(defun ardentia--format-cost (value)
  "Format the cost VALUE in US dollars as \"$4.09\".
Nil, \"null\" and garbage give the empty string, never \"$0.00\"."
  (let ((n (ardentia--parse-number value)))
    (if n (format "$%.2f" n) "")))

(defun ardentia--task-tokens-total (task)
  "Return the tokens TASK used over all runs (AI_TOKENS_TOTAL), or nil."
  (ardentia--parse-number (ardentia--task-prop task "AI_TOKENS_TOTAL")))

(defun ardentia--task-cost-total (task)
  "Return the cost in USD of TASK over all runs (AI_COST_USD_TOTAL), or nil."
  (ardentia--parse-number (ardentia--task-prop task "AI_COST_USD_TOTAL")))

(defun ardentia--usage-line (props)
  "Return a readable usage summary from PROPS, an alist of strings, or nil.
Example: \"5.4M tokens, $4.09 over 2 runs (last run 3.0M tokens, $2.19)\".
Return nil when none of the usage properties holds a usable value."
  (let* ((get (lambda (name) (ardentia--parse-number
                              (ardentia--val (cdr (assoc name props))))))
         (tokens (funcall get "AI_TOKENS_TOTAL"))
         (cost (funcall get "AI_COST_USD_TOTAL"))
         (runs (funcall get "AI_USAGE_RUNS"))
         (last-tokens (funcall get "AI_TOKENS"))
         (last-cost (funcall get "AI_COST_USD"))
         (amounts (lambda (tk co)
                    (string-join
                     (delq nil (list (and tk (concat (ardentia--format-tokens tk)
                                                     " tokens"))
                                     (and co (ardentia--format-cost co))))
                     ", ")))
         (total (funcall amounts tokens cost))
         (last (funcall amounts last-tokens last-cost))
         (runs-text (and runs (format "%d run%s" (round runs)
                                      (if (= (round runs) 1) "" "s"))))
         (main (string-join
                (delq nil (list (and (not (string-empty-p total)) total)
                                (and runs-text
                                     (if (string-empty-p total)
                                         runs-text
                                       (concat "over " runs-text)))))
                " ")))
    (cond ((and (string-empty-p main) (string-empty-p last)) nil)
          ((string-empty-p last) main)
          ((string-empty-p main) (format "last run %s" last))
          (t (format "%s (last run %s)" main last)))))

(defun ardentia--format-age (seconds)
  "Format the age SECONDS as \"12m\", \"3h\" or \"2d\".  Nil gives \"\"."
  (cond ((null seconds) "")
        (t (let ((s (max 0 seconds)))
             (cond ((< s 3600) (format "%dm" (floor s 60)))
                   ((< s 86400) (format "%dh" (floor s 3600)))
                   (t (format "%dd" (floor s 86400))))))))

(defun ardentia--task-stamp (task)
  "Return the time value that defines the age of TASK, or nil.
Review tasks use AI_SESSION, running tasks AI_CLAIMED, ready tasks the
creation time."
  (ardentia--parse-stamp
   (pcase (plist-get task :queue)
     ('review (ardentia--task-prop task "AI_SESSION"))
     ('running (ardentia--task-prop task "AI_CLAIMED"))
     (_ (plist-get task :created)))))

(defun ardentia--task-age (task now)
  "Return the age of TASK in seconds relative to NOW, or nil."
  (let ((stamp (ardentia--task-stamp task)))
    (when stamp (float-time (time-subtract now stamp)))))

(defun ardentia--only-host (tags)
  "Return the host of the first NAME_only tag in TAGS, or nil."
  (seq-some (lambda (tag)
              (when (string-match "\\`\\(.+\\)_only\\'" tag)
                (match-string 1 tag)))
            tags))

(defun ardentia--task-host (task)
  "Return the host column value of TASK."
  (or (ardentia--task-prop task "AI_CLAIMED_BY")
      (ardentia--only-host (plist-get task :tags))
      "default"))

(defun ardentia--task-attempts (task)
  "Return the attempts column value of TASK."
  (let ((a (ardentia--task-prop task "AI_ATTEMPTS")))
    (cond ((null a) "")
          ((>= (string-to-number a) 3) (concat a "!"))
          (t a))))

(defun ardentia--task-due-time (task)
  "Return the time from which the runner picks up TASK, or nil.
Mirrors the runner's eligibility rule on the raw `:scheduled' string.
Only the date and the first HH:MM after it count (range start; repeater
and warning period are ignored).  No time or an impossible time (hour >
23, minute > 59) means due from 00:00 of that day.  Nil means no
SCHEDULED or one that cannot be parsed: due at once."
  (let ((s (ardentia--val (plist-get task :scheduled))))
    (when (and (stringp s)
               (string-match
                "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)"
                s))
      (let ((year (string-to-number (match-string 1 s)))
            (month (string-to-number (match-string 2 s)))
            (day (string-to-number (match-string 3 s)))
            (rest (substring s (match-end 0)))
            (hour 0) (minute 0))
        (when (string-match "\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)" rest)
          (let ((h (string-to-number (match-string 1 rest)))
                (m (string-to-number (match-string 2 rest))))
            (when (and (< h 24) (< m 60))
              (setq hour h minute m))))
        (encode-time (list 0 minute hour day month year nil -1 nil))))))

(defun ardentia--task-later-p (task now)
  "Return non-nil when TASK is ready but not yet due at time NOW.
Only tasks of the ready queue can be later."
  (and (eq (plist-get task :queue) 'ready)
       (let ((due (ardentia--task-due-time task)))
         (and due (time-less-p now due)))))

(defun ardentia--later-flag (task now)
  "Return \"due HH:MM\" or \"due MM-DD\" for the later TASK at time NOW."
  (let ((due (ardentia--task-due-time task)))
    (if (string= (format-time-string "%F" due) (format-time-string "%F" now))
        (format-time-string "due %H:%M" due)
      (format-time-string "due %m-%d" due))))

(defun ardentia--task-flags (task now)
  "Return the flags column value of TASK at time NOW."
  (let (flags)
    (when (ardentia--task-later-p task now)
      (push (ardentia--later-flag task now) flags))
    (when (ardentia--task-prop task "AI_LIMIT_INTERRUPTED")
      (push "limit" flags))
    (when (eq (plist-get task :queue) 'running)
      (let ((age (ardentia--task-age task now)))
        (when (and age (> age (* 60 ardentia-stale-minutes)))
          (push "stale?" flags))))
    (string-join (nreverse flags) " ")))

(defun ardentia--effective-queue (task &optional now)
  "Return the queue symbol of TASK for display; ready becomes `later'.
A ready task is later when NOW is given and the task is not yet due."
  (let ((queue (plist-get task :queue)))
    (if (and now (ardentia--task-later-p task now)) 'later queue)))

(defun ardentia--sort-rank (task &optional now)
  "Return the sort rank of TASK: review (blocked, partial, rest), running,
ready, later.  A ready task counts as later only when NOW is given."
  (pcase (ardentia--effective-queue task now)
    ('review (pcase (ardentia--task-prop task "AI_STATUS")
               ("blocked" 0) ("partial" 1) (_ 2)))
    ('running 3)
    ('ready 4)
    ('later 5)
    (_ 6)))

(defun ardentia--sort-key (task now)
  "Return the time TASK is sorted by inside its rank, or nil.
Later tasks use their due time (soonest first), the others their age stamp."
  (if (and now (ardentia--task-later-p task now))
      (ardentia--task-due-time task)
    (ardentia--task-stamp task)))

(defun ardentia--task-priority (task)
  "Return the priority character of TASK.
Use the first character of its :priority, upcased; without a cookie
return `ardentia-default-priority'."
  (let ((p (plist-get task :priority)))
    (if (and (stringp p) (not (string-empty-p p)))
        (upcase (aref p 0))
      ardentia-default-priority)))

(defun ardentia--sort-tasks (tasks &optional now)
  "Return TASKS sorted by queue; inside a rank the oldest task comes first.
Inside the ready rank, tasks are sorted by priority first (A before B),
a task without a cookie counting as `ardentia-default-priority'.
With NOW, ready tasks that are not yet due sort last, soonest due first."
  (let ((keyed (mapcar (lambda (task)
                         (cons task (ardentia--sort-key task now)))
                       tasks)))
    (mapcar #'car
            (sort keyed
                  (lambda (a b)
                    (let ((ra (ardentia--sort-rank (car a) now))
                          (rb (ardentia--sort-rank (car b) now)))
                      (cond ((/= ra rb) (< ra rb))
                            ((and (= ra 4)
                                  (/= (ardentia--task-priority (car a))
                                      (ardentia--task-priority (car b))))
                             (< (ardentia--task-priority (car a))
                                (ardentia--task-priority (car b))))
                            ((and (cdr a) (cdr b))
                             (time-less-p (cdr a) (cdr b)))
                            (t (and (cdr a) t)))))))))

(defun ardentia--queue-name (task &optional now)
  "Return the queue column value of TASK; \"later\" when not yet due at NOW."
  (pcase (ardentia--effective-queue task now)
    ('review "review") ('running "running") ('ready "ready")
    ('later "later") (_ "")))

(defun ardentia--task-row (task now)
  "Return the dashboard row vector for TASK at time NOW.
The Cost column is empty for a task without usage data."
  (vector (ardentia--queue-name task now)
          (or (ardentia--task-prop task "AI_STATUS") "")
          (ardentia--task-host task)
          (ardentia--task-attempts task)
          (ardentia--format-age (ardentia--task-age task now))
          (ardentia--task-flags task now)
          (ardentia--format-cost (ardentia--task-cost-total task))
          (file-name-nondirectory (or (plist-get task :file) ""))
          (let ((p (ardentia--stringify (plist-get task :priority))))
            (if p
                (format "[#%c] %s" (upcase (aref p 0)) (plist-get task :title))
              (plist-get task :title)))))

(defun ardentia--parse-hosts-conf (string)
  "Parse the contents STRING of aiready-hosts.conf.
Return a plist with :hosts (list of names) and :default-host."
  (let (hosts default)
    (dolist (line (split-string string "\n"))
      (cond ((string-match "\\`[ \t]*hosts=\\(.*\\)\\'" line)
             (setq hosts (split-string (match-string 1 line))))
            ((string-match "\\`[ \t]*default_host=\\([^ \t]*\\)" line)
             (setq default (match-string 1 line)))))
    (list :hosts hosts :default-host default)))

(defun ardentia--parse-timer-output (string)
  "Parse the `systemctl show' output STRING into a plist.
STRING holds the block of the service unit first, then the one of the
timer unit.  The plist has :last and :next (strings or nil when absent)
and :running, non-nil when the first ActiveState (the service's) is
\"activating\" or \"active\": a oneshot service is in that state while
a run is going on, and the timer has no next time until it ends."
  (let (last next state)
    (dolist (line (split-string string "\n"))
      (cond ((and (null state) (string-match "\\`ActiveState=\\(.*\\)\\'" line))
             (setq state (string-trim (match-string 1 line))))
            ((string-match "\\`LastTriggerUSec=\\(.*\\)\\'" line)
             (setq last (ardentia--val (string-trim (match-string 1 line)))))
            ((string-match "\\`NextElapseUSecRealtime=\\(.*\\)\\'" line)
             (setq next (ardentia--val (string-trim (match-string 1 line)))))))
    (list :last (unless (equal last "n/a") last)
          :next (unless (equal next "n/a") next)
          :running (and (member state '("activating" "active")) t))))

(defun ardentia--valid-tag-p (tag)
  "Return non-nil when TAG is a valid org tag."
  (string-match-p "\\`[[:alnum:]_@#%]+\\'" tag))

;;;; Files and hosts

(defun ardentia--org-files ()
  "Return the org files to scan, without Syncthing conflict copies."
  (let ((dir (expand-file-name ardentia-org-directory)))
    (when (file-directory-p dir)
      (seq-remove (lambda (f) (string-match-p "\\.sync-conflict-" f))
                  (directory-files dir t "\\.org\\'")))))

(defun ardentia-hosts ()
  "Return the plist of the hosts configuration, or nil when unreadable."
  (when (file-readable-p ardentia-hosts-file)
    (ardentia--parse-hosts-conf
     (with-temp-buffer
       (insert-file-contents ardentia-hosts-file)
       (buffer-string)))))

;;;; Async processes

(defun ardentia--run-async (program args callback)
  "Run PROGRAM with ARGS asynchronously, then call CALLBACK.
CALLBACK receives the exit status (nil when the process could not be
started), the standard output and the standard error as strings."
  (let ((out (generate-new-buffer " *ardentia-out*"))
        (err (generate-new-buffer " *ardentia-err*"))
        (default-directory (let ((d (expand-file-name ardentia-org-directory)))
                             (if (file-directory-p d) d default-directory)))
        proc timer)
    (condition-case e
        (setq proc
              (make-process
               :name "ardentia" :command (cons program args)
               :buffer out :stderr err :noquery t
               :connection-type 'pipe
               :sentinel
               (lambda (p _event)
                 (unless (process-live-p p)
                   (when timer (cancel-timer timer))
                   (let ((status (process-exit-status p))
                         (o (with-current-buffer out (buffer-string)))
                         (er (with-current-buffer err (buffer-string))))
                     (kill-buffer out)
                     (kill-buffer err)
                     (funcall callback status o er))))))
      (error
       (kill-buffer out)
       (kill-buffer err)
       (funcall callback nil "" (error-message-string e))))
    (when (processp proc)
      (setq timer (run-at-time ardentia-process-timeout nil
                               (lambda ()
                                 (when (process-live-p proc)
                                   (kill-process proc))))))
    proc))

(defun ardentia--make-join (count done)
  "Return a function that calls DONE with a list of results after COUNT calls.
The returned function takes (INDEX RESULT)."
  (let ((results (make-vector count nil)) (left count))
    (lambda (index result)
      (aset results index result)
      (setq left (1- left))
      (when (= left 0)
        (funcall done (append results nil))))))

(defun ardentia--fetch-tags (tags callback)
  "Fetch the open tasks tagged with any of TAGS asynchronously.
CALLBACK receives the deduplicated task list and an error string or nil."
  (let ((files (ardentia--org-files)))
    (if (null files)
        (funcall callback nil "no org files found")
      (let ((join (ardentia--make-join
                   (length tags)
                   (lambda (results)
                     (funcall callback
                              (ardentia--dedupe
                               (apply #'append (mapcar #'car results)))
                              (when-let* ((errs (delq nil (mapcar #'cdr results))))
                                (string-join errs "; "))))))
            (i 0))
        (dolist (tag tags)
          (let ((index i))
            (ardentia--run-async
             ardentia-org-tool (append (list "find-tagged" tag) files)
             (lambda (status out er)
               (funcall
                join index
                (cond
                 ((not (eql status 0))
                  (cons nil (format "org-tool %s failed: %s" tag
                                    (string-trim (if (string-blank-p er) out er)))))
                 (t (condition-case e
                        (cons (ardentia--tasks-from-json (ardentia--parse-json out))
                              nil)
                      (error (cons nil (format "org-tool %s: bad JSON: %s" tag
                                               (error-message-string e)))))))))))
          (setq i (1+ i)))))))

(defun ardentia--fetch (callback)
  "Fetch all agent tasks asynchronously and call CALLBACK.
CALLBACK receives the task list and an error string or nil."
  (ardentia--fetch-tags '("aiready" "ai_in_progress" "humanreview") callback))

(defvar ardentia--local-host nil
  "Cached name of the local agent host.")

(defun ardentia--fetch-host (callback)
  "Determine the local host name asynchronously and call CALLBACK with it.
The answer is cached; CALLBACK receives nil when it cannot be determined."
  (if ardentia--local-host
      (funcall callback ardentia--local-host)
    (ardentia--run-async
     ardentia-host-command nil
     (lambda (status out _err)
       (let ((name (and (eql status 0) (ardentia--val (string-trim out)))))
         (when name (setq ardentia--local-host name))
         (funcall callback name))))))

(defun ardentia--fetch-timer-status (callback)
  "Query the systemd timer asynchronously and call CALLBACK.
CALLBACK receives a plist with :last and :next, or nil when unknown."
  (ardentia--run-async
   "systemctl"
   (list "--user" "show" ardentia-service-unit ardentia-timer-unit
         "-p" "ActiveState" "-p" "LastTriggerUSec" "-p" "NextElapseUSecRealtime")
   (lambda (status out _err)
     (funcall callback
              (and (eql status 0) (ardentia--parse-timer-output out))))))

;;;; Shared state and refresh

(defvar ardentia--state nil
  "Plist of the last refresh: :tasks :host :timer :time :error :refreshing.")

(defvar ardentia--review-count nil
  "Number of open humanreview tasks at the last refresh, or nil if unknown.")

(defvar ardentia--refresh-callbacks nil
  "Callbacks waiting for the refresh that is currently running.")

(defun ardentia-refresh (&optional callback)
  "Refresh tasks, host and timer status asynchronously; then call CALLBACK.
When a refresh is already running, CALLBACK joins it."
  (when callback (push callback ardentia--refresh-callbacks))
  (unless (plist-get ardentia--state :refreshing)
    (setq ardentia--state (plist-put ardentia--state :refreshing t))
    (let ((join (ardentia--make-join
                 3
                 (lambda (results)
                   (pcase-let ((`((,tasks . ,err) ,host ,timer) results))
                     (setq ardentia--state
                           (list :tasks tasks :host host :timer timer
                                 :time (current-time) :error err
                                 :refreshing nil))
                     (unless err
                       (setq ardentia--review-count
                             (length (seq-filter
                                      (lambda (tk) (eq (plist-get tk :queue) 'review))
                                      tasks))))
                     (let ((cbs (nreverse ardentia--refresh-callbacks)))
                       (setq ardentia--refresh-callbacks nil)
                       (dolist (cb cbs) (funcall cb))))))))
      (ardentia--fetch (lambda (tasks err) (funcall join 0 (cons tasks err))))
      (ardentia--fetch-host (lambda (host) (funcall join 1 host)))
      (ardentia--fetch-timer-status (lambda (timer) (funcall join 2 timer))))))

;;;; Visiting headings

(defun ardentia--goto-id (id)
  "Move point to the heading with ID in the current buffer and return it.
Return nil when there is none.  Narrowing is ignored."
  (let ((pos (and id (org-find-entry-with-id id))))
    (when pos (goto-char pos))))

(defun ardentia-visit-task (task)
  "Visit the heading of TASK in its org file."
  (let ((file (plist-get task :file)))
    (find-file file)
    (let ((pos (or (and (plist-get task :id)
                        (org-find-entry-with-id (plist-get task :id)))
                   (save-excursion
                     (save-restriction
                       (widen)
                       (goto-char (point-min))
                       (forward-line (1- (or (plist-get task :line) 1)))
                       (point))))))
      (widen)
      (goto-char pos)
      (when (fboundp 'org-fold-show-context) (org-fold-show-context))
      (recenter 1))))

(declare-function org-find-entry-with-id "org" (ident))
(declare-function org-fold-show-context "org-fold" (&optional key))

(provide 'ardentia-core)
;;; ardentia-core.el ends here
