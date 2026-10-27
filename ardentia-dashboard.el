;;; ardentia-dashboard.el --- The task list of ardentia: dashboard, review, completed -*- lexical-binding: t -*-

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

;; There is one task list buffer, `*ardentia*', in `ardentia-dashboard-mode'.
;; It shows one of three views of the same tasks:
;;
;; - dashboard: the open agent queues (trouble, review, running, ready,
;;   later).  The trouble group holds the open tasks tagged for errors or
;;   sync problems and the running Syncthing merges; it is absent when
;;   there is nothing in it.  `ardentia-dashboard' shows this view.
;; - review: only the tasks waiting for review, `ardentia-review-inbox'.
;; - completed: every task the agents handed back, accepted or not, with
;;   tokens and cost, `ardentia-completed'.  The shared state drops done
;;   tasks, so this view keeps its own list, fetched when you switch to it
;;   and on every refresh while you are in it.
;;
;; The keys 1, 2, 3, ] and [ switch the view; 4 opens the agent stats
;; (`ardentia-stats'), a buffer of its own.  The filters (host,
;; title, later tasks, period, state) belong to the buffer, persist over
;; view switches and apply where they make sense; `f' shows them all.
;; Refreshing is asynchronous.
;;
;; In the dashboard and review views the review tasks are grouped under
;; their parent heading: a parent with two or more of them gets a header
;; row, a review task whose parent is itself a listed review task is
;; nested under it.  TAB folds or unfolds the group at point, S-TAB all of
;; them, RET on a header does the same as TAB.  Marks stay on folded
;; tasks.  See `ardentia-dashboard-group-reviews' and
;; `ardentia-dashboard-group-min'.
;;
;; Tasks are marked, tagged, scheduled and changed with the keys of
;; org-agenda: `m' marks, `:' sets tags, `C-c C-s' schedules, `B' applies
;; a bulk action to the marked tasks.  Marks are kept as task keys, so
;; they survive every re-render.
;;
;; The header line starts with the view indicator, then lists the active
;; filters, then host, timer, refresh time and usage (completed view: the
;; totals and the refresh time of its own fetch).  In a narrow window the
;; timer times are shortened and what still does not fit moves into lines
;; at the top of the buffer.

;;; Code:

(require 'tabulated-list)
(require 'seq)
(require 'cl-lib)
(require 'subr-x)
(require 'ardentia-core)
(require 'ardentia-actions)
(require 'ardentia-review)
(require 'ardentia-session)
(require 'ardentia-run)
(require 'ardentia-usage)
(require 'ardentia-completed)
(require 'ardentia-model)
(require 'ardentia-stats)

(declare-function ardentia "ardentia" ())
(declare-function dired "dired" (dirname &optional switches))

(defvar-local ardentia-dashboard--filter nil
  "Legacy queue filter of buffers made before the views existed.
Only read when `ardentia-dashboard--view' is unset: `review' then means
the review view.  Nothing sets it any more.")

(defconst ardentia-dashboard--views '(dashboard review completed)
  "The views of the task list, in the order ] cycles through them.")

(defconst ardentia-dashboard--view-labels
  '((dashboard . "Dashboard") (review . "Review") (completed . "Completed"))
  "Alist of view and the name the view indicator shows.")

(defvar-local ardentia-dashboard--view nil
  "The view this buffer shows, one of `ardentia-dashboard--views', or nil.
Nil means a buffer made by older code; `ardentia-dashboard--view-now'
then derives the view from `ardentia-dashboard--filter'.")

(defvar-local ardentia-dashboard--format-kind nil
  "Which column format is installed: `dashboard' or `completed', or nil.
Nil (a buffer of older code) makes the next render install the format.")

(defvar-local ardentia-dashboard--view-points nil
  "Alist of view and the row key that had point when the view was left.")

(defvar-local ardentia-dashboard--folded nil
  "Keys of the folded groups of this buffer.
A key is (FILE ID-OR-LINE) of the parent heading, like
`ardentia--task-key'.  Kept over re-renders and view switches, like the
marks.")

(defvar-local ardentia-dashboard--hidden nil
  "The tasks the last render left out because their group is folded.
Marks on them stay, so pruning marks and bulk actions look here too.")

(defvar-local ardentia-dashboard--group-keys nil
  "The keys of all groups (and parent tasks) of the last render.")

(defvar-local ardentia-dashboard--marked nil
  "Keys (`ardentia--task-key') of the marked tasks of this buffer.
Keys, not rows: the buffer is printed anew on every refresh.")

(defvar-local ardentia-dashboard--filter-host nil
  "Host filter of this buffer: a host name, or nil for all hosts.")

(defvar-local ardentia-dashboard--filter-title nil
  "Title filter of this buffer: a regexp string, or nil for no filter.")

(defvar-local ardentia-dashboard--filter-tags nil
  "Tag filter of this buffer: a list of tag specs, or nil for no filter.
A spec is a tag the task must carry (\"foo\") or, prefixed with a minus,
one it must not carry (\"-foo\").")

(defvar-local ardentia-dashboard--period 'default
  "Period filter of the completed view, or `default' for the option.
The option is `ardentia-completed-period'.")

(defvar-local ardentia-dashboard--state-filter 'all
  "State filter of the completed view: `all', `review' or `accepted'.")

(defvar-local ardentia-dashboard--completed-tasks nil
  "All tasks fetched by the last successful completed fetch of this buffer.")

(defvar-local ardentia-dashboard--completed-time nil
  "Time of the last completed fetch (successful or not) of this buffer.")

(defvar-local ardentia-dashboard--completed-error nil
  "Error text of the last completed fetch of this buffer, or nil.")

(defvar-local ardentia-dashboard--completed-refreshing nil
  "Non-nil while this buffer waits for a completed fetch.")

(defvar-local ardentia-dashboard--completed-summary nil
  "Totals text of the completed rows shown by the last render, or nil.")

(defconst ardentia-dashboard--format
  [("Queue" 8 nil) ("Status" 10 nil) ("Host" 10 nil) ("Att" 4 nil)
   ("Age" 5 nil) ("Flags" 16 nil) ("Cost" 7 nil :right-align t)
   ("File" 18 nil) ("Title" 0 nil)]
  "The `tabulated-list-format' of the dashboard and review views.
Treat it as read-only; buffers get a copy.")

(defun ardentia-dashboard--view-now ()
  "Return the view of the current buffer.
An unset view, as in a buffer made by older code, is `review' when the old
`ardentia-dashboard--filter' says so, else `dashboard'."
  (cond ((memq ardentia-dashboard--view ardentia-dashboard--views)
         ardentia-dashboard--view)
        ((eq ardentia-dashboard--filter 'review) 'review)
        (t 'dashboard)))

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

(defface ardentia-view-active
  '((t :inherit font-lock-function-name-face :weight normal))
  "The active view in the view indicator of the header line."
  :group 'ardentia-faces)

(defface ardentia-group
  '((t :inherit font-lock-type-face :weight bold))
  "The header row of a group of review tasks and its fold indicator."
  :group 'ardentia-faces)

(defface ardentia-mark
  '((t :inherit warning :weight bold))
  "The mark of a marked task, and the count in the header line."
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

(defun ardentia-dashboard--view-segment ()
  "Return the view indicator, the first segment of the header line.
It lists all views with the active one in brackets, like
\" [Dashboard] Review Completed\"."
  (let ((view (ardentia-dashboard--view-now)))
    (concat
     " "
     (mapconcat
      (lambda (v)
        (let ((label (alist-get v ardentia-dashboard--view-labels)))
          (if (eq v view)
              (ardentia-dashboard--maybe-face (format "[%s]" label)
                                              'ardentia-view-active)
            (ardentia-dashboard--maybe-face label 'ardentia-dashboard-meta))))
      ardentia-dashboard--views " "))))

(defun ardentia-dashboard--filter-segments ()
  "Return the segment listing the active filters of the view, or nil."
  (when-let* ((parts (ardentia-dashboard--filter-parts)))
    (list (ardentia-dashboard--maybe-face
           (concat " | filter: " (string-join parts ", "))
           'ardentia-flag-attention))))

(defun ardentia-dashboard--completed-segments (&optional _compact)
  "Return the header segments of the completed view after the filters.
They are the totals of the rows shown and the state of its own fetch."
  (append
   (mapcar (lambda (part) (concat " | " part))
           (split-string (or ardentia-dashboard--completed-summary
                             (ardentia-completed--summary nil))
                         " | " t "[ ]+"))
   (list (format " | Refreshed: %s"
                 (if ardentia-dashboard--completed-time
                     (format-time-string "%H:%M:%S"
                                         ardentia-dashboard--completed-time)
                   "never")))
   (and ardentia-dashboard--completed-error
        (list (ardentia-dashboard--maybe-face
               (format " | ERROR: %s" ardentia-dashboard--completed-error)
               'ardentia-flag-trouble)))
   (and ardentia-dashboard--completed-refreshing
        (list (ardentia-dashboard--maybe-face " | refreshing..."
                                              'ardentia-flag-info)))))

(defun ardentia-dashboard--info-segments (&optional state compact now)
  "Return the info line from STATE as a list of strings.
The first is the view indicator, \" [Dashboard] Review Completed\", every
further one starts with \" | \".  Then come the active filters and the
data of the view: host, timer, refresh time and usage, or in the completed
view its totals and refresh time.  With COMPACT the timer times are
shortened (see `ardentia-dashboard--short-time', relative to NOW).  STATE
defaults to `ardentia--state'."
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
     (list (ardentia-dashboard--view-segment))
     (ardentia-dashboard--filter-segments)
     (and ardentia-dashboard--marked
          (list (ardentia-dashboard--maybe-face
                 (format " | %d marked" (length ardentia-dashboard--marked))
                 'ardentia-mark)))
     (if (eq (ardentia-dashboard--view-now) 'completed)
         (ardentia-dashboard--completed-segments compact)
       (append
        (list (format " | Host: %s" (or (plist-get st :host) "unknown"))
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
                (split-string usage " | " t)))))))

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
The key `l' toggles this per task list buffer, in the dashboard view."
  :type 'boolean
  :group 'ardentia)

(defvar ardentia-dashboard--refresh-timer nil
  "The one global timer that refreshes visible dashboards, or nil.")

;; The `:set' of `ardentia-dashboard-refresh-interval' runs at once when the
;; option is already bound, i.e. when the file is loaded again: everything
;; it calls must be defined above it.
(defvar ardentia-dashboard-refresh-interval)

(defun ardentia-dashboard--live-buffers ()
  "Return the live dashboard buffers."
  (seq-filter (lambda (b) (with-current-buffer b
                            (derived-mode-p 'ardentia-dashboard-mode)))
              (buffer-list)))

(defun ardentia-dashboard--refresh-timer-start ()
  "Start the auto-refresh timer unless disabled or already running."
  (when (and (integerp ardentia-dashboard-refresh-interval)
             (> ardentia-dashboard-refresh-interval 0)
             (not (timerp ardentia-dashboard--refresh-timer)))
    (setq ardentia-dashboard--refresh-timer
          (run-at-time ardentia-dashboard-refresh-interval
                       ardentia-dashboard-refresh-interval
                       #'ardentia-dashboard--refresh-tick))))

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

(defcustom ardentia-dashboard-group-reviews t
  "Non-nil means review tasks are grouped under their parent heading.
Applies to the dashboard and the review view.  See
`ardentia-dashboard-group-min' and the keys TAB, S-TAB and RET on a group.
Sorting a column by its title shows the tasks flat for as long as the
sort lasts."
  :type 'boolean
  :group 'ardentia)

(defcustom ardentia-dashboard-group-min 2
  "Number of review tasks a parent heading needs to get a group header.
A parent with fewer visible review tasks leaves them flat.  A review task
below another listed review task is always nested under it."
  :type 'integer
  :group 'ardentia)

;;;; Groups: review tasks under their parent heading

(defun ardentia-dashboard--groupable-p (task)
  "Return non-nil when TASK may be put in a group.
Review tasks only; trouble tasks, merges and done tasks stay flat."
  (and (eq (plist-get task :queue) 'review)
       (not (plist-get task :trouble))
       (not (plist-get task :merge))
       (not (plist-get task :done))))

(defun ardentia-dashboard--parent-key (task)
  "Return the key (FILE ID-OR-LINE) of the parent heading of TASK, or nil.
The shape is that of `ardentia--task-key', so the parent of a task is
recognised when it is listed as a task itself."
  (when-let* ((parent (plist-get task :parent))
              (k (or (plist-get parent :id) (plist-get parent :line))))
    (list (plist-get task :file) k)))

(defun ardentia-dashboard--group-nodes (tasks folded)
  "Arrange the sorted TASKS into display nodes, with FOLDED groups closed.
Return a list (NODES HIDDEN KEYS).  A node is a plist: (:task TASK
:depth N :kids N :total N :folded BOOL :last BOOL) for a task, or a group
header (:group t :key KEY :title :outline :file :count :folded).  HIDDEN
are the tasks left out by folding, KEYS the keys of all groups.

Review tasks are grouped by the direct parent heading only.  When that
parent is a listed review task, the task is nested under it and the
parent task row acts as the header.  Otherwise a parent with at least
`ardentia-dashboard-group-min' listed review tasks gets a header row
and its tasks follow, one step indented.  A group stands where its
first member stands in TASKS; members keep their order."
  (let ((by-key (make-hash-table :test 'equal))
        (kids (make-hash-table :test 'equal))
        (up (make-hash-table :test 'equal))
        (groups (make-hash-table :test 'equal))
        (emitted (make-hash-table :test 'equal))
        (min-size (max 1 ardentia-dashboard-group-min))
        nodes hidden keys)
    (dolist (tk tasks)
      (when (ardentia-dashboard--groupable-p tk)
        (puthash (ardentia--task-key tk) tk by-key)))
    (dolist (tk tasks)
      (when-let* (((ardentia-dashboard--groupable-p tk))
                  (pk (ardentia-dashboard--parent-key tk)))
        (let ((ptask (gethash pk by-key)))
          (if (and ptask (not (eq ptask tk)))
              (progn (puthash (ardentia--task-key tk) ptask up)
                     (puthash pk (cons tk (gethash pk kids)) kids))
            (puthash pk (cons tk (gethash pk groups)) groups)))))
    (cl-labels
        ((descendants (tk)
           (let ((n 0))
             (dolist (c (gethash (ardentia--task-key tk) kids) n)
               (setq n (+ n 1 (descendants c))))))
         (hide (tk)
           (let ((key (ardentia--task-key tk)))
             (unless (gethash key emitted)
               (puthash key t emitted)
               (push tk hidden)
               (when (gethash key kids) (push key keys))
               (mapc #'hide (reverse (gethash key kids))))))
         (emit (tk depth last)
           (let* ((key (ardentia--task-key tk)))
             (unless (gethash key emitted)
               (puthash key t emitted)
               (let* ((ks (reverse (gethash key kids)))
                      (open (not (member key folded))))
                 (when ks (push key keys))
                 (push (list :task tk :depth depth :kids (length ks)
                             :total (descendants tk)
                             :folded (and ks (not open)) :last last)
                       nodes)
                 (while ks
                   (let ((c (pop ks)))
                     (if open (emit c (1+ depth) (null ks)) (hide c))))))))
         (root (tk)
           (let ((cur tk) (n 0))
             (while (and (gethash (ardentia--task-key cur) up)
                         (< n (hash-table-count by-key)))
               (setq cur (gethash (ardentia--task-key cur) up)
                     n (1+ n)))
             cur))
         (emit-group (pk members)
           (let* ((parent (plist-get (car members) :parent))
                  (open (not (member pk folded)))
                  (count (apply #'+ (mapcar (lambda (m) (1+ (descendants m)))
                                            members))))
             (push pk keys)
             (push (list :group t :key pk
                         :title (or (plist-get parent :title) "")
                         :outline (plist-get (car members) :outline)
                         :file (car pk) :count count :folded (not open))
                   nodes)
             (while members
               (let ((m (pop members)))
                 (if open (emit m 1 (null members)) (hide m)))))))
      (dolist (tk tasks)
        (cond ((not (ardentia-dashboard--groupable-p tk))
               (push (list :task tk :depth 0 :kids 0) nodes))
              ((gethash (ardentia--task-key tk) emitted))
              (t (let* ((r (root tk))
                        (pk (ardentia-dashboard--parent-key r))
                        (members (and pk (not (gethash pk by-key))
                                      (reverse (gethash pk groups)))))
                   (if (>= (length members) min-size)
                       (emit-group pk members)
                     (emit r 0 t)))))))
    (list (nreverse nodes) hidden keys)))

(defun ardentia-dashboard--fold-glyph (folded)
  "Return the fold indicator for a FOLDED or open group."
  (if folded "\u25b8" "\u25be"))

(defun ardentia-dashboard--decorate-title (title node)
  "Return TITLE of the task of NODE with its tree prefix and group count."
  (let ((depth (plist-get node :depth))
        (kids (plist-get node :kids)))
    (if (and (zerop depth) (zerop kids))
        title
      (concat
       (ardentia-dashboard--maybe-face
        (concat (make-string (* 2 depth) ?\s)
                (cond ((> kids 0)
                       (ardentia-dashboard--fold-glyph (plist-get node :folded)))
                      ((plist-get node :last) "\u2514")
                      (t "\u251c"))
                " ")
        'ardentia-group)
       title
       (if (> kids 0)
           (ardentia-dashboard--maybe-face
            (format " (%d)" (plist-get node :total))
            'ardentia-dashboard-meta)
         "")))))

(defun ardentia-dashboard--group-row (group)
  "Return the row vector of the header row GROUP."
  (let* ((title (plist-get group :title))
         (text (format "%s %s (%d)"
                       (ardentia-dashboard--fold-glyph (plist-get group :folded))
                       title (plist-get group :count)))
         (file (plist-get group :file)))
    (vector "" "" "" "" "" "" ""
            (ardentia-dashboard--maybe-face
             (file-name-nondirectory (or file "")) 'ardentia-dashboard-meta)
            (propertize (if ardentia-dashboard-colors
                            (propertize text 'face 'ardentia-group)
                          text)
                        'help-echo
                        (string-join (append (plist-get group :outline)
                                             (list title))
                                     " / ")))))

(defun ardentia-dashboard--entries (tasks filter now &optional hide-later merges)
  "Return `tabulated-list-entries' for TASKS restricted to queue FILTER at NOW.
With HIDE-LATER, ready tasks that are not yet due are left out, trouble
tasks never.  MERGES, the running merge rows, are added when FILTER is nil.
Review tasks are grouped, see `ardentia-dashboard-group-reviews'; not
while a column sort is active, which would scatter the groups."
  (let* ((sorted
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
           now))
         (grouped (and ardentia-dashboard-group-reviews
                       (null tabulated-list-sort-key)
                       (seq-some (lambda (tk) (plist-get tk :parent)) sorted)))
         (result (if grouped
                     (ardentia-dashboard--group-nodes
                      sorted ardentia-dashboard--folded)
                   (list (mapcar (lambda (tk) (list :task tk :depth 0 :kids 0))
                                 sorted)
                         nil nil))))
    (setq ardentia-dashboard--hidden (nth 1 result)
          ardentia-dashboard--group-keys (nth 2 result))
    (mapcar
     (lambda (node)
       (if (plist-get node :group)
           (list node (ardentia-dashboard--group-row node))
         (let* ((task (plist-get node :task))
                (row (ardentia-dashboard--colorize-row
                      task
                      (ardentia--task-row
                       task now
                       (and (eq (plist-get task :queue) 'review)
                            (ardentia-review-for-you-class task)))
                      now)))
           (when (or (> (plist-get node :depth) 0) (> (plist-get node :kids) 0))
             (setq row (copy-sequence row))
             (aset row 8 (ardentia-dashboard--decorate-title (aref row 8) node)))
           (list task row))))
     (nth 0 result))))

;;;; Filters

(defun ardentia-dashboard--period-now ()
  "Return the period filter of this buffer, the option as long as unset."
  (if (eq ardentia-dashboard--period 'default)
      ardentia-completed-period
    ardentia-dashboard--period))

(defun ardentia-dashboard--filter-parts (&optional view)
  "Return the active filters of VIEW (default: this buffer's) as strings.
Only filters that apply in the view count, and only those that differ
from their defaults.  The result is nil when the view is unfiltered."
  (let ((view (or view (ardentia-dashboard--view-now))))
    (delq nil
          (list
           (and ardentia-dashboard--filter-host
                (format "host %s" ardentia-dashboard--filter-host))
           (and ardentia-dashboard--filter-title
                (format "/%s/" ardentia-dashboard--filter-title))
           (and ardentia-dashboard--filter-tags
                (ardentia-dashboard--tags-text ardentia-dashboard--filter-tags))
           (and (eq view 'dashboard)
                (not (eq (and (ardentia-dashboard--show-later-p) t)
                         (and ardentia-dashboard-show-later t)))
                (if (ardentia-dashboard--show-later-p)
                    "later shown" "later hidden"))
           (and (eq view 'completed)
                (not (eq (ardentia-dashboard--period-now)
                         ardentia-completed-period))
                (format "period %s" (ardentia-completed--period-label
                                     (ardentia-dashboard--period-now))))
           (and (eq view 'completed)
                (not (eq ardentia-dashboard--state-filter 'all))
                (format "state %s" (ardentia-completed--state-label
                                    ardentia-dashboard--state-filter)))))))

(defun ardentia-dashboard--title-match-p (regexp title)
  "Return non-nil when TITLE matches REGEXP, ignoring case; nil REGEXP matches."
  (or (null regexp)
      (let ((case-fold-search t))
        (string-match-p regexp (or title "")))))

(defun ardentia-dashboard--normalize-tag-specs (specs)
  "Return SPECS, tag filter entries, trimmed and without a leading plus.
Empty entries are dropped, duplicates removed; a minus prefix stays."
  (let (result)
    (dolist (spec specs)
      (let ((spec (string-trim spec)))
        (when (string-prefix-p "+" spec)
          (setq spec (string-trim-left (substring spec 1))))
        (unless (member spec '("" "-"))
          (push spec result))))
    (seq-uniq (nreverse result))))

(defun ardentia-dashboard--tags-text (specs)
  "Return the tag filter SPECS as text, each required tag with a plus."
  (concat "tags "
          (mapconcat (lambda (spec)
                       (if (string-prefix-p "-" spec) spec (concat "+" spec)))
                     specs " ")))

(defun ardentia-dashboard--tags-match-p (specs tags)
  "Return non-nil when the task TAGS satisfy the filter SPECS.
A spec \"foo\" needs the tag foo, \"-foo\" needs it absent.  The match
is exact and case-sensitive, like org tags.  Nil SPECS match everything."
  (seq-every-p (lambda (spec)
                 (if (string-prefix-p "-" spec)
                     (not (member (substring spec 1) tags))
                   (member spec tags)))
               specs))

(defun ardentia-dashboard--task-host-name (task default-host)
  "Return the host TASK counts for in the host filter.
That is `ardentia--task-host': the host that claimed the task (also for a
completed one), else the NAME of a NAME_only tag, else the default host.
DEFAULT-HOST, the one of the hosts file, stands for what the Host column
shows as \"default\"; it may be nil."
  (let ((host (ardentia--task-host task)))
    (if (and (equal host "default") default-host) default-host host)))

(defun ardentia-dashboard--filter-tasks (tasks host title default-host
                                               &optional tags)
  "Return the TASKS that pass the HOST, TITLE and TAGS filters.
Any filter nil lets everything pass.  TAGS is a list of tag specs, see
`ardentia-dashboard--tags-match-p'.  See `ardentia-dashboard--task-host-name'
for DEFAULT-HOST."
  (if (not (or host title tags))
      tasks
    (seq-filter (lambda (task)
                  (and (or (null host)
                           (equal host (ardentia-dashboard--task-host-name
                                        task default-host)))
                       (ardentia-dashboard--title-match-p
                        title (plist-get task :title))
                       (ardentia-dashboard--tags-match-p
                        tags (plist-get task :tags))))
                tasks)))

(defun ardentia-dashboard--filter-merges (merges host title local-host
                                                &optional tags)
  "Return the running MERGES that pass the HOST, TITLE and TAGS filters.
A merge runs on the local host, LOCAL-HOST.  A merge has no tags, so it
is hidden while TAGS require a tag and passes when TAGS only exclude."
  (seq-filter (lambda (merge)
                (and (or (null host) (equal host local-host))
                     (ardentia-dashboard--tags-match-p tags nil)
                     (ardentia-dashboard--title-match-p
                      title (plist-get merge :title))))
              merges))

(defun ardentia-dashboard--next-host (current local hosts)
  "Return the host filter after CURRENT: all (nil), LOCAL, then HOSTS.
CURRENT is nil or a host name.  LOCAL may be nil.  A CURRENT that is in
none of them goes back to all."
  (let* ((choices (cons nil (delete-dups
                             (append (and local (list local))
                                     (copy-sequence hosts)))))
         (rest (cdr (member current choices))))
    (if (and (member current choices) rest) (car rest) (car choices))))

;;;; Keys

(ardentia-define-keys-and-menu
    ardentia-dashboard-mode-map ardentia-dashboard-menu
  :keymap-doc "Keymap for `ardentia-dashboard-mode'."
  :menu-doc "Keys of the ardentia task list.
Also bound: S-<up> and S-<down> (priority up and down), C-c C-q (tags),
c (completed view) and r (review view).  Marks, tags, schedule, deadline,
TODO state and priority work like in org-agenda."
  :groups
  (("Task at point"
    ("RET" "Open" ardentia-dashboard-enter)
    ("o" "Visit heading" ardentia-dashboard-visit)
    ("t" "Transcript" ardentia-dashboard-transcript)
    ("A" "Agent shell" ardentia-dashboard-agent-shell))
   ("Review and run"
    ("a" "Accept (marked)" ardentia-dashboard-accept)
    ("s" "Send back" ardentia-dashboard-send-back)
    ("x" "Run now" ardentia-dashboard-run)
    ("k" "Stop" ardentia-dashboard-stop))
   ("Mark"
    ("m" "Mark, next line" ardentia-dashboard-mark)
    ("u" "Unmark, next line" ardentia-dashboard-unmark)
    ("U" "Unmark all" ardentia-dashboard-unmark-all)
    ("M-m" "Toggle mark" ardentia-dashboard-mark-toggle)
    ("*" "Mark all" ardentia-dashboard-mark-all)
    ("M-*" "Toggle all marks" ardentia-dashboard-mark-toggle-all)
    ("%" "Mark by title regexp" ardentia-dashboard-mark-regexp))
   ("Edit"
    (":" "Tags (C-c C-q)" ardentia-dashboard-set-tags)
    ("C-c C-s" "Schedule" ardentia-dashboard-schedule)
    ("C-c C-d" "Deadline" ardentia-dashboard-deadline)
    ("C-c C-t" "TODO state" ardentia-dashboard-todo)
    ("," "Priority" ardentia-dashboard-priority)
    ("+" "Priority up (S-<up>)" ardentia-dashboard-priority-up)
    ("-" "Priority down (S-<down>)" ardentia-dashboard-priority-down)
    ("M" "Model of a role (AI_MODEL_<ROLE>)" ardentia-dashboard-set-model)
    ("B" "Bulk action on marked" ardentia-dashboard-bulk-action))
   ("View"
    ("1" "Dashboard" ardentia-dashboard)
    ("2" "Review (r)" ardentia-review-inbox)
    ("3" "Completed (c)" ardentia-completed)
    ("4" "Agent stats (own buffer)" ardentia-stats)
    ("]" "Next view" ardentia-dashboard-next-view)
    ("[" "Previous view" ardentia-dashboard-previous-view)
    ("g" "Refresh" ardentia-dashboard-refresh)
    ("d" "Toggle due date/time left" ardentia-dashboard-toggle-due)
    ("TAB" "Fold/unfold group" ardentia-dashboard-fold-toggle)
    ("<backtab>" "Fold/unfold all groups" ardentia-dashboard-fold-all)
    ("$" "Usage" ardentia-usage-refresh)
    ("q" "Quit" quit-window))
   ("Filter"
    ("f" "Filter menu" ardentia-dashboard-filter-menu)
    ("h" "Cycle host" ardentia-dashboard-cycle-host)
    ("/" "Title regexp" ardentia-dashboard-filter-title)
    ("\\" "Tags (+tag, -tag)" ardentia-dashboard-filter-tags)
    ("l" "Toggle later (Dashboard)" ardentia-dashboard-toggle-later)
    ("P" "Cycle period (Completed)" ardentia-dashboard-cycle-period)
    ("w" "Cycle state (Completed)" ardentia-dashboard-cycle-state)))
  :alternates
  (("C-c C-q" . ardentia-dashboard-set-tags)
   ("S-<up>" . ardentia-dashboard-priority-up)
   ("S-<down>" . ardentia-dashboard-priority-down)
   ("c" . ardentia-completed)
   ("r" . ardentia-review-inbox)))

(add-hook 'ardentia-usage-updated-hook #'ardentia-dashboard--render-all)

;; Both the S key and a click on a column title sort and thereby scatter the
;; group rows; the remaps rebuild them (see `ardentia-dashboard-sort').
(keymap-set ardentia-dashboard-mode-map "<remap> <tabulated-list-sort>"
            #'ardentia-dashboard-sort)
(keymap-set ardentia-dashboard-mode-map "<remap> <tabulated-list-col-sort>"
            #'ardentia-dashboard-col-sort)

(define-derived-mode ardentia-dashboard-mode tabulated-list-mode "Ardentia"
  "Major mode listing the agent team's tasks in one of three views.
The views are the dashboard (all open queues), the review queue and the
completed tasks; see `ardentia-dashboard--view'.

\\{ardentia-dashboard-mode-map}"
  (setq tabulated-list-padding 1)
  (setq tabulated-list-printer #'ardentia-dashboard--print-entry)
  (setq tabulated-list-sort-key nil)
  (setq ardentia--current-task-function #'ardentia-dashboard--task-at-point)
  ;; The info line owns the header line; the column titles go into the buffer.
  (setq tabulated-list-use-header-line nil)
  (ardentia-dashboard--ensure-format (ardentia-dashboard--view-now))
  (setq header-line-format
        '(:eval (ardentia-dashboard--header-line)))
  (add-hook 'window-size-change-functions
            #'ardentia-dashboard--window-size-change nil t)
  (ardentia-dashboard--refresh-timer-start))

(defun ardentia-dashboard--group-p (id)
  "Return non-nil when the row ID is a group header row."
  (and id (plist-get id :group) t))

(defun ardentia-dashboard--non-task-p (id)
  "Return non-nil when the row ID is a running merge or a group header."
  (and id (or (plist-get id :merge) (plist-get id :group)) t))

(defun ardentia-dashboard--task-at-point ()
  "Return the task plist of the row at point, or nil.
The header row of a group is no task; see `ardentia-dashboard--group-at-point'."
  (let ((id (tabulated-list-get-id)))
    (unless (ardentia-dashboard--group-p id) id)))

(defun ardentia-dashboard--group-at-point ()
  "Return the group header row at point, or nil."
  (let ((id (tabulated-list-get-id)))
    (and (ardentia-dashboard--group-p id) id)))

(defun ardentia-dashboard--row-key (id)
  "Return the stable key of the row ID, a task, merge or group plist, or nil.
Unlike the plist itself the key survives edits of the task, such as a
shifted line number or a changed property."
  (cond ((null id) nil)
        ((plist-get id :group) (list :group (plist-get id :key)))
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
mark of point and a list of (WINDOW MARK SCROLL-LINES AT-TOP).
SCROLL-LINES is the number of lines the window start lies above the
window point.  AT-TOP is non-nil when the window start was the start of
the buffer, so the column titles in line 1 are in view."
  (cons (ardentia-dashboard--mark-at (point))
        (mapcar (lambda (win)
                  (let ((pt (window-point win)))
                    ;; Count to the start of the line: `count-lines' adds one
                    ;; when PT is past it, which would scroll up every render.
                    (list win (ardentia-dashboard--mark-at pt)
                          (count-lines (window-start win)
                                       (save-excursion
                                         (goto-char pt)
                                         (line-beginning-position)))
                          (= (window-start win) (point-min)))))
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
MARKS is the result of `ardentia-dashboard--remember-positions'.
A window that showed the top of the buffer keeps showing it, whatever
the number of rows above its point has become."
  (save-excursion
    (dolist (entry (cdr marks))
      (pcase-let ((`(,win ,mark ,scroll ,at-top) entry))
        (when (window-live-p win)
          (ardentia-dashboard--goto-mark mark)
          (set-window-point win (point))
          (if at-top
              (set-window-start win (point-min) t)
            (forward-line (- scroll))
            (set-window-start win (point) t))))))
  (ardentia-dashboard--goto-mark (car marks)))

(defun ardentia-dashboard--ensure-format (view)
  "Install the column format that VIEW needs unless it is installed.
Buffers made by older code have none of the new variables, so this runs
in every render.  A new format resets the sort key."
  (let ((kind (if (eq view 'completed) 'completed 'dashboard)))
    (unless (eq ardentia-dashboard--format-kind kind)
      (setq tabulated-list-format
            (copy-tree (if (eq kind 'completed)
                           ardentia-completed--format
                         ardentia-dashboard--format)
                       t)
            tabulated-list-sort-key nil
            ardentia-dashboard--format-kind kind)
      (tabulated-list-init-header))))

(defun ardentia-dashboard--default-host ()
  "Return the default host of the hosts file, or nil."
  (plist-get (ardentia-hosts) :default-host))

(defun ardentia-dashboard--view-entries (view now)
  "Return `tabulated-list-entries' for VIEW at NOW from the current filters.
In the completed view also record the totals for the header line."
  (let* ((host ardentia-dashboard--filter-host)
         (title ardentia-dashboard--filter-title)
         (tags ardentia-dashboard--filter-tags)
         (default-host (and host (ardentia-dashboard--default-host))))
    (if (eq view 'completed)
        (let* ((tasks (ardentia-dashboard--filter-tasks
                       ardentia-dashboard--completed-tasks
                       host title default-host tags))
               (period (ardentia-dashboard--period-now))
               (state ardentia-dashboard--state-filter))
          (setq ardentia-dashboard--completed-summary
                (ardentia-completed--summary
                 (ardentia-completed--select tasks period state now)))
          (ardentia-completed--entries tasks period state now))
      (let ((ardentia--due-display-override (ardentia-dashboard--due-display))
            (dashboard (eq view 'dashboard)))
        (ardentia-dashboard--entries
         (ardentia-dashboard--filter-tasks
          (plist-get ardentia--state :tasks) host title default-host tags)
         (and (eq view 'review) 'review)
         now
         (and dashboard (not (ardentia-dashboard--show-later-p)))
         (and dashboard
              (ardentia-dashboard--filter-merges
               (plist-get ardentia--state :merges)
               host title (plist-get ardentia--state :host) tags)))))))

(defun ardentia-dashboard--goto-key (key)
  "Move point to the row with the row KEY; return nil and stay when none."
  (when key
    (let ((start (point)))
      (goto-char (point-min))
      (or (catch 'found
            (while (not (eobp))
              (when (equal key (ardentia-dashboard--row-key
                                (tabulated-list-get-id)))
                (throw 'found t))
              (forward-line 1)))
          (progn (goto-char start) nil)))))

(defun ardentia-dashboard--goto-first-row ()
  "Move point to the first row, or to the start of an empty list."
  (goto-char (point-min))
  (while (and (not (eobp)) (null (tabulated-list-get-id)))
    (forward-line 1))
  (when (eobp) (goto-char (point-min))))

(defun ardentia-dashboard--render (&optional switched)
  "Fill the current task list buffer from its view and filters.
Point and the windows keep their rows, see
`ardentia-dashboard--remember-positions'.  With SWITCHED the view has just
changed: point goes to the row remembered for the view instead, or to the
top."
  (let ((view (ardentia-dashboard--view-now)))
    (ardentia-dashboard--ensure-format view)
    (setq ardentia-dashboard--view view)
    ;; A buffer already in this mode may predate the printer.
    (setq tabulated-list-printer #'ardentia-dashboard--print-entry)
    (setq tabulated-list-entries (ardentia-dashboard--view-entries
                                  view (current-time)))
    ;; The original order of `tabulated-list-sort' refers to the old entries.
    (setq tabulated-list--original-order nil)
    (ardentia-dashboard--prune-marks)
    (let ((marks (unless switched (ardentia-dashboard--remember-positions))))
      (tabulated-list-print)
      (ardentia-dashboard--update-layout)
      (if (not switched)
          (ardentia-dashboard--restore-positions marks)
        (unless (ardentia-dashboard--goto-key
                 (alist-get view ardentia-dashboard--view-points))
          (ardentia-dashboard--goto-first-row))
        (dolist (win (get-buffer-window-list (current-buffer) nil t))
          (set-window-point win (point))
          (set-window-start win (point-min) t))))))

(defun ardentia-dashboard--render-all ()
  "Render all live dashboard buffers."
  (dolist (buf (ardentia-dashboard--live-buffers))
    (with-current-buffer buf (ardentia-dashboard--render))))

(defun ardentia-dashboard--completed-refresh ()
  "Fetch the completed tasks for the current buffer asynchronously.
Does nothing while a fetch is running.  A failed fetch keeps the old list
and shows its error in the header line."
  (unless ardentia-dashboard--completed-refreshing
    (let ((buf (current-buffer)))
      (setq ardentia-dashboard--completed-refreshing t)
      (ardentia-completed--fetch
       (lambda (tasks err)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (setq ardentia-dashboard--completed-refreshing nil
                   ardentia-dashboard--completed-time (current-time)
                   ardentia-dashboard--completed-error err)
             (unless err (setq ardentia-dashboard--completed-tasks tasks))
             (when (derived-mode-p 'ardentia-dashboard-mode)
               (ardentia-dashboard--render)))))))
    (force-mode-line-update)))

(defun ardentia-dashboard-refresh-all ()
  "Refresh all dashboard buffers asynchronously."
  (ardentia-refresh #'ardentia-dashboard--render-all)
  ;; Not part of the join in `ardentia-refresh': a slow network must not
  ;; delay the task list.  Throttled; re-renders through the hook.
  (ardentia-usage-maybe-refresh)
  ;; The completed view has its own data: fetch it again.
  (dolist (buf (ardentia-dashboard--live-buffers))
    (with-current-buffer buf
      (when (eq (ardentia-dashboard--view-now) 'completed)
        (ardentia-dashboard--completed-refresh))))
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

(defun ardentia-dashboard-refresh ()
  "Refresh the dashboard asynchronously; Emacs stays responsive."
  (interactive)
  (ardentia-dashboard-refresh-all))

(defun ardentia-dashboard--switch-view (view)
  "Show VIEW in the current task list buffer.
Remember the row at point for the view left and restore the one
remembered for VIEW.  Sort keys start over.  The completed view fetches its
data."
  (let ((old (ardentia-dashboard--view-now)))
    (unless (eq old view)
      (setf (alist-get old ardentia-dashboard--view-points)
            (ardentia-dashboard--row-key (tabulated-list-get-id))))
    (setq ardentia-dashboard--view view
          tabulated-list-sort-key nil)
    (ardentia-dashboard--ensure-format view)
    (tabulated-list-init-header)
    (ardentia-dashboard--render (not (eq old view)))
    (when (eq view 'completed)
      (ardentia-dashboard--completed-refresh))))

(defun ardentia-dashboard--open (view &optional name)
  "Show the task list buffer NAME (default `*ardentia*') in VIEW.
Create it when needed.  The shared data is refreshed unless the command
was given from inside that buffer, where it is current already."
  (let* ((buf (get-buffer-create (or name "*ardentia*")))
         (inside (eq buf (current-buffer))))
    (with-current-buffer buf
      (unless (derived-mode-p 'ardentia-dashboard-mode)
        (ardentia-dashboard-mode))
      (ardentia-dashboard--switch-view view))
    ;; The mode only starts the timer for a new buffer; a buried one, or a
    ;; buffer that predates loading this code, gets it here.
    (ardentia-dashboard--refresh-timer-start)
    (pop-to-buffer-same-window buf)
    (unless inside
      (ardentia-dashboard-refresh-all))))

;;;###autoload
(defun ardentia-dashboard ()
  "Show the task list in its dashboard view: all open agent queues."
  (interactive)
  (ardentia-dashboard--open 'dashboard))

;;;###autoload
(defun ardentia-review-inbox ()
  "Show the task list in its review view: the tasks waiting for review."
  (interactive)
  (ardentia-dashboard--open 'review))

(defun ardentia-dashboard--cycle-view (step)
  "Switch to the view STEP places after the current one, cyclically."
  (let* ((views ardentia-dashboard--views)
         (pos (or (seq-position views (ardentia-dashboard--view-now)) 0)))
    (ardentia-dashboard--switch-view
     (nth (mod (+ pos step) (length views)) views))))

(defun ardentia-dashboard-next-view ()
  "Switch to the next view: dashboard, review, completed, and around."
  (interactive)
  (ardentia-dashboard--cycle-view 1))

(defun ardentia-dashboard-previous-view ()
  "Switch to the previous view: completed, review, dashboard, and around."
  (interactive)
  (ardentia-dashboard--cycle-view -1))

(defun ardentia-dashboard--require-task (&optional review)
  "Return the task at point; with REVIEW require the review queue.
The row of a running merge or of a group header is not a task and signals
a `user-error'."
  (when (ardentia-dashboard--group-at-point)
    (user-error "Not a task: a group of review tasks"))
  (let ((task (or (ardentia-dashboard--task-at-point) (user-error "No task here"))))
    (when (plist-get task :merge)
      (user-error "Not a task: a running merge"))
    (when (and review (not (eq (plist-get task :queue) 'review)))
      (user-error "Task is not waiting for review"))
    task))

(defun ardentia-dashboard--set-priority (action)
  "Apply priority ACTION to the task at point.
The asynchronous refresh re-renders the dashboard afterwards."
  (let ((task (ardentia-dashboard--require-task)))
    (ardentia-set-priority-1 (plist-get task :file) (plist-get task :id)
                             action (plist-get task :line)
                             (plist-get task :title)
                             (ardentia--task-lifecycle-tags task))))

(defun ardentia-dashboard-priority ()
  "Set the priority of the task at point, like org-agenda.
Prompt for a letter; SPC removes the cookie."
  (interactive)
  (ardentia-dashboard--require-task)
  (ardentia-dashboard--set-priority (ardentia--read-priority)))

(defun ardentia-dashboard-priority-up ()
  "Raise the priority of the task at point."
  (interactive)
  (ardentia-dashboard--set-priority 'up))

(defun ardentia-dashboard-priority-down ()
  "Lower the priority of the task at point."
  (interactive)
  (ardentia-dashboard--set-priority 'down))

(defun ardentia-dashboard-set-tags ()
  "Set the tags of the task at point, like `org-agenda-set-tags'.
Marks are not used; see `ardentia-dashboard-bulk-action'."
  (interactive)
  (ardentia-set-tags-1 (ardentia-dashboard--require-task)))

(defun ardentia-dashboard-set-model (&optional force)
  "Set or remove the model of one role on the task at point.
Ask for the role first, then the model.
With a prefix argument FORCE refetch the model list."
  (interactive "P")
  (let* ((task (ardentia-dashboard--require-task))
         (property (ardentia-model-read-role)))
    (ardentia-set-model-1
     task (ardentia-model-read (ardentia--task-prop task property) force)
     property)))

(defun ardentia-dashboard-schedule (&optional arg)
  "Schedule the task at point, like `org-agenda-schedule'.
With a prefix ARG remove the schedule."
  (interactive "P")
  (let ((task (ardentia-dashboard--require-task)))
    (ardentia-schedule-1 task arg (ardentia--read-planning-time 'scheduled arg))))

(defun ardentia-dashboard-deadline (&optional arg)
  "Set the deadline of the task at point, like `org-agenda-deadline'.
With a prefix ARG remove the deadline."
  (interactive "P")
  (let ((task (ardentia-dashboard--require-task)))
    (ardentia-deadline-1 task arg (ardentia--read-planning-time 'deadline arg))))

(defun ardentia-dashboard-todo (&optional arg)
  "Change the TODO state of the task at point, like `org-agenda-todo'.
ARG is the prefix argument of `org-todo'.  Org may ask for a note; the
file is saved once it is stored."
  (interactive "P")
  (ardentia-set-todo-1 (ardentia-dashboard--require-task) nil arg))

;;;; Marks, like org-agenda

(defun ardentia-dashboard--key-at-point (&optional strict)
  "Return the key of the task row at point, or nil.
A running merge and a group header are not tasks.  With STRICT signal a
`user-error' instead of returning nil."
  (let ((id (tabulated-list-get-id)))
    (cond ((null id) (and strict (user-error "No task here")))
          ((plist-get id :group)
           (and strict (user-error "Not a task: a group of review tasks")))
          ((plist-get id :merge)
           (and strict (user-error "Not a task: a running merge")))
          (t (ardentia--task-key id)))))

(defun ardentia-dashboard--put-mark (on)
  "Show the mark of the row at point when ON, else clear it."
  (tabulated-list-put-tag
   (if on (ardentia-dashboard--maybe-face ">" 'ardentia-mark) " "))
  (force-mode-line-update))

(defun ardentia-dashboard--print-entry (id cols)
  "Print the row ID with the columns COLS, with its mark if it has one.
This is the `tabulated-list-printer' of the task list, so every way of
printing the rows shows the marks."
  (tabulated-list-print-entry id cols)
  (when (and ardentia-dashboard--marked
             (not (ardentia-dashboard--non-task-p id))
             (member (ardentia--task-key id) ardentia-dashboard--marked))
    (save-excursion
      (forward-line -1)
      (ardentia-dashboard--put-mark t))))

(defun ardentia-dashboard--marks-prunable-p ()
  "Return non-nil when the list of the view is complete enough to prune marks.
After a failed fetch, or before the first one, the list is empty and
the marks must stay."
  (if (eq (ardentia-dashboard--view-now) 'completed)
      (not ardentia-dashboard--completed-error)
    (and ardentia--state (not (plist-get ardentia--state :error)))))

(defun ardentia-dashboard--prune-marks ()
  "Drop the marks of tasks the current entries do not list any more."
  (when (and ardentia-dashboard--marked
             (ardentia-dashboard--marks-prunable-p))
    (let ((keys (delq nil
                      (mapcar (lambda (id)
                                (and id (not (ardentia-dashboard--non-task-p id))
                                     (ardentia--task-key id)))
                              (append (mapcar #'car tabulated-list-entries)
                                      ardentia-dashboard--hidden)))))
      (setq ardentia-dashboard--marked
            (seq-filter (lambda (key) (member key keys))
                        ardentia-dashboard--marked)))))

(defun ardentia-dashboard--set-mark (on)
  "Mark (ON) or unmark the task row at point.  Signal if it is no task."
  (let ((key (ardentia-dashboard--key-at-point t)))
    (if on
        (unless (member key ardentia-dashboard--marked)
          (push key ardentia-dashboard--marked))
      (setq ardentia-dashboard--marked (delete key ardentia-dashboard--marked)))
    (ardentia-dashboard--put-mark on)))

(defun ardentia-dashboard--marked-p ()
  "Return non-nil when the row at point is marked."
  (let ((key (ardentia-dashboard--key-at-point)))
    (and key (member key ardentia-dashboard--marked) t)))

(defun ardentia-dashboard--map-task-rows (fn)
  "Call FN with point at the start of every task row, top to bottom."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (ardentia-dashboard--key-at-point) (funcall fn))
      (forward-line 1))))

(defun ardentia-dashboard-mark (&optional n)
  "Mark the task at point and move to the next line, like `org-agenda-bulk-mark'.
With a numeric prefix N mark that many tasks in succession."
  (interactive "p")
  (let ((n (max 1 (or n 1))) (i 0))
    (while (and (< i n) (or (= i 0) (ardentia-dashboard--key-at-point)))
      (ardentia-dashboard--set-mark t)
      (forward-line 1)
      (setq i (1+ i)))))

(defun ardentia-dashboard-unmark ()
  "Unmark the task at point and move to the next line, like org-agenda's `u'."
  (interactive)
  (when (ardentia-dashboard--key-at-point)
    (ardentia-dashboard--set-mark nil))
  (forward-line 1))

(defun ardentia-dashboard-unmark-all ()
  "Unmark all tasks, like `org-agenda-bulk-unmark-all'."
  (interactive)
  (let ((n (length ardentia-dashboard--marked)))
    (setq ardentia-dashboard--marked nil)
    (tabulated-list-clear-all-tags)
    (force-mode-line-update)
    (message "Unmarked %d task%s" n (if (= n 1) "" "s"))))

(defun ardentia-dashboard-mark-toggle ()
  "Toggle the mark of the task at point, like `org-agenda-bulk-toggle'."
  (interactive)
  (ardentia-dashboard--set-mark (not (ardentia-dashboard--marked-p))))

(defun ardentia-dashboard-mark-all ()
  "Mark all tasks shown, like `org-agenda-bulk-mark-all'.
Tasks hidden in a folded group are not marked."
  (interactive)
  (ardentia-dashboard--map-task-rows
   (lambda () (ardentia-dashboard--set-mark t)))
  (message "%d task%s marked" (length ardentia-dashboard--marked)
           (if (= (length ardentia-dashboard--marked) 1) "" "s")))

(defun ardentia-dashboard-mark-toggle-all ()
  "Toggle the mark of all tasks shown, like `org-agenda-bulk-toggle-all'."
  (interactive)
  (ardentia-dashboard--map-task-rows
   (lambda () (ardentia-dashboard--set-mark (not (ardentia-dashboard--marked-p)))))
  (message "%d task%s marked" (length ardentia-dashboard--marked)
           (if (= (length ardentia-dashboard--marked) 1) "" "s")))

(defun ardentia-dashboard-mark-regexp (regexp)
  "Mark the tasks whose title matches REGEXP, ignoring case.
Like `org-agenda-bulk-mark-regexp'."
  (interactive (list (read-string "Mark tasks with title matching regexp: ")))
  (when (condition-case nil (progn (string-match-p regexp "") nil)
          (invalid-regexp t))
    (user-error "Invalid regexp: %s" regexp))
  (let ((n 0))
    (ardentia-dashboard--map-task-rows
     (lambda ()
       (when (and (ardentia-dashboard--title-match-p
                   regexp (plist-get (tabulated-list-get-id) :title))
                  (not (ardentia-dashboard--marked-p)))
         (ardentia-dashboard--set-mark t)
         (setq n (1+ n)))))
    (message "%d new task%s marked" n (if (= n 1) "" "s"))))

;;;; Folding groups

(defun ardentia-dashboard--fold-key-at-point ()
  "Return the key of the group the row at point belongs to or heads, or nil.
On a header or on a task with nested tasks that is the row's own group;
on any other grouped task it is the group of its parent heading."
  (let ((id (tabulated-list-get-id)))
    (cond ((null id) nil)
          ((plist-get id :group) (plist-get id :key))
          ((plist-get id :merge) nil)
          (t (let ((own (ardentia--task-key id))
                   (parent (ardentia-dashboard--parent-key id)))
               (cond ((member own ardentia-dashboard--group-keys) own)
                     ((member parent ardentia-dashboard--group-keys) parent)))))))

(defun ardentia-dashboard--goto-group (key)
  "Move point to the header row of the group KEY, a header or a parent task."
  (or (ardentia-dashboard--goto-key (list :group key))
      (ardentia-dashboard--goto-key key)))

(defun ardentia-dashboard-fold-toggle ()
  "Fold or unfold the group at point.
On a task of a group fold the group and move point to its header."
  (interactive)
  (let ((key (or (ardentia-dashboard--fold-key-at-point)
                 (user-error "No group here"))))
    (if (member key ardentia-dashboard--folded)
        (setq ardentia-dashboard--folded (delete key ardentia-dashboard--folded))
      (ardentia-dashboard--goto-group key)
      (push key ardentia-dashboard--folded))
    (ardentia-dashboard--render)))

(defun ardentia-dashboard-fold-all ()
  "Fold all groups when any is open, else unfold all."
  (interactive)
  (let ((keys ardentia-dashboard--group-keys))
    (unless keys (user-error "No groups here"))
    (if (seq-some (lambda (k) (not (member k ardentia-dashboard--folded))) keys)
        (setq ardentia-dashboard--folded
              (seq-union keys ardentia-dashboard--folded))
      (setq ardentia-dashboard--folded
            (seq-difference ardentia-dashboard--folded keys)))
    (ardentia-dashboard--render)))

(defun ardentia-dashboard-sort (&optional n)
  "Sort by the column at point (or the Nth), then rebuild the groups.
Like `tabulated-list-sort' (N is passed on), but the sort scatters the
group header rows, so the entries are built again: flat while a sort key
is set, grouped when it is cleared."
  (interactive "P")
  (tabulated-list-sort n)
  (ardentia-dashboard--render))

(defun ardentia-dashboard-col-sort (event)
  "Sort by the column of the mouse click EVENT, then rebuild the groups.
See `ardentia-dashboard-sort'."
  (interactive "e")
  (tabulated-list-col-sort event)
  (with-current-buffer (window-buffer (posn-window (event-start event)))
    (ardentia-dashboard--render)))

;;;; Bulk action on the marked tasks

(defun ardentia-dashboard--marked-tasks ()
  "Return the marked tasks of the current entries.
Marked tasks hidden by a folded group count: the mark stays."
  (seq-filter (lambda (task)
                (and (not (ardentia-dashboard--non-task-p task))
                     (member (ardentia--task-key task)
                             ardentia-dashboard--marked)))
              (append (mapcar #'car tabulated-list-entries)
                      ardentia-dashboard--hidden)))

(defun ardentia-dashboard--task-buffers (tasks)
  "Return the buffers visiting the files of TASKS that can be used."
  (delq nil (mapcar (lambda (file) (ignore-errors (ardentia--task-buffer file)))
                    (delete-dups (mapcar (lambda (task) (plist-get task :file))
                                         tasks)))))

(defun ardentia-dashboard--tag-candidates (tasks)
  "Return the tags to complete over for TASKS.
They are the tags of the files involved, of the tasks, and the ones of
`org-tag-alist' and `org-tag-persistent-alist'."
  (let (tags)
    (dolist (buf (ardentia-dashboard--task-buffers tasks))
      (with-current-buffer buf
        (dolist (tag (org-with-wide-buffer (org-get-buffer-tags)))
          (push (if (consp tag) (car tag) tag) tags))))
    (dolist (task tasks) (setq tags (append (plist-get task :tags) tags)))
    (dolist (alist (list org-tag-alist org-tag-persistent-alist))
      (dolist (entry alist)
        (when (stringp (car-safe entry)) (push (car entry) tags))))
    (sort (delete-dups (copy-sequence tags)) #'string<)))

(defun ardentia-dashboard--list-tags (tasks)
  "Return the sorted, de-duplicated tags carried by TASKS.
Pure: no buffer is visited."
  (let (tags)
    (dolist (task tasks)
      (dolist (tag (plist-get task :tags))
        (when (stringp tag) (push tag tags))))
    (sort (delete-dups tags) #'string<)))

(defun ardentia-dashboard--read-tag (prompt tasks)
  "Read a tag with PROMPT, completing over the tags for TASKS."
  (let ((tag (string-trim
              (completing-read prompt
                               (ardentia-dashboard--tag-candidates tasks)))))
    (unless (ardentia--valid-tag-p tag)
      (user-error "Not a valid tag: \"%s\"" tag))
    tag))

(defun ardentia-dashboard--read-todo-state (tasks)
  "Read a TODO keyword for TASKS; the empty string removes the keyword."
  (let ((keywords nil))
    (dolist (buf (ardentia-dashboard--task-buffers tasks))
      (dolist (kw (buffer-local-value 'org-todo-keywords-1 buf))
        (unless (member kw keywords) (push kw keywords))))
    (completing-read "TODO state (empty removes it): " (nreverse keywords))))

(defun ardentia-dashboard--bulk-function (key arg tasks)
  "Return the function that applies bulk action KEY to a task.
Read what the action needs once, with ARG the prefix argument of the
command.  TASKS are the marked tasks.  Signal a `user-error' for an
unknown KEY."
  (pcase key
    (?+ (let ((tag (ardentia-dashboard--read-tag "Tag to add: " tasks)))
          (lambda (_task) (org-toggle-tag tag 'on))))
    (?- (let ((tag (ardentia-dashboard--read-tag "Tag to remove: " tasks)))
          (lambda (_task) (org-toggle-tag tag 'off))))
    ((or ?s ?d)
     (let* ((kind (if (eq key ?s) 'scheduled 'deadline))
            (time (ardentia--read-planning-time kind arg))
            (arg (and arg '(4))))
       (if (eq key ?s)
           (lambda (_task) (org-schedule arg time))
         (lambda (_task) (org-deadline arg time)))))
    (?t (let ((state (ardentia-dashboard--read-todo-state tasks)))
          (lambda (_task) (org-todo state))))
    (?, (let ((action (ardentia--read-priority)))
          (lambda (_task) (org-priority action))))
    (_ (user-error "Unknown bulk action `%s'" (single-key-description key)))))

(defun ardentia-dashboard--clear-marks ()
  "Unmark all tasks, unless `org-agenda-persistent-marks' is set."
  (unless (bound-and-true-p org-agenda-persistent-marks)
    (setq ardentia-dashboard--marked nil)
    (tabulated-list-clear-all-tags)))

(defun ardentia-dashboard--report-bulk (result &optional extra-skipped extra-reason)
  "Show the message for the bulk edit RESULT.
EXTRA-SKIPPED tasks were left out before the edit, for EXTRA-REASON."
  (let ((reason (or extra-reason (plist-get result :reason))))
    (message "Bulk: %d done, %d skipped%s"
             (plist-get result :done)
             (+ (plist-get result :skipped) (or extra-skipped 0))
             (if reason (format " (%s)" reason) ""))))

(defun ardentia-dashboard--bulk-accept (tasks)
  "Accept those of the marked TASKS that wait for review (set DONE).
Tasks that are not waiting for review, or are accepted already, are
skipped.  Ask once for confirmation; when declined change nothing and
keep the marks.  Afterwards clear the marks like the other bulk actions."
  (let* ((open (seq-filter (lambda (task)
                             (and (eq (plist-get task :queue) 'review)
                                  (not (plist-get task :done))))
                           tasks))
         (left-out (- (length tasks) (length open)))
         (reason (and (> left-out 0) "not waiting for review")))
    (cond
     ((null open)
      (ardentia-dashboard--report-bulk (list :done 0 :skipped 0)
                                       left-out reason))
     ((y-or-n-p (format "Accept %d task%s (set DONE)? "
                        (length open) (if (cdr open) "s" "")))
      (ardentia-dashboard--clear-marks)
      (ardentia-dashboard--report-bulk
       (ardentia--without-logging
         (ardentia-bulk-edit open (lambda (_task) (ardentia--set-done))))
       left-out reason)))))

(defun ardentia-dashboard-bulk-action (&optional arg)
  "Apply an action to all marked tasks, like `org-agenda-bulk-action'.
The key asked for is one of
  +  add a tag                 -  remove a tag
  s  schedule                  d  set the deadline
  t  change the TODO state     ,  set the priority
  a  accept (set DONE) the tasks waiting for review, after one confirmation
What the action needs is asked for once.  With a prefix ARG, `s' and `d'
remove the schedule or deadline.  A task that cannot be changed is
skipped; with `a' so is every marked task that is not waiting for review.
Org writes no log entries or notes in a bulk action.  Afterwards
all tasks are unmarked, unless `org-agenda-persistent-marks' is set;
a declined `a' keeps the marks."
  (interactive "P")
  (let ((tasks (ardentia-dashboard--marked-tasks)))
    (unless tasks (user-error "No task is marked"))
    (let ((key (read-char-exclusive
                (format "Bulk action on %d task%s: [+]tag [-]untag [s]chedule [d]eadline [t]odo [,]priority [a]ccept "
                        (length tasks) (if (cdr tasks) "s" "")))))
      (if (eq key ?a)
          (ardentia-dashboard--bulk-accept tasks)
        (let ((fn (ardentia-dashboard--bulk-function key arg tasks)))
          (ardentia-dashboard--clear-marks)
          (ardentia-dashboard--report-bulk
           (ardentia--without-logging
             (ardentia-bulk-edit tasks fn))))))))

(defun ardentia-dashboard-review ()
  "Open the review buffer of the task at point."
  (interactive)
  (ardentia-review-task (ardentia-dashboard--require-task t)))

;;;###autoload
(defun ardentia-dashboard-enter ()
  "Open the task at point: review buffer, or session viewer when running.
Other queues signal a `user-error'; `t' shows their last transcript.
On a running merge open its directory with dired.  On the header row of
a group fold or unfold the group."
  (interactive)
  (cond ((ardentia-dashboard--group-at-point)
         (ardentia-dashboard-fold-toggle))
        ((plist-get (ardentia-dashboard--task-at-point) :merge)
         (ardentia-dashboard--open-merge))
        (t (ardentia-dashboard--enter-task))))

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

(defun ardentia-dashboard--require-view (view what)
  "Signal a `user-error' unless the buffer shows VIEW; WHAT names the filter."
  (unless (eq (ardentia-dashboard--view-now) view)
    (user-error "%s only works in the %s view (%s)"
                what (alist-get view ardentia-dashboard--view-labels)
                (pcase view ('dashboard "1") ('review "2") (_ "3")))))

(defun ardentia-dashboard-toggle-later ()
  "Hide or show ready tasks that are not yet due in this task list.
A filter of the dashboard view."
  (interactive)
  (ardentia-dashboard--require-view 'dashboard "Later tasks")
  (setq ardentia-dashboard--show-later
        (not (ardentia-dashboard--show-later-p)))
  (ardentia-dashboard--render)
  (message "Later tasks: %s"
           (if (ardentia-dashboard--show-later-p) "shown" "hidden")))

(defun ardentia-dashboard-cycle-host ()
  "Cycle the host filter: all, the local host, each other host, all.
The hosts come from the hosts file.  A task counts for the host that
claimed it, else for its NAME_only tag, else for the default host."
  (interactive)
  (setq ardentia-dashboard--filter-host
        (ardentia-dashboard--next-host
         ardentia-dashboard--filter-host
         (plist-get ardentia--state :host)
         (plist-get (ardentia-hosts) :hosts)))
  (ardentia-dashboard--render)
  (message "Host filter: %s" (or ardentia-dashboard--filter-host "all")))

(defun ardentia-dashboard-filter-title (regexp)
  "Show only the tasks whose title matches REGEXP, ignoring case.
An empty REGEXP clears the filter."
  (interactive
   (list (read-string "Title regexp (empty clears): "
                      ardentia-dashboard--filter-title)))
  (when (and (not (string-empty-p regexp))
             (condition-case nil (progn (string-match-p regexp "") nil)
               (invalid-regexp t)))
    (user-error "Invalid regexp: %s" regexp))
  (setq ardentia-dashboard--filter-title
        (if (string-empty-p regexp) nil regexp))
  (ardentia-dashboard--render)
  (message "Title filter: %s"
           (if ardentia-dashboard--filter-title
               (format "/%s/" ardentia-dashboard--filter-title) "none")))

(defun ardentia-dashboard-filter-tags (specs)
  "Show only the tasks that match the tag filter SPECS.
SPECS is a list of strings: a tag (or +tag) the task must carry, or
-tag for one it must not carry; all must hold.  Matching is exact and
case-sensitive.  An empty list clears the filter.  Running merges have
no tags, so they are hidden while a tag is required and stay when SPECS
only exclude."
  (interactive
   (list (completing-read-multiple
          "Tags (tag, -tag; empty clears): "
          (let ((tags (ardentia-dashboard--list-tags
                       (append (plist-get ardentia--state :tasks)
                               ardentia-dashboard--completed-tasks))))
            (append tags (mapcar (lambda (tag) (concat "-" tag)) tags)))
          nil nil
          (and ardentia-dashboard--filter-tags
               (string-join ardentia-dashboard--filter-tags ",")))))
  (setq ardentia-dashboard--filter-tags
        (ardentia-dashboard--normalize-tag-specs specs))
  (ardentia-dashboard--render)
  (message "Tag filter: %s"
           (if ardentia-dashboard--filter-tags
               (string-join (mapcar (lambda (spec)
                                      (if (string-prefix-p "-" spec)
                                          spec (concat "+" spec)))
                                    ardentia-dashboard--filter-tags)
                            " ")
             "none")))

(defun ardentia-dashboard-cycle-period ()
  "Cycle the period filter: all, last 30 days, last 7 days, today.
A filter of the completed view."
  (interactive)
  (ardentia-dashboard--require-view 'completed "The period filter")
  (setq ardentia-dashboard--period
        (ardentia-completed--next-period (ardentia-dashboard--period-now)))
  (ardentia-dashboard--render)
  (message "Period: %s"
           (ardentia-completed--period-label (ardentia-dashboard--period-now))))

(defun ardentia-dashboard-cycle-state ()
  "Cycle the state filter: all, waiting for review, accepted.
A filter of the completed view."
  (interactive)
  (ardentia-dashboard--require-view 'completed "The state filter")
  (setq ardentia-dashboard--state-filter
        (ardentia-completed--next-state ardentia-dashboard--state-filter))
  (ardentia-dashboard--render)
  (message "State: %s"
           (ardentia-completed--state-label ardentia-dashboard--state-filter)))

(defun ardentia-dashboard-clear-filters ()
  "Reset every filter of this task list to its default."
  (interactive)
  (setq ardentia-dashboard--filter-host nil
        ardentia-dashboard--filter-title nil
        ardentia-dashboard--filter-tags nil
        ardentia-dashboard--show-later 'default
        ardentia-dashboard--period 'default
        ardentia-dashboard--state-filter 'all)
  (ardentia-dashboard--render)
  (message "Filters cleared"))

(defun ardentia-dashboard--menu-buffer ()
  "Return the task list buffer a filter menu entry refers to, or nil.
Transient evaluates descriptions and predicates in varying buffers."
  (seq-find (lambda (buf)
              (and (buffer-live-p buf)
                   (with-current-buffer buf
                     (derived-mode-p 'ardentia-dashboard-mode))))
            (list (current-buffer)
                  (bound-and-true-p transient--original-buffer)
                  (bound-and-true-p transient--shadowed-buffer))))

(defmacro ardentia-dashboard--in-menu-buffer (&rest body)
  "Run BODY in the task list buffer the filter menu belongs to."
  (declare (indent 0))
  `(if-let* ((buf (ardentia-dashboard--menu-buffer)))
       (with-current-buffer buf ,@body)
     nil))

(defun ardentia-dashboard--menu-text (label value-fn)
  "Return \"LABEL: VALUE\" for the filter menu, VALUE from VALUE-FN.
VALUE-FN runs in the task list buffer the menu belongs to."
  (format "%s: %s" label
          (or (ardentia-dashboard--in-menu-buffer (funcall value-fn)) "?")))

(defun ardentia-dashboard--menu-inapt-p (view)
  "Return non-nil unless the buffer of the filter menu shows VIEW."
  (not (ardentia-dashboard--in-menu-buffer
         (eq (ardentia-dashboard--view-now) view))))

(transient-define-prefix ardentia-dashboard-filter-menu ()
  "Filters of the ardentia task list, each with its current value.
The keys stay open; entries that do not apply in the current view are
greyed out."
  [["Everywhere"
    ("h" ardentia-dashboard-cycle-host
     :description (lambda ()
                    (ardentia-dashboard--menu-text
                     "Host" (lambda () (or ardentia-dashboard--filter-host
                                           "all"))))
     :transient t)
    ("/" ardentia-dashboard-filter-title
     :description (lambda ()
                    (ardentia-dashboard--menu-text
                     "Title" (lambda ()
                               (if ardentia-dashboard--filter-title
                                   (format "/%s/" ardentia-dashboard--filter-title)
                                 "none"))))
     :transient t)
    ("\\" ardentia-dashboard-filter-tags
     :description (lambda ()
                    (ardentia-dashboard--menu-text
                     "Tags" (lambda ()
                              (if ardentia-dashboard--filter-tags
                                  (substring (ardentia-dashboard--tags-text
                                              ardentia-dashboard--filter-tags)
                                             5)
                                "none"))))
     :transient t)]
   ["Dashboard view"
    ("l" ardentia-dashboard-toggle-later
     :description (lambda ()
                    (ardentia-dashboard--menu-text
                     "Later tasks" (lambda ()
                                     (if (ardentia-dashboard--show-later-p)
                                         "shown" "hidden"))))
     :inapt-if (lambda () (ardentia-dashboard--menu-inapt-p 'dashboard))
     :transient t)]
   ["Completed view"
    ("P" ardentia-dashboard-cycle-period
     :description (lambda ()
                    (ardentia-dashboard--menu-text
                     "Period" (lambda ()
                                (ardentia-completed--period-label
                                 (ardentia-dashboard--period-now)))))
     :inapt-if (lambda () (ardentia-dashboard--menu-inapt-p 'completed))
     :transient t)
    ("w" ardentia-dashboard-cycle-state
     :description (lambda ()
                    (ardentia-dashboard--menu-text
                     "State" (lambda ()
                               (ardentia-completed--state-label
                                ardentia-dashboard--state-filter))))
     :inapt-if (lambda () (ardentia-dashboard--menu-inapt-p 'completed))
     :transient t)]
   ["Reset"
    ("c" "Clear all filters" ardentia-dashboard-clear-filters :transient t)]])

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

(defun ardentia-dashboard--require-open-review-task ()
  "Return the task at point; require it to wait for review, not accepted.
An accepted (done) task of the completed view is refused."
  (let ((task (ardentia-dashboard--require-task t)))
    (when (plist-get task :done)
      (user-error "Task is already accepted"))
    task))

(defun ardentia-dashboard-accept ()
  "Accept the review task at point.
When tasks are marked, accept the marked tasks instead, like
`ardentia-dashboard-bulk-action' with `a'."
  (interactive)
  (if-let* ((tasks (ardentia-dashboard--marked-tasks)))
      (ardentia-dashboard--bulk-accept tasks)
    (ardentia-accept (ardentia-dashboard--require-open-review-task))))

(defun ardentia-dashboard-send-back ()
  "Send the review task at point back to the agents."
  (interactive)
  (ardentia-send-back (ardentia-dashboard--require-open-review-task)))

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
