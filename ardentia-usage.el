;;; ardentia-usage.el --- Claude usage limits for the ardentia dashboard -*- lexical-binding: t -*-

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

;; Shows the current usage limits (5 hour session, weekly, per-model
;; weekly, extra usage) and the runner's own limit pause in the dashboard
;; header line.
;;
;; The data comes from an undocumented endpoint (the one Claude Code's
;; /usage uses) and may change or vanish.  The OAuth token is read from
;; `ardentia-usage-credentials-file', kept in a local variable, sent to
;; `ardentia-usage-url' only and handed to curl on its standard input, so
;; it never appears in a process argument list.  A failed refresh never
;; signals an error; the header line shows "usage: unavailable (REASON)".
;;
;; Fetches are throttled to one per `ardentia-usage-min-interval'; in
;; between the cached value is shown.  Nothing here blocks Emacs.

;;; Code:

(require 'json)
(require 'seq)
(require 'subr-x)
(require 'time-date)
(require 'iso8601)

(defgroup ardentia-usage nil
  "Usage limits in the ardentia dashboard."
  :group 'ardentia)

(defcustom ardentia-usage-enabled t
  "Non-nil means the dashboard fetches and shows the usage limits."
  :type 'boolean
  :group 'ardentia)

(defcustom ardentia-usage-credentials-file "~/.claude/.credentials.json"
  "JSON file holding the OAuth token under `claudeAiOauth.accessToken'."
  :type 'file
  :group 'ardentia)

(defcustom ardentia-usage-url "https://api.anthropic.com/api/oauth/usage"
  "Endpoint that returns the usage limits (undocumented)."
  :type 'string
  :group 'ardentia)

(defcustom ardentia-usage-min-interval 300
  "Minimum seconds between two fetches of the usage endpoint.
The dashboard refreshes more often; in between the cached value is shown."
  :type 'integer
  :group 'ardentia)

(defcustom ardentia-usage-limit-state-file
  (expand-file-name "process-aiready/limit-until"
                    (or (getenv "XDG_STATE_HOME") "~/.local/state"))
  "File the runner writes when it hit the session limit, or nil.
One line: UNTIL_EPOCH, a tab, a text.  Absent or expired means no pause."
  :type '(choice file (const :tag "None" nil))
  :group 'ardentia)

(defcustom ardentia-usage-warn-percent 80
  "Usage percentage from which a limit is shown in the `warning' face."
  :type 'number
  :group 'ardentia)

(defcustom ardentia-usage-hide-idle-scoped t
  "Non-nil means per-model weekly limits at 0% are left out of the line."
  :type 'boolean
  :group 'ardentia)

(defvar ardentia-usage-updated-hook nil
  "Hook run after a fetch finished, successfully or not.")

(defvar ardentia-usage--state nil
  "Plist of the last result: :limits, :extra, :time, :error.")

(defvar ardentia-usage--last-attempt nil
  "Time of the last started fetch, or nil.")

(defvar ardentia-usage--process nil
  "The running fetch process, or nil.")

(defvar ardentia-usage--pause nil
  "Cached runner pause as (EPOCH . TEXT), or nil.")

(defvar ardentia-usage--zone nil
  "Time zone for displayed times; nil means local.  Bound by tests.")

;;;; Parsing

(defun ardentia-usage--get (obj &rest keys)
  "Return the value in the alist OBJ along KEYS, or nil if anything is off."
  (while (and keys obj (listp obj))
    (setq obj (cdr (assq (pop keys) obj))))
  (and (null keys) obj))

(defun ardentia-usage--parse-time (string)
  "Return the time of the ISO 8601 STRING, or nil."
  (when (stringp string)
    (ignore-errors
      (let ((cleaned (replace-regexp-in-string
                      "\\(:[0-9][0-9]\\)\\.[0-9]+" "\\1" string)))
        (encode-time (iso8601-parse cleaned))))))

(defun ardentia-usage--label (kind scope)
  "Return the display label for limit KIND with SCOPE (an alist or nil)."
  (let ((name (or (ardentia-usage--get scope 'model 'display_name)
                  (let ((s (ardentia-usage--get scope 'surface)))
                    (and (stringp s) s)))))
    (pcase kind
      ("session" "5h")
      ("weekly_all" "week")
      ("weekly_scoped" (if (stringp name) (concat name " week") "scoped week"))
      ((pred stringp) (replace-regexp-in-string "_" " " kind))
      (_ "limit"))))

(defun ardentia-usage--entry (label percent resets severity active scoped)
  "Return a limit plist from LABEL, PERCENT, RESETS, SEVERITY, ACTIVE, SCOPED."
  (list :label label
        :percent (and (numberp percent) percent)
        :resets-at (ardentia-usage--parse-time resets)
        :severity (and (stringp severity) severity)
        :active (and active t)
        :scoped scoped))

(defun ardentia-usage--parse-limits (data)
  "Return the list of limit plists from the parsed JSON DATA (an alist)."
  (let ((limits (ardentia-usage--get data 'limits)) result)
    (if (and (consp limits) (consp (car limits)))
        (dolist (l limits)
          (when (and (consp l) (numberp (alist-get 'percent l)))
            (let ((kind (alist-get 'kind l)))
              (push (ardentia-usage--entry
                     (ardentia-usage--label kind (alist-get 'scope l))
                     (alist-get 'percent l) (alist-get 'resets_at l)
                     (alist-get 'severity l) (alist-get 'is_active l)
                     (equal kind "weekly_scoped"))
                    result))))
      (dolist (spec '((five_hour . "5h") (seven_day . "week")))
        (let ((o (ardentia-usage--get data (car spec))))
          (when (and (consp o) (numberp (alist-get 'utilization o)))
            (push (ardentia-usage--entry
                   (cdr spec) (alist-get 'utilization o)
                   (alist-get 'resets_at o) nil nil nil)
                  result)))))
    (nreverse result)))

(defun ardentia-usage--parse-extra (data)
  "Return a plist (:used :limit :currency) when extra usage is enabled in DATA."
  (let ((e (ardentia-usage--get data 'extra_usage)))
    (when (and (consp e) (eq (alist-get 'is_enabled e) t))
      (list :used (and (numberp (alist-get 'used_credits e))
                       (alist-get 'used_credits e))
            :limit (and (numberp (alist-get 'monthly_limit e))
                        (alist-get 'monthly_limit e))
            :currency (and (stringp (alist-get 'currency e))
                           (alist-get 'currency e))))))

(defun ardentia-usage--parse (json)
  "Parse the JSON string into a plist (:limits :extra), or nil if unusable."
  (let ((data (ignore-errors
                (json-parse-string json :object-type 'alist
                                   :array-type 'list
                                   :null-object nil :false-object nil))))
    (when (and (consp data) (consp (car data)))
      (let ((limits (ardentia-usage--parse-limits data)))
        (when limits
          (list :limits limits :extra (ardentia-usage--parse-extra data)))))))

;;;; Formatting

(defun ardentia-usage--format-time (time &optional now)
  "Format TIME as HH:MM when on the same day as NOW, else as Ddd HH:MM."
  (let ((zone ardentia-usage--zone)
        (now (or now (current-time))))
    (format-time-string
     (if (equal (format-time-string "%F" time zone)
                (format-time-string "%F" now zone))
         "%H:%M" "%a %H:%M")
     time zone)))

(defun ardentia-usage--face (percent severity)
  "Return the face for PERCENT and SEVERITY, or nil for none."
  (cond ((or (and percent (>= percent 100))
             (member severity '("critical" "exceeded")))
         'error)
        ((or (and percent (>= percent ardentia-usage-warn-percent))
             (and severity (not (equal severity "normal"))))
         'warning)))

(defun ardentia-usage--format-limit (limit &optional now)
  "Return the string of one LIMIT plist, relative to NOW."
  (let* ((percent (plist-get limit :percent))
         (resets (plist-get limit :resets-at))
         (text (format "%s %d%%%s" (plist-get limit :label) (round percent)
                       (if resets
                           (format " (resets %s)"
                                   (ardentia-usage--format-time resets now))
                         "")))
         (face (ardentia-usage--face percent (plist-get limit :severity))))
    (if face (propertize text 'face face) text)))

(defun ardentia-usage--format-extra (extra)
  "Return the string for the EXTRA usage plist, or nil."
  (when (and extra (plist-get extra :used))
    (format "extra %.2f%s%s" (plist-get extra :used)
            (if (plist-get extra :limit)
                (format "/%.2f" (plist-get extra :limit)) "")
            (if (plist-get extra :currency)
                (concat " " (plist-get extra :currency)) ""))))

(defun ardentia-usage--format-age (seconds)
  "Return SECONDS as a short age such as 12m or 2h."
  (if (< seconds 3600)
      (format "%dm" (/ seconds 60))
    (format "%dh" (/ seconds 3600))))

(defun ardentia-usage--format (state &optional now)
  "Return the compact usage text for STATE relative to NOW, or nil."
  (let ((now (or now (current-time))))
    (cond
     ((plist-get state :error)
      (propertize (format "usage: unavailable (%s)" (plist-get state :error))
                  'face 'warning))
     ((plist-get state :limits)
      (let* ((age (and (plist-get state :time)
                       (floor (float-time (time-subtract
                                           now (plist-get state :time))))))
             (parts
              (append
               (mapcar (lambda (l) (ardentia-usage--format-limit l now))
                       (seq-remove
                        (lambda (l)
                          (and ardentia-usage-hide-idle-scoped
                               (plist-get l :scoped)
                               (<= (plist-get l :percent) 0)))
                        (plist-get state :limits)))
               (delq nil (list (ardentia-usage--format-extra
                                (plist-get state :extra)))))))
        (concat (string-join parts " | ")
                (if (and age (> age (* 2 ardentia-usage-min-interval)))
                    (format " (stale %s)" (ardentia-usage--format-age age))
                  "")))))))

;;;; Runner pause

(defun ardentia-usage--read-pause (&optional file)
  "Return (EPOCH . TEXT) from the limit state FILE, or nil.
Nil when the file is absent or malformed; expiry is not checked here."
  (let ((file (or file ardentia-usage-limit-state-file)))
    (when (and file (file-readable-p file))
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents file nil 0 1000)
          (goto-char (point-min))
          (when (looking-at "\\([0-9]+\\)\\(?:\t\\(.*\\)\\)?$")
            (cons (string-to-number (match-string 1))
                  (or (match-string 2) ""))))))))

(defun ardentia-usage--pause-text (pause &optional now)
  "Return the pause line for PAUSE (EPOCH . TEXT) at NOW, or nil if expired."
  (let ((now (or now (current-time))))
    (when (and pause (time-less-p now (seconds-to-time (car pause))))
      (propertize (format "runner paused until %s"
                          (ardentia-usage--format-time
                           (seconds-to-time (car pause)) now))
                  'face 'warning))))

;;;; Header line

(defun ardentia-usage-header-string (&optional now)
  "Return the header line part for usage and runner pause, or an empty string.
Uses cached data only; it does no I/O, so it is cheap to call on redisplay."
  (let ((usage (and ardentia-usage-enabled
                    (ardentia-usage--format ardentia-usage--state now)))
        (pause (ardentia-usage--pause-text ardentia-usage--pause now)))
    (concat (if usage (concat " | " usage) "")
            (if pause (concat " | " pause) ""))))

;;;; Fetching

(defun ardentia-usage--token ()
  "Return the OAuth token from the credentials file, or nil."
  (let ((file (expand-file-name ardentia-usage-credentials-file)))
    (when (file-readable-p file)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (let ((token (ardentia-usage--get
                        (json-parse-buffer :object-type 'alist
                                           :null-object nil :false-object nil)
                        'claudeAiOauth 'accessToken)))
            (and (stringp token) (not (string-empty-p token)) token)))))))

(defun ardentia-usage--result (output exit-status)
  "Return the new state plist from curl OUTPUT and EXIT-STATUS."
  (let ((now (current-time)))
    (cond
     ((/= exit-status 0)
      (list :time now :error (pcase exit-status
                               ((or 6 7) "no network")
                               (28 "timeout")
                               (_ (format "curl failed (%d)" exit-status)))))
     ((not (string-match "\n\\([0-9]+\\)\\'" output))
      (list :time now :error "no HTTP status"))
     (t
      (let ((code (string-to-number (match-string 1 output)))
            (body (substring output 0 (match-beginning 0))))
        (cond
         ((= code 401)
          (list :time now :error "token expired, run claude once to refresh login"))
         ((/= code 200) (list :time now :error (format "HTTP %d" code)))
         (t (let ((parsed (ardentia-usage--parse body)))
              (if parsed
                  (append (list :time now) parsed)
                (list :time now :error "unexpected response"))))))))))

(defun ardentia-usage--finish (state)
  "Store STATE, clear the process and run the update hook."
  (setq ardentia-usage--state state
        ardentia-usage--process nil)
  (run-hooks 'ardentia-usage-updated-hook))

(defun ardentia-usage--fetch ()
  "Start an asynchronous fetch of the usage; never signal an error."
  (let ((token (ardentia-usage--token))
        (curl (executable-find "curl")))
    (cond
     ((not curl)
      (ardentia-usage--finish (list :time (current-time) :error "curl missing")))
     ((not token)
      (ardentia-usage--finish (list :time (current-time) :error "no credentials")))
     (t
      (condition-case nil
          (let* ((buf (generate-new-buffer " *ardentia-usage*"))
                 (proc (make-process
                        :name "ardentia-usage"
                        :buffer buf
                        :command (list curl "-sS" "-m" "15" "-H" "@-"
                                       "-w" "\n%{http_code}"
                                       ardentia-usage-url)
                        :connection-type 'pipe
                        :noquery t
                        :sentinel #'ardentia-usage--sentinel)))
            (setq ardentia-usage--process proc)
            (process-send-string
             proc (format "Authorization: Bearer %s\nanthropic-beta: oauth-2025-04-20\n"
                          token))
            (process-send-eof proc))
        (error
         (ardentia-usage--finish
          (list :time (current-time) :error "cannot start curl"))))))))

(defun ardentia-usage--sentinel (proc _event)
  "Process sentinel: collect the output of PROC when it has finished."
  (unless (process-live-p proc)
    (let ((buf (process-buffer proc))
          (state nil))
      (setq state
            (condition-case nil
                (ardentia-usage--result
                 (if (buffer-live-p buf)
                     (with-current-buffer buf (buffer-string))
                   "")
                 (process-exit-status proc))
              (error (list :time (current-time) :error "internal error"))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (ardentia-usage--finish state))))

(defun ardentia-usage-maybe-refresh (&optional force)
  "Refresh the pause cache and start a usage fetch unless throttled.
With FORCE ignore `ardentia-usage-min-interval'.  Never signals an error."
  (condition-case nil
      (progn
        (setq ardentia-usage--pause (ardentia-usage--read-pause))
        (when (and ardentia-usage-enabled
                   (not (process-live-p ardentia-usage--process))
                   (or force
                       (null ardentia-usage--last-attempt)
                       (>= (float-time (time-subtract
                                        (current-time)
                                        ardentia-usage--last-attempt))
                           ardentia-usage-min-interval)))
          (setq ardentia-usage--last-attempt (current-time))
          (ardentia-usage--fetch)))
    (error nil)))

;;;###autoload
(defun ardentia-usage-refresh ()
  "Fetch the usage now, ignoring the throttle."
  (interactive)
  (ardentia-usage-maybe-refresh t))

(provide 'ardentia-usage)
;;; ardentia-usage.el ends here
