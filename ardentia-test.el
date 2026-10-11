;;; ardentia-test.el --- ERT tests for ardentia -*- lexical-binding: t -*-

;; Copyright (C) 2026 Karsten Klöss

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

;; Run with ./run-tests.  The tests never touch the real org files; the
;; asynchronous fetch is tested against a fake `org-tool' script.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ardentia)

;;;; Helpers

(defconst ardentia-test--json
  "[{\"file\":\"/o/a.org\",\"line\":10,\"level\":1,\"todo\":\"TODO\",\"priority\":\"null\",
\"title\":\"Review me\",\"tags\":[\"refile\",\"humanreview\"],\"id\":\"id-1\",
\"scheduled\":\"null\",\"deadline\":\"null\",\"effort\":\"null\",\"created\":\"2026-10-10 19:40\",
\"done\":false,\"open\":true,
\"properties\":{\"CATEGORY\":\"a\",\"AI_STATUS\":\"blocked\",\"AI_SESSION\":\"20261010-1953\",\"AI_ATTEMPTS\":\"3\",\"ID\":\"id-1\"}},
{\"file\":\"/o/a.org\",\"line\":20,\"level\":1,\"todo\":\"TODO\",\"priority\":\"null\",
\"title\":\"Finished\",\"tags\":[\"humanreview\"],\"id\":\"id-2\",\"scheduled\":\"null\",
\"deadline\":\"null\",\"effort\":\"null\",\"created\":\"null\",\"done\":true,\"open\":false,
\"properties\":{}},
{\"file\":\"/o/b.org\",\"line\":5,\"level\":1,\"todo\":\"null\",\"priority\":\"null\",
\"title\":\"Plain\",\"tags\":[\"aiready\",\"laptop_only\"],\"id\":\"null\",
\"scheduled\":\"null\",\"deadline\":\"null\",\"effort\":\"null\",\"created\":\"2026-10-09 08:00\",
\"done\":false,\"open\":true,\"properties\":{}}]"
  "Canned `org-tool find-tagged' output.")

(defun ardentia-test--task (&rest plist)
  "Build a task plist from PLIST with defaults; :props is an alist."
  (append plist (list :title "T" :file "/o/x.org" :tags nil :props nil)))

(defun ardentia-test--t (stamp)
  "Parse STAMP into a time value."
  (ardentia--parse-stamp stamp))

(defmacro ardentia-test--with-org-file (var text &rest body)
  "Run BODY with VAR bound to a temporary org file containing TEXT.
Org settings are fixed to the values of the human's setup."
  (declare (indent 2))
  `(let* ((,var (make-temp-file "ardentia-test-" nil ".org" ,text))
          (org-todo-keywords
           '((sequence "TODO(t!/!)" "WAIT(w!/!)" "|" "DONE(d!/!)" "CANC(c@/!)")))
          (org-log-into-drawer t)
          (org-log-done 'time)
          (org-adapt-indentation nil)
          (org-tags-column 0)
          (org-startup-folded nil)
          (org-mode-hook nil)
          (message-log-max nil))
     (unwind-protect (progn ,@body)
       (let ((b (find-buffer-visiting ,var)))
         (when b (with-current-buffer b (set-buffer-modified-p nil))
               (kill-buffer b)))
       (delete-file ,var))))

(defun ardentia-test--file-text (file)
  "Return the text of FILE with time stamps replaced by STAMP."
  (replace-regexp-in-string
   "\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [^]]*\\]" "[STAMP]"
   (with-temp-buffer (insert-file-contents file) (buffer-string))))

(defun ardentia-test--wait (pred)
  "Wait until PRED returns non-nil, at most 10 seconds."
  (let ((end (+ (float-time) 10)))
    (while (and (not (funcall pred)) (< (float-time) end))
      (accept-process-output nil 0.05))))

(defconst ardentia-test--org
  "* TODO Task one :refile:humanreview:
:PROPERTIES:
:ID:       id-one
:AI_STATUS: completed
:END:
:LOGBOOK:
- AI-Lauf [2026-10-10 Sat 19:53] completed: Headline \\\\
  - Recommendation: do it
  - == COMMITS ==
    - abc1234 something
:END:
Original task text
** Result A
:PROPERTIES:
:ID:       id-a
:END:
** TODO Decide this :humanreview:
** TODO Do that :humanreview:
** DONE Old :humanreview:
* TODO Other :refile:
"
  "Org fixture with an existing LOGBOOK drawer.")

;;;; Parsing

(ert-deftest ardentia-test-parse-json-null-strings-and-done ()
  (let* ((tasks (ardentia--tasks-from-json (ardentia--parse-json ardentia-test--json))))
    (should (= (length tasks) 2))
    (let ((a (nth 0 tasks)) (b (nth 1 tasks)))
      (should (equal (plist-get a :id) "id-1"))
      (should (eq (plist-get a :queue) 'review))
      (should (null (plist-get a :scheduled)))
      (should (equal (ardentia--task-prop a "AI_STATUS") "blocked"))
      (should (null (plist-get b :todo)))
      (should (null (plist-get b :id)))
      (should (eq (plist-get b :queue) 'ready))
      (should (null (ardentia--task-prop b "AI_STATUS"))))))

(ert-deftest ardentia-test-parse-json-empty ()
  (should (null (ardentia--parse-json "")))
  (should (null (ardentia--parse-json "[]\n"))))

(ert-deftest ardentia-test-dedupe ()
  (let* ((a (ardentia-test--task :id "1" :file "/f"))
         (b (ardentia-test--task :id "1" :file "/f"))
         (c (ardentia-test--task :id "1" :file "/g")))
    (should (= (length (ardentia--dedupe (list a b c))) 2))))

(ert-deftest ardentia-test-queue-from-tags ()
  (should (eq (ardentia--tag-queue '("aiready" "humanreview")) 'review))
  (should (eq (ardentia--tag-queue '("aiready" "ai_in_progress")) 'running))
  (should (eq (ardentia--tag-queue '("aiready")) 'ready))
  (should (null (ardentia--tag-queue '("refile")))))

;;;; Sorting

(ert-deftest ardentia-test-sort-queue ()
  (let* ((mk (lambda (title queue status session)
               (ardentia-test--task
                :title title :queue queue
                :props (list (cons "AI_STATUS" status) (cons "AI_SESSION" session)))))
         (tasks (list (funcall mk "ready" 'ready nil nil)
                      (funcall mk "running" 'running nil nil)
                      (funcall mk "done-new" 'review "completed" "20261010-1000")
                      (funcall mk "done-old" 'review "completed" "20261009-1000")
                      (funcall mk "partial" 'review "partial" "20261010-1200")
                      (funcall mk "blocked" 'review "blocked" "20261010-1300")))
         (sorted (mapcar (lambda (x) (plist-get x :title))
                         (ardentia--sort-tasks tasks))))
    (should (equal sorted '("blocked" "partial" "done-old" "done-new"
                            "running" "ready")))))

;;;; Later (scheduled, not yet due)

(defun ardentia-test--later (scheduled &optional queue)
  "Return non-nil when a QUEUE task SCHEDULED is later at 2026-10-10 12:00."
  (and (ardentia--task-later-p
        (ardentia-test--task :queue (or queue 'ready) :scheduled scheduled)
        (ardentia-test--t "20261010-1200"))
       t))

(ert-deftest ardentia-test-later-date-rules ()
  (should (ardentia-test--later "<2026-10-12 Mon>"))
  (should-not (ardentia-test--later "<2026-10-10 Sat>"))
  (should-not (ardentia-test--later "<2026-10-09 Fri>"))
  (should-not (ardentia-test--later nil))
  (should-not (ardentia-test--later "garbage")))

(ert-deftest ardentia-test-later-time-rules ()
  (should (ardentia-test--later "<2026-10-10 Sat 18:30>"))
  (should-not (ardentia-test--later "<2026-10-10 Sat 11:59>"))
  (should-not (ardentia-test--later "<2026-10-10 Sat 12:00>")))

(ert-deftest ardentia-test-later-range-repeater ()
  (should (ardentia-test--later "<2026-10-10 Sat 13:00-14:00>"))
  (should-not (ardentia-test--later "<2026-10-10 Sat 09:00-18:00>"))
  (should (ardentia-test--later "<2026-10-10 Sat 18:30 +1w>"))
  (should-not (ardentia-test--later "<2026-10-10 Sat +1w>"))
  (should (ardentia-test--later "<2026-10-12 Mon +1w -2d>")))

(ert-deftest ardentia-test-later-impossible-time ()
  (should-not (ardentia-test--later "<2026-10-10 Sat 25:00>"))
  (should-not (ardentia-test--later "<2026-10-10 Sat 12:75>"))
  (should (ardentia-test--later "<2026-10-11 Sun 25:00>")))

(ert-deftest ardentia-test-later-only-ready ()
  (should-not (ardentia-test--later "<2026-10-12 Mon>" 'review))
  (should-not (ardentia-test--later "<2026-10-12 Mon>" 'running)))

(ert-deftest ardentia-test-later-due-time ()
  (should (null (ardentia--task-due-time (ardentia-test--task))))
  (should (equal (format-time-string
                  "%F %R" (ardentia--task-due-time
                           (ardentia-test--task :scheduled "<2026-10-12 Mon 09:00-11:00>")))
                 "2026-10-12 09:00"))
  (should (equal (format-time-string
                  "%F %R" (ardentia--task-due-time
                           (ardentia-test--task :scheduled "<2026-10-12 Mon 24:00>")))
                 "2026-10-12 00:00")))

(ert-deftest ardentia-test-later-sort ()
  (let* ((now (ardentia-test--t "20261010-1200"))
         (mk (lambda (title sched &optional queue)
               (ardentia-test--task :title title :queue (or queue 'ready) :scheduled sched
                                    :created "20200101-0000")))
         (tasks (list (funcall mk "far" "<2026-10-20 Tue>")
                      (funcall mk "near" "<2026-10-11 Sun 08:00>")
                      (funcall mk "ready" nil)
                      (funcall mk "review" "<2026-10-30 Fri>" 'review)))
         (sorted (mapcar (lambda (x) (plist-get x :title))
                         (ardentia--sort-tasks tasks now))))
    (should (equal sorted '("review" "ready" "near" "far")))))

(ert-deftest ardentia-test-later-flags-row ()
  (let ((now (ardentia-test--t "20261010-1200"))
        (ardentia-due-display 'absolute))
    (should (equal (ardentia--task-flags
                    (ardentia-test--task :queue 'ready
                                         :scheduled "<2026-10-10 Sat 18:30>")
                    now)
                   "due 18:30"))
    (should (equal (ardentia--task-flags
                    (ardentia-test--task :queue 'ready
                                         :scheduled "<2026-10-12 Mon 18:30>")
                    now)
                   "due 10-12"))
    (should (equal (ardentia--task-flags
                    (ardentia-test--task :queue 'ready :scheduled "<2026-10-09 Fri>")
                    now)
                   ""))
    (let ((row (ardentia--task-row
                (ardentia-test--task :queue 'ready :scheduled "<2026-10-12 Mon>")
                now)))
      (should (equal (aref row 0) "later"))
      (should (equal (aref row 5) "due 10-12")))
    (should (equal (ardentia--queue-name
                    (ardentia-test--task :queue 'review :scheduled "<2026-10-12 Mon>")
                    now)
                   "review"))))

(ert-deftest ardentia-test-later-toggle-filter ()
  (let* ((now (ardentia-test--t "20261010-1200"))
         (tasks (list (ardentia-test--task :queue 'ready :scheduled "<2026-10-12 Mon>")
                      (ardentia-test--task :queue 'ready)
                      (ardentia-test--task :queue 'review :scheduled "<2026-10-12 Mon>"))))
    (should (= (length (ardentia-dashboard--entries tasks nil now)) 3))
    (should (= (length (ardentia-dashboard--entries tasks nil now t)) 2))
    (should (= (length (ardentia-dashboard--entries tasks 'review now t)) 1))
    (with-temp-buffer
      (ardentia-dashboard-mode)
      (let ((ardentia-dashboard-show-later t))
        (should (ardentia-dashboard--show-later-p))
        (setq ardentia-dashboard--show-later nil)
        (should-not (ardentia-dashboard--show-later-p))))))

(ert-deftest ardentia-test-format-time-left ()
  (should (equal (ardentia--format-time-left 20) "1m"))
  (should (equal (ardentia--format-time-left 0) "1m"))
  (should (equal (ardentia--format-time-left 60) "1m"))
  (should (equal (ardentia--format-time-left 61) "2m"))
  (should (equal (ardentia--format-time-left 3540) "59m"))
  (should (equal (ardentia--format-time-left 3541) "1h00m"))
  (should (equal (ardentia--format-time-left 3600) "1h00m"))
  (should (equal (ardentia--format-time-left (+ (* 3 3600) 300)) "3h05m"))
  (should (equal (ardentia--format-time-left (- 86400 60)) "23h59m"))
  (should (equal (ardentia--format-time-left 86400) "1d0h"))
  (should (equal (ardentia--format-time-left (+ (* 2 86400) (* 3 3600) 600))
                 "2d3h")))

(ert-deftest ardentia-test-later-flag-relative-and-toggle ()
  (let* ((now (ardentia-test--t "20261010-1200"))
         (task (ardentia-test--task :queue 'ready
                                    :scheduled "<2026-10-10 Sat 15:05>"))
         (far (ardentia-test--task :queue 'ready
                                   :scheduled "<2026-10-12 Mon 15:00>")))
    (let ((ardentia-due-display 'relative))
      (should (equal (ardentia--task-flags task now) "due in 3h05m"))
      (should (equal (ardentia--task-flags far now) "due in 2d3h"))
      (should (equal (ardentia--later-flag task now 'absolute) "due 15:05")))
    (let ((ardentia-due-display 'absolute))
      (should (equal (ardentia--task-flags task now) "due 15:05"))
      (should (equal (ardentia--later-flag task now 'relative)
                     "due in 3h05m")))
    ;; the default is relative
    (should (eq (default-value 'ardentia-due-display) 'relative))
    ;; the dashboard toggle switches the rendered flag and sorting is kept
    (with-temp-buffer
      (ardentia-dashboard-mode)
      (let ((ardentia-due-display 'relative)
            (ardentia--state (list :tasks (list far task))))
        (cl-letf (((symbol-function 'ardentia-dashboard--update-layout) #'ignore)
                  ((symbol-function 'current-time) (lambda () now)))
          (ardentia-dashboard--render)
          (should (equal (mapcar (lambda (e) (aref (cadr e) 5))
                                 tabulated-list-entries)
                         '("due in 3h05m" "due in 2d3h")))
          (ardentia-dashboard-toggle-due)
          (should (eq (ardentia-dashboard--due-display) 'absolute))
          (should (equal (mapcar (lambda (e) (aref (cadr e) 5))
                                 tabulated-list-entries)
                         '("due 15:05" "due 10-12")))
          (ardentia-dashboard-toggle-due)
          (should (eq (ardentia-dashboard--due-display) 'relative)))))))

;;;; Age, host, flags, rows

(ert-deftest ardentia-test-format-age ()
  (should (equal (ardentia--format-age nil) ""))
  (should (equal (ardentia--format-age 720) "12m"))
  (should (equal (ardentia--format-age (* 3 3600 1.0)) "3h"))
  (should (equal (ardentia--format-age (* 2 86400)) "2d"))
  (should (equal (ardentia--format-age -5) "0m")))

(ert-deftest ardentia-test-parse-stamp ()
  (should (ardentia--parse-stamp "20261010-1953"))
  (should (ardentia--parse-stamp "2026-10-10 19:40"))
  (should (null (ardentia--parse-stamp "null")))
  (should (null (ardentia--parse-stamp nil)))
  (should (null (ardentia--parse-stamp "garbage")))
  (should (= (float-time (time-subtract (ardentia--parse-stamp "20261010-1953")
                                        (ardentia--parse-stamp "20261010-1941")))
             720)))

(ert-deftest ardentia-test-age-per-queue ()
  (let ((now (ardentia-test--t "20261010-2000")))
    (should (= (ardentia--task-age
                (ardentia-test--task :queue 'review
                                     :props '(("AI_SESSION" . "20261010-1948")))
                now)
               720))
    (should (= (ardentia--task-age
                (ardentia-test--task :queue 'running
                                     :props '(("AI_CLAIMED" . "20261010-1900")))
                now)
               3600))
    (should (= (ardentia--task-age
                (ardentia-test--task :queue 'ready :created "2026-10-10 19:00")
                now)
               3600))
    (should (null (ardentia--task-age (ardentia-test--task :queue 'ready) now)))))

(ert-deftest ardentia-test-host-column ()
  (should (equal (ardentia--task-host
                  (ardentia-test--task :tags '("a" "desktop_only")
                                       :props '(("AI_CLAIMED_BY" . "laptop"))))
                 "laptop"))
  (should (equal (ardentia--task-host (ardentia-test--task :tags '("a" "desktop_only")))
                 "desktop"))
  (should (equal (ardentia--task-host (ardentia-test--task :tags '("a"))) "default")))

(ert-deftest ardentia-test-attempts-and-flags ()
  (let ((now (ardentia-test--t "20261010-2000")))
    (should (equal (ardentia--task-attempts
                    (ardentia-test--task :props '(("AI_ATTEMPTS" . "2")))) "2"))
    (should (equal (ardentia--task-attempts
                    (ardentia-test--task :props '(("AI_ATTEMPTS" . "3")))) "3!"))
    (should (equal (ardentia--task-attempts (ardentia-test--task)) ""))
    (should (equal (ardentia--task-flags
                    (ardentia-test--task :queue 'running
                                         :props '(("AI_CLAIMED" . "20261010-1829")))
                    now)
                   "stale?"))
    (should (equal (ardentia--task-flags
                    (ardentia-test--task :queue 'running
                                         :props '(("AI_CLAIMED" . "20261010-1831")))
                    now)
                   ""))
    (should (equal (ardentia--task-flags
                    (ardentia-test--task
                     :queue 'review
                     :props '(("AI_LIMIT_INTERRUPTED" . "20261010-1900")
                              ("AI_SESSION" . "20200101-0000")))
                    now)
                   "limit"))))

(ert-deftest ardentia-test-row ()
  (let ((row (ardentia--task-row
              (ardentia-test--task :queue 'review :title "Hi" :file "/o/a.org"
                                   :props '(("AI_STATUS" . "partial")
                                            ("AI_SESSION" . "20261010-1948")
                                            ("AI_ATTEMPTS" . "1")))
              (ardentia-test--t "20261010-2000"))))
    (should (equal row ["review" "partial" "default" "1" "12m" "" "" "a.org" "Hi"]))))

;; Cost column of the dashboard row.
(ert-deftest ardentia-test-row-cost ()
  (let ((now (ardentia-test--t "20261010-2000")))
    (should (equal (aref (ardentia--task-row
                          (ardentia-test--task
                           :queue 'review
                           :props '(("AI_COST_USD_TOTAL" . "4.09")))
                          now)
                         6)
                   "$4.09"))
    (should (equal (aref (ardentia--task-row
                          (ardentia-test--task :queue 'review) now)
                         6)
                   ""))))

(ert-deftest ardentia-test-dashboard-entries-filter ()
  (let* ((tasks (list (ardentia-test--task :queue 'ready)
                      (ardentia-test--task :queue 'review)))
         (now (current-time)))
    (should (= (length (ardentia-dashboard--entries tasks nil now)) 2))
    (should (= (length (ardentia-dashboard--entries tasks 'review now)) 1))))

;;;; Trouble queue

(defun ardentia-test--trouble (tags &rest plist)
  "Build a task plist with TAGS as parsed from org-tool, plus PLIST."
  (ardentia--task-from-json
   (append plist
           `((tags . ,tags) (title . "T") (file . "/o/x.org") (done . :false)
             (created . "2026-10-09 08:00")))))

(defun ardentia-test--work-dir (&rest names)
  "Create a temp org directory whose ai/conflicts/work/ holds dirs NAMES."
  (let ((dir (make-temp-file "ardentia-merges-" t)))
    (make-directory (expand-file-name "ai/conflicts/work" dir) t)
    (dolist (n names)
      (make-directory (expand-file-name n (expand-file-name "ai/conflicts/work" dir)) t))
    dir))

(ert-deftest ardentia-test-trouble-from-json ()
  (should (equal (plist-get (ardentia-test--trouble '("auto_error")) :trouble) "error"))
  (should (equal (plist-get (ardentia-test--trouble '("x" "syncmerge" "aiready")) :trouble)
                 "merge"))
  (should (equal (plist-get (ardentia-test--trouble '("gitsync")) :trouble) "gitsync"))
  (should-not (plist-get (ardentia-test--trouble '("aiready")) :trouble))
  (should (eq (plist-get (ardentia-test--trouble '("auto_error" "aiready")) :queue)
              'ready)))

(ert-deftest ardentia-test-trouble-display ()
  (let ((now (ardentia-test--t "20261010-2000")))
    (dolist (case '((("auto_error") nil "" "error")
                    (("auto_error" "humanreview") review "review" "error:review")
                    (("syncmerge" "ai_in_progress") running "running" "merge:running")
                    (("gitsync" "aiready") ready "ready" "gitsync:ready")))
      (let ((task (ardentia-test--trouble (nth 0 case))))
        (should (eq (plist-get task :queue) (nth 1 case)))
        (should (eq (ardentia--effective-queue task now) 'trouble))
        (should (= (ardentia--sort-rank task now) -1))
        (should (equal (ardentia--queue-name task now) "trouble"))
        (should (equal (ardentia--task-flags task now) (nth 3 case)))))
    ;; not yet due: the lifecycle name is later, the flags keep the due time
    (let ((task (ardentia-test--trouble '("auto_error" "aiready")
                                        '(scheduled . "<2026-10-11 Sun>")))
          (ardentia-due-display 'absolute))
      (should (equal (ardentia--task-flags task now) "error:later due 10-11")))
    ;; existing flags follow
    (let ((task (ardentia-test--trouble
                 '("auto_error" "humanreview")
                 '(properties . ((AI_LIMIT_INTERRUPTED . "x"))))))
      (should (equal (ardentia--task-flags task now) "error:review limit")))
    ;; non-trouble tasks are untouched
    (should (equal (ardentia--task-flags (ardentia-test--task :queue 'review) now) ""))))

(ert-deftest ardentia-test-trouble-sorts-first-oldest-first ()
  (let* ((now (ardentia-test--t "20261010-2000"))
         (blocked (ardentia-test--task :queue 'review :title "blocked"
                                       :props '(("AI_STATUS" . "blocked"))))
         (t-new (ardentia-test--trouble '("gitsync") '(title . "new")
                                        '(created . "2026-10-10 19:00")))
         (t-old (ardentia-test--trouble '("auto_error" "aiready") '(title . "old")
                                        '(created . "2026-10-09 08:00")))
         (sorted (ardentia--sort-tasks (list blocked t-new t-old) now)))
    (should (equal (mapcar (lambda (x) (plist-get x :title)) sorted)
                   '("old" "new" "blocked")))))

(ert-deftest ardentia-test-trouble-leaves-review-alone ()
  (let* ((now (current-time))
         (tr (ardentia-test--trouble '("auto_error" "humanreview")))
         (bare (ardentia-test--trouble '("auto_error")))
         (tasks (list tr bare (ardentia-test--task :queue 'ready))))
    (should (= (length (ardentia-dashboard--entries tasks nil now)) 3))
    ;; the review inbox still shows the trouble task that is in review
    (should (= (length (ardentia-dashboard--entries tasks 'review now)) 1))
    (should (eq (car (car (ardentia-dashboard--entries tasks 'review now))) tr))
    ;; trouble tasks survive hiding later tasks
    (should (= (length (ardentia-dashboard--entries
                        (list (ardentia-test--trouble
                               '("auto_error" "aiready")
                               '(scheduled . "<2999-01-01 Mon>")))
                        nil now t))
               1))
    ;; the counter counts the review queue only
    (should (= (length (seq-filter (lambda (tk) (eq (plist-get tk :queue) 'review))
                                   tasks))
               1))))

(ert-deftest ardentia-test-merge-file-name ()
  (should (equal (ardentia--merge-file-name
                  "20260915-154732-refile.sync-conflict-20260915-051414-5RGYLOM.org.quarantined")
                 "refile.org"))
  (should (equal (ardentia--merge-file-name
                  "20260915-154732-my.notes.sync-conflict-20260915-051414-ABC1234.md.quarantined")
                 "my.notes.md"))
  (should (equal (ardentia--merge-file-name
                  "20260915-154732-todo.sync-conflict-20260915-051414-ABC1234.quarantined")
                 "todo"))
  (should (equal (ardentia--merge-file-name "something-else") "something-else"))
  (should (equal (ardentia--merge-file-name "") "")))

(ert-deftest ardentia-test-list-merges ()
  (let* ((name "20260915-154732-refile.sync-conflict-20260915-051414-5RGYLOM.org.quarantined")
         (dir (ardentia-test--work-dir name))
         (ardentia-org-directory dir))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "ai/conflicts/work/plain.txt" dir))
          (let ((merges (ardentia--list-merges)))
            (should (= (length merges) 1))
            (let ((m (car merges)))
              (should (plist-get m :merge))
              (should (equal (plist-get m :file) "refile.org"))
              (should (equal (plist-get m :title) (concat "Running merge: " name)))
              (should (file-directory-p (plist-get m :dir)))
              (should (plist-get m :mtime)))))
      (delete-directory dir t))
    ;; missing directory, and a directory that cannot be listed, give nil
    (let ((ardentia-org-directory "/nonexistent/ardentia-test/"))
      (should-not (ardentia--list-merges)))
    (let ((dir (ardentia-test--work-dir "x")))
      (unwind-protect
          (let ((ardentia-org-directory dir))
            (cl-letf (((symbol-function 'directory-files)
                       (lambda (&rest _) (error "boom"))))
              (should-not (ardentia--list-merges))))
        (delete-directory dir t)))))

(ert-deftest ardentia-test-merge-rows ()
  (let* ((dir (ardentia-test--work-dir
               "20260915-154732-a.sync-conflict-20260915-051414-AAAAAAA.org.quarantined"))
         (ardentia-org-directory dir)
         (now (current-time)))
    (unwind-protect
        (let* ((merges (ardentia--list-merges))
               (m (car merges)))
          (should (equal (ardentia--task-row m now)
                         (vector "trouble" "" "" "" "0m" "merging" "" "a.org"
                                 (plist-get m :title))))
          (should (eq (ardentia--effective-queue m now) 'trouble))
          (should (= (ardentia--sort-rank m now) -1))
          ;; sorted into the trouble rank, before a blocked review task
          (let* ((blocked (ardentia-test--task :queue 'review
                                               :props '(("AI_STATUS" . "blocked"))))
                 (entries (ardentia-dashboard--entries (list blocked) nil now nil merges)))
            (should (= (length entries) 2))
            (should (eq (car (car entries)) m))
            (should (eq (car (cadr entries)) blocked)))
          ;; the review inbox never lists merges
          (should-not (ardentia-dashboard--entries nil 'review now nil merges)))
      (delete-directory dir t))))

(ert-deftest ardentia-test-trouble-group-absent-when-empty ()
  (let* ((now (current-time))
         (tasks (list (ardentia-test--task :queue 'review)
                      (ardentia-test--task :queue 'ready))))
    (should-not (seq-some (lambda (e) (equal (aref (cadr e) 0) "trouble"))
                          (ardentia-dashboard--entries tasks nil now nil nil)))))

(ert-deftest ardentia-test-merge-rows-are-not-tasks ()
  (let ((buf (get-buffer-create " *ardentia-test-merge*"))
        (merge (list :merge t :dir "/tmp/x/" :name "x" :file "x" :title "Running merge: x"))
        (ardentia--state nil)
        dired-arg)
    (unwind-protect
        (with-current-buffer buf
          (ardentia-dashboard-mode)
          (cl-letf (((symbol-function 'ardentia-dashboard--task-at-point)
                     (lambda () merge))
                    ((symbol-function 'dired)
                     (lambda (d &rest _) (setq dired-arg d)))
                    ((symbol-function 'ardentia-visit-task)
                     (lambda (_) (error "must not visit")))
                    ((symbol-function 'ardentia-review-task)
                     (lambda (_) (error "must not review"))))
            (ardentia-dashboard-enter)
            (should (equal dired-arg "/tmp/x/"))
            (setq dired-arg nil)
            (ardentia-dashboard-visit)
            (should (equal dired-arg "/tmp/x/"))
            (dolist (cmd '(ardentia-dashboard-transcript ardentia-dashboard-agent-shell
                           ardentia-dashboard-accept ardentia-dashboard-send-back
                           ardentia-dashboard-run ardentia-dashboard-stop
                           ardentia-dashboard-review ardentia-dashboard-priority
                           ardentia-dashboard-priority-up
                           ardentia-dashboard-priority-down))
              (let ((err (should-error (funcall cmd) :type 'user-error)))
                (should (equal (cadr err) "Not a task: a running merge"))))
            ;; commands reached through the current-task function
            (let ((err (should-error (ardentia--current-task) :type 'user-error)))
              (should (equal (cadr err) "Not a task: a running merge")))))
      (kill-buffer buf))))

;;;; Dashboard buffer

(ert-deftest ardentia-test-dashboard-buffer ()
  (let* ((ardentia--state
          (list :tasks (ardentia--tasks-from-json (ardentia--parse-json ardentia-test--json))
                :host "desktop" :time (current-time)))
         (buf (get-buffer-create " *ardentia-test*")))
    (unwind-protect
        (with-current-buffer buf
          (ardentia-dashboard-mode)
          (ardentia-dashboard--render)
          (should (string-match-p "review +blocked +default +3!" (buffer-string)))
          (should (string-match-p "ready .*laptop.*b\\.org +Plain" (buffer-string)))
          (goto-char (point-min))
          (forward-line 1)
          (should (equal (plist-get (ardentia-dashboard--task-at-point) :title)
                         "Review me"))
          (should (string-match-p "Host: desktop" (ardentia-dashboard--info-line))))
      (kill-buffer buf))))

(defun ardentia-test--dash-task (title line &optional props)
  "Return a minimal ready task plist with TITLE at LINE."
  (list :file "/tmp/x/a.org" :id title :line line :title title
        :queue 'ready :props props :tags nil))

(defmacro ardentia-test--with-dash (tasks &rest body)
  "Run BODY in a dashboard buffer rendered from TASKS."
  (declare (indent 1))
  `(let* ((ardentia--state (list :tasks ,tasks :host "desktop" :time (current-time)))
          (buf (get-buffer-create " *ardentia-test-keep*")))
     (unwind-protect
         (with-current-buffer buf
           (ardentia-dashboard-mode)
           (ardentia-dashboard--render)
           ,@body)
       (kill-buffer buf))))

(defun ardentia-test--goto-title (title)
  "Move point to the row of the task TITLE."
  (goto-char (point-min))
  (while (not (equal (plist-get (ardentia-dashboard--task-at-point) :id) title))
    (forward-line 1)))

(ert-deftest ardentia-test-dashboard-render-keeps-point ()
  (ardentia-test--with-dash (list (ardentia-test--dash-task "Aaa" 1)
                                  (ardentia-test--dash-task "Bbb" 5)
                                  (ardentia-test--dash-task "Ccc" 9))
    (ardentia-test--goto-title "Bbb")
    (setq ardentia--state
          (list :tasks (list (ardentia-test--dash-task "New" 1)
                             (ardentia-test--dash-task "Aaa" 4)
                             (ardentia-test--dash-task "Bbb" 9 '(("X" . "1")))
                             (ardentia-test--dash-task "Ccc" 13))
                :host "desktop" :time (current-time)))
    (ardentia-dashboard--render)
    (should (equal (plist-get (ardentia-dashboard--task-at-point) :id) "Bbb"))))

(ert-deftest ardentia-test-dashboard-render-removed-row ()
  (ardentia-test--with-dash (list (ardentia-test--dash-task "Aaa" 1)
                                  (ardentia-test--dash-task "Bbb" 5)
                                  (ardentia-test--dash-task "Ccc" 9))
    (ardentia-test--goto-title "Bbb")
    (let ((line (line-number-at-pos)))
      (setq ardentia--state
            (list :tasks (list (ardentia-test--dash-task "Aaa" 1)
                               (ardentia-test--dash-task "Ccc" 9))
                  :host "desktop" :time (current-time)))
      (ardentia-dashboard--render)
      (should (> (point) (point-min)))
      (should (= (line-number-at-pos) line))
      ;; the last rows disappear: clamp to the last row, not the top
      (ardentia-test--goto-title "Ccc")
      (setq ardentia--state
            (list :tasks (list (ardentia-test--dash-task "Aaa" 1))
                  :host "desktop" :time (current-time)))
      (ardentia-dashboard--render)
      (should (equal (plist-get (ardentia-dashboard--task-at-point) :id) "Aaa")))))

(ert-deftest ardentia-test-dashboard-render-keeps-window-point ()
  (ardentia-test--with-dash (list (ardentia-test--dash-task "Aaa" 1)
                                  (ardentia-test--dash-task "Bbb" 5)
                                  (ardentia-test--dash-task "Ccc" 9))
    (save-window-excursion
      (let ((win (selected-window)) (other nil))
        (set-window-buffer win buf)
        (setq other (condition-case nil (split-window win) (error nil)))
        (skip-unless other)
        (set-window-buffer other buf)
        (ardentia-test--goto-title "Ccc")
        (set-window-point other (point))
        (goto-char (point-min))
        (set-window-point win (point-min))
        (setq ardentia--state
              (list :tasks (list (ardentia-test--dash-task "New" 1)
                                 (ardentia-test--dash-task "Aaa" 2)
                                 (ardentia-test--dash-task "Bbb" 6)
                                 (ardentia-test--dash-task "Ccc" 10))
                    :host "desktop" :time (current-time)))
        (ardentia-dashboard--render)
        (should (equal (plist-get (get-text-property
                                   (window-point other) 'tabulated-list-id)
                                  :id)
                       "Ccc"))))))

;; A window keeps its scroll offset over several renders also when its point
;; is not at the start of a line (`count-lines' counts that partial line).
(ert-deftest ardentia-test-dashboard-render-keeps-window-start ()
  (ardentia-test--with-dash (mapcar (lambda (n)
                                      (ardentia-test--dash-task
                                       (format "T%02d" n) n))
                                    (number-sequence 1 60))
    (save-window-excursion
      (let ((win (selected-window)) (other nil))
        (setq other (condition-case nil (split-window win) (error nil)))
        (skip-unless other)
        (set-window-buffer win (get-buffer-create " *ardentia-test-other*"))
        (set-window-buffer other buf)
        (goto-char (point-min))
        (forward-line 29)
        (move-to-column 12)
        (should (> (current-column) 0))
        (set-window-point other (point))
        (set-window-start other (save-excursion (forward-line -5) (point)))
        (let ((start (line-number-at-pos (window-start other)))
              (row (line-number-at-pos (window-point other))))
          (dotimes (_ 3) (ardentia-dashboard--render))
          (should (= row (line-number-at-pos (window-point other))))
          (should (= start (line-number-at-pos (window-start other)))))))))

(ert-deftest ardentia-test-send-back-adapted-indentation ()
  (ardentia-test--with-org-file f "* TODO Bare :humanreview:
:PROPERTIES:
:ID:       id-bare
:END:
Text
"
    (let ((org-adapt-indentation t))
      (ardentia-send-back-1 f "id-bare" "again"))
    (should (string-match-p ":LOGBOOK:\n *- Note taken on \\[STAMP\\] \\\\\\\\\n *  again\n *:END:"
                            (ardentia-test--file-text f)))))

;;;; Configuration parsing

(ert-deftest ardentia-test-hosts-conf ()
  (let ((conf (ardentia--parse-hosts-conf
               "# comment\nhosts=laptop desktop\n# x\ndefault_host=desktop\nactive_hosts=laptop\n")))
    (should (equal (plist-get conf :hosts) '("laptop" "desktop")))
    (should (equal (plist-get conf :default-host) "desktop"))))

(ert-deftest ardentia-test-timer-output ()
  (let ((p (ardentia--parse-timer-output
            "LastTriggerUSec=Sat 2026-10-10 19:53:05 CEST\nNextElapseUSecRealtime=n/a\n")))
    (should (equal (plist-get p :last) "Sat 2026-10-10 19:53:05 CEST"))
    (should (null (plist-get p :next)))
    (should (null (plist-get p :running))))
  ;; Real output of `systemctl --user show SERVICE TIMER' during a run
  ;; (2026-10-10): the service block comes first.
  (let ((p (ardentia--parse-timer-output
            "ActiveState=activating\n\nNextElapseUSecRealtime=\nLastTriggerUSec=Sat 2026-10-10 20:15:35 CEST\nActiveState=active\n")))
    (should (plist-get p :running))
    (should (null (plist-get p :next)))
    (should (string-match-p "after the current run"
                            (ardentia-dashboard--info-line (list :timer p)))))
  (let ((p (ardentia--parse-timer-output
            "ActiveState=inactive\n\nNextElapseUSecRealtime=Sat 2026-10-10 20:45:00 CEST\nLastTriggerUSec=x\nActiveState=active\n")))
    (should (null (plist-get p :running)))
    (should (equal (plist-get p :next) "Sat 2026-10-10 20:45:00 CEST"))))

(ert-deftest ardentia-test-info-line ()
  (should (string-match-p "Host: unknown.*last: unknown.*Refreshed: never"
                          (ardentia-dashboard--info-line nil)))
  (should (string-match-p
           "Host: desktop.*last: L next: N.*refreshing"
           (ardentia-dashboard--info-line
            '(:host "desktop" :timer (:last "L" :next "N") :refreshing t)))))

;;;; Run report extraction

(defconst ardentia-test--logbook
  "- Note taken on [2026-10-10 Sat 20:10] \\\\
  lets go with ardentia and build an mvp
- AI-Lauf [2026-10-10 Sat 19:53] completed: Headline \\\\
  - Open: your choice
  - == METHOD ==
    - 3 researchers
      - nested
  - == COMMITS ==
    - 10cd93a1 something
- Added: [2026-10-10 Sat 19:40]
- Added From: 
- AI-Lauf [2026-10-09 Fri 10:00] completed: Older \\\\
  - older stuff
"
  "LOGBOOK content in the real format.")

(ert-deftest ardentia-test-extract-run-report ()
  (let* ((r (ardentia--extract-run-report ardentia-test--logbook))
         (report (plist-get r :report))
         (notes (plist-get r :notes)))
    (should (string-prefix-p "- AI-Lauf [2026-10-10 Sat 19:53] completed: Headline"
                             (car report)))
    (should (equal (nth 1 report) "  - Open: your choice"))
    (should (member "      - nested" report))
    (should (equal (car (last report)) "    - 10cd93a1 something"))
    (should-not (cl-some (lambda (l) (string-match-p "Added\\|Older" l)) report))
    (should (= (length notes) 1))
    (should (equal (nth 1 (car notes)) "  lets go with ardentia and build an mvp"))))

(ert-deftest ardentia-test-extract-run-report-none ()
  (let ((r (ardentia--extract-run-report "- Added: [2026-10-10 Sat 19:40]\n")))
    (should (null (plist-get r :report)))
    (should (null (plist-get r :notes))))
  (should (null (plist-get (ardentia--extract-run-report nil) :report))))

(ert-deftest ardentia-test-logbook-content ()
  (should (equal (ardentia--logbook-content
                  "x\n:LOGBOOK:\n- a\n:END:\ntext\n:LOGBOOK:\n- b\n:END:\n")
                 "- a\n"))
  (should (null (ardentia--logbook-content "no drawer\n"))))

(ert-deftest ardentia-test-classify-children ()
  (let* ((c (ardentia--classify-children
             (list '(:title "R1" :todo nil)
                   '(:title "Done" :todo "DONE" :done t)
                   '(:title "Open" :todo "TODO" :done nil)
                   '(:title "R2" :todo nil))))
         (titles (lambda (k) (mapcar (lambda (x) (plist-get x :title)) (plist-get c k)))))
    (should (equal (funcall titles :results) '("R1" "R2")))
    (should (equal (funcall titles :for-you) '("Open" "Done")))))

;;;; Review collection and rendering (visiting buffer)

(ert-deftest ardentia-test-review-collect-and-render ()
  (ardentia-test--with-org-file f ardentia-test--org
    (let* ((info (ardentia-review--collect f "id-one"))
           (text (ardentia-review--render-string info)))
      (should (equal (plist-get info :title) "Task one"))
      (should (equal (plist-get info :tags) '("refile" "humanreview")))
      (should (equal (plist-get info :props) '(("AI_STATUS" . "completed"))))
      (should (equal (plist-get info :body) "Original task text"))
      (should (equal (mapcar (lambda (c) (plist-get c :title)) (plist-get info :results))
                     '("Result A")))
      (should (equal (mapcar (lambda (c) (plist-get c :title)) (plist-get info :for-you))
                     '("Decide this" "Do that" "Old")))
      (should (string-match-p "\\[\\[id:id-a\\]\\[Result A\\]\\]" text))
      (should (string-match-p "- TODO Decide this :humanreview:" text))
      (should (string-match-p "  - == COMMITS ==\n    - abc1234 something" text))
      (should (equal (plist-get info :todo) "TODO"))
      (should (null (plist-get info :priority)))
      (should (string-match-p "^\\* TODO Task one :refile:humanreview:$" text))
      (should (string-match-p "^- Status :: TODO, agents: completed$" text))
      (should (string-match-p "Original task text" text)))))

(ert-deftest ardentia-test-review-collect-priority ()
  (ardentia-test--with-org-file f "* DONE [#B] Prio task :humanreview:
:PROPERTIES:
:ID:       id-p
:END:
* WAIT Plain task
:PROPERTIES:
:ID:       id-q
:END:
"
    (let ((p (ardentia-review--collect f "id-p"))
          (q (ardentia-review--collect f "id-q")))
      (should (equal (plist-get p :todo) "DONE"))
      (should (equal (plist-get p :priority) ?B))
      (should (equal (plist-get p :title) "Prio task"))
      (should (equal (plist-get q :todo) "WAIT"))
      (should (null (plist-get q :priority))))))

(ert-deftest ardentia-test-review-render-state ()
  (let ((base '(:file "/tmp/x.org" :title "T" :body "")))
    (let ((text (ardentia-review--render-string
                 (append '(:todo "TODO" :priority ?A :tags ("a")
                           :props (("AI_STATUS" . "completed")))
                         base))))
      (should (string-match-p "^\\* TODO \\[#A\\] T :a:$" text))
      (should (string-match-p "^- Status :: TODO, agents: completed$" text)))
    (let ((text (ardentia-review--render-string base)))
      (should (string-match-p "^\\* T$" text))
      (should (string-match-p "^- Status :: no TODO keyword$" text)))
    (let ((text (ardentia-review--render-string
                 (append '(:todo "DONE" :props (("AI_STATUS" . "failed"))) base))))
      (should (string-match-p "^\\* DONE T$" text))
      (should (string-match-p "^- Status :: DONE, agents: failed$" text)))
    (should (string-match-p
             "^- Status :: no TODO keyword, agents: x$"
             (ardentia-review--render-string
              (append '(:props (("AI_STATUS" . "x"))) base))))))

(ert-deftest ardentia-test-review-buffer ()
  (ardentia-test--with-org-file f ardentia-test--org
    (let (buf)
      (cl-letf (((symbol-function 'pop-to-buffer) (lambda (b &rest _) (setq buf b))))
        (ardentia-review-task (list :file f :id "id-one" :title "Task one")))
      (unwind-protect
          (with-current-buffer buf
            (should buffer-read-only)
            (should (derived-mode-p 'ardentia-review-mode))
            (should org-indent-mode)
            (should (string-match-p "Latest run report" (buffer-string)))
            ;; Re-rendering keeps the indentation on.
            (ardentia-review-render)
            (should org-indent-mode)
            (should (string-match-p "Latest run report" (buffer-string))))
        (kill-buffer buf)))))

(ert-deftest ardentia-test-review-buffer-no-indent ()
  (ardentia-test--with-org-file f ardentia-test--org
    (let (buf (ardentia-review-indent nil))
      (cl-letf (((symbol-function 'pop-to-buffer) (lambda (b &rest _) (setq buf b))))
        (ardentia-review-task (list :file f :id "id-one" :title "Task one")))
      (unwind-protect
          (with-current-buffer buf
            (should-not org-indent-mode))
        (kill-buffer buf)))))

(ert-deftest ardentia-test-review-buffer-wraps-lines ()
  (ardentia-test--with-org-file f ardentia-test--org
    (let (buf (org-startup-truncated t)
              (task (list :file f :id "id-one" :title "Task one")))
      (cl-letf (((symbol-function 'pop-to-buffer) (lambda (b &rest _) (setq buf b))))
        (ardentia-review-task task)
        (unwind-protect
            (with-current-buffer buf
              (should-not truncate-lines)
              (should word-wrap)
              ;; A manual toggle survives a re-render.
              (setq truncate-lines t)
              (ardentia-review-render)
              (should truncate-lines)
              ;; Opening the task again restores the default.
              (ardentia-review-task task)
              (should-not truncate-lines))
          (kill-buffer buf))))))

(ert-deftest ardentia-test-review-buffer-truncate-lines-option ()
  (ardentia-test--with-org-file f ardentia-test--org
    (let (buf (ardentia-review-truncate-lines t) (org-startup-truncated nil))
      (cl-letf (((symbol-function 'pop-to-buffer) (lambda (b &rest _) (setq buf b))))
        (ardentia-review-task (list :file f :id "id-one" :title "Task one")))
      (unwind-protect
          (with-current-buffer buf
            (should truncate-lines))
        (kill-buffer buf)))))

;;;; Mutations

(ert-deftest ardentia-test-accept ()
  (ardentia-test--with-org-file f ardentia-test--org
    (ardentia-accept-1 f "id-one")
    (let ((text (ardentia-test--file-text f)))
      (should (string-match-p "^\\* DONE Task one :refile:humanreview:$" text))
      (should (string-match-p "CLOSED: \\[STAMP\\]" text))
      ;; The human's DONE logs only the CLOSED time stamp, no State note.
      (should (string-match-p ":LOGBOOK:\n- AI-Lauf" text)))
    (should-not (buffer-modified-p (find-buffer-visiting f)))))

(ert-deftest ardentia-test-send-back-existing-logbook ()
  (ardentia-test--with-org-file f ardentia-test--org
    (ardentia-send-back-1 f "id-one" "please check\nthe second part")
    (should (equal
             (ardentia-test--file-text f)
             (concat "* TODO Task one :refile:aiready:
:PROPERTIES:
:ID:       id-one
:AI_STATUS: completed
:END:
:LOGBOOK:
- Note taken on [STAMP] \\\\
  please check
  the second part
- AI-Lauf [STAMP] completed: Headline \\\\
  - Recommendation: do it
  - == COMMITS ==
    - abc1234 something
:END:
Original task text
** Result A
:PROPERTIES:
:ID:       id-a
:END:
** TODO Decide this :humanreview:
** TODO Do that :humanreview:
** DONE Old :humanreview:
* TODO Other :refile:
")))
    (should-not (buffer-modified-p (find-buffer-visiting f)))))

(ert-deftest ardentia-test-send-back-new-logbook ()
  (ardentia-test--with-org-file f "* TODO Bare :humanreview:
:PROPERTIES:
:ID:       id-bare
:END:
Text
* TODO Next
"
    (ardentia-send-back-1 f "id-bare" "try again")
    (should (equal
             (ardentia-test--file-text f)
             "* TODO Bare :aiready:
:PROPERTIES:
:ID:       id-bare
:END:
:LOGBOOK:
- Note taken on [STAMP] \\\\
  try again
:END:
Text
* TODO Next
"))))

(ert-deftest ardentia-test-send-back-needs-note ()
  (ardentia-test--with-org-file f ardentia-test--org
    (should-error (ardentia-send-back-1 f "id-one" "  ") :type 'user-error)
    (should-not (buffer-modified-p (find-buffer-visiting f)))))

(ert-deftest ardentia-test-yourself-child ()
  (ardentia-test--with-org-file f ardentia-test--org
    (ardentia-yourself-1 f "id-one" "Do that" "you can")
    (let ((text (ardentia-test--file-text f)))
      (should (string-match-p
               "\\*\\* TODO Do that :aiready:\n:LOGBOOK:\n- Note taken on \\[STAMP\\] \\\\\\\\\n  you can\n:END:\n\\*\\* DONE Old"
               text))
      (should (string-match-p "^\\*\\* TODO Decide this :humanreview:$" text))
      (should (string-match-p "^\\* TODO Task one :refile:humanreview:$" text)))))

(ert-deftest ardentia-test-yourself-without-note ()
  (ardentia-test--with-org-file f ardentia-test--org
    (ardentia-yourself-1 f "id-one" "Do that" "")
    (should (string-match-p "^\\*\\* TODO Do that :aiready:\n\\*\\* DONE Old"
                            (ardentia-test--file-text f)))))

(ert-deftest ardentia-test-yourself-refuses-done-child ()
  (ardentia-test--with-org-file f ardentia-test--org
    (should-error (ardentia-yourself-1 f "id-one" "Old" "") :type 'user-error)
    (should-error (ardentia-yourself-1 f "id-one" "Result A" "") :type 'user-error)))

(ert-deftest ardentia-test-precheck-wrong-tag ()
  (ardentia-test--with-org-file f ardentia-test--org
    (let ((err (should-error (ardentia-accept-1 f "id-one-missing") :type 'user-error)))
      (should (string-match-p "not found" (cadr err))))
    ;; Task "Other" lacks an ID; give the fixture a second task without the tag.
    (with-current-buffer (find-file-noselect f)
      (goto-char (point-min))
      (org-toggle-tag "humanreview" 'off)
      (save-buffer))
    (let ((err (should-error (ardentia-accept-1 f "id-one") :type 'user-error)))
      (should (string-match-p "Task state changed -- refresh (g)" (cadr err))))
    (should-error (ardentia-send-back-1 f "id-one" "x") :type 'user-error)
    (should-not (string-match-p "DONE Task one" (ardentia-test--file-text f)))))

(ert-deftest ardentia-test-precheck-modified-buffer-stays-unsaved ()
  (ardentia-test--with-org-file f ardentia-test--org
    (with-current-buffer (find-file-noselect f)
      (goto-char (point-max))
      (insert "unsaved\n"))
    (ardentia-send-back-1 f "id-one" "note")
    (let ((buf (find-buffer-visiting f)))
      (should (buffer-modified-p buf))
      (should-not (string-match-p "note" (with-temp-buffer
                                           (insert-file-contents f)
                                           (buffer-string)))))))

(ert-deftest ardentia-test-precheck-reverts-changed-file ()
  (ardentia-test--with-org-file f ardentia-test--org
    (find-file-noselect f)
    (sleep-for 1.1)
    (with-temp-file f
      (insert (replace-regexp-in-string "Task one" "Task uno" ardentia-test--org)))
    (ardentia-accept-1 f "id-one")
    (should (string-match-p "^\\* DONE Task uno" (ardentia-test--file-text f)))))

(ert-deftest ardentia-test-precheck-refuses-changed-file-with-edits ()
  (ardentia-test--with-org-file f ardentia-test--org
    (with-current-buffer (find-file-noselect f)
      (goto-char (point-max)) (insert "x"))
    (sleep-for 1.1)
    (with-temp-file f (insert ardentia-test--org "more\n"))
    (let ((err (should-error (ardentia-accept-1 f "id-one") :type 'user-error)))
      (should (string-match-p "unsaved changes" (cadr err))))))

;;;; Capture

(ert-deftest ardentia-test-capture-template ()
  (should (equal (ardentia-capture--template nil)
                 "* TODO %^{Task} :refile:aiready:\n:LOGBOOK:\n- Added: %U\n- Added From: %a\n:END:\n%?"))
  (should (string-match-p ":refile:aiready:desktop_only:\n"
                          (ardentia-capture--template "desktop")))
  (should (string-match-p "#\\+begin_quote\n%i\n#\\+end_quote\n%\\?\\'"
                          (ardentia-capture--template "laptop" t)))
  (dolist (h '(nil "desktop" "laptop"))
    (should (string-match-p
             "\\`\\* TODO %\\^{Task} \\(:[[:alnum:]_@#%]+\\)+:\n"
             (ardentia-capture--template h))))
  (should-error (ardentia-capture--template "bad-host") :type 'user-error))

(defun ardentia-test--capture (host region)
  "Capture with HOST and REGION into a temp file; return the final text."
  (let* ((target (make-temp-file "ardentia-cap-" nil ".org" "* Existing\n"))
         (src (make-temp-file "ardentia-src-" nil ".org" "* Source\nsome text\n"))
         (ardentia-capture-file target)
         (org-bookmark-names-plist nil)
         (org-todo-keywords '((sequence "TODO" "|" "DONE")))
         (message-log-max nil)
         text)
    (unwind-protect
        (with-current-buffer (find-file-noselect src)
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Ship it"))
                    ((symbol-function 'completing-read)
                     (lambda (p coll &rest _)
                       (if (string-prefix-p "Task" p) "Ship it" (car coll)))))
            (ardentia-capture--run host region))
          (goto-char (point-max))
          (insert "BODY")
          (org-capture-finalize)
          (with-current-buffer (find-file-noselect target)
            (save-buffer)
            (setq text (buffer-string))))
      (dolist (f (list target src))
        (let ((b (find-buffer-visiting f)))
          (when b (with-current-buffer b (set-buffer-modified-p nil)) (kill-buffer b)))
        (delete-file f)))
    text))

(ert-deftest ardentia-test-capture-end-to-end ()
  (let ((text (ardentia-test--capture "desktop" "Quote 100% %a %U\nline two")))
    (should (string-match-p
             "^\\* TODO Ship it +:refile:aiready:desktop_only:$" text))
    (should (string-match-p ":LOGBOOK:\n- Added: \\[[0-9-]+ [^]]+\\]\n- Added From: \\[\\[file:" text))
    (should (string-match-p
             "#\\+begin_quote\nQuote 100% %a %U\nline two\n#\\+end_quote\n" text))))

(ert-deftest ardentia-test-capture-without-region ()
  (let ((text (ardentia-test--capture nil nil)))
    (should (string-match-p "^\\* TODO Ship it +:refile:aiready:$" text))
    (should-not (string-match-p "begin_quote" text))))

;;;; Mode line

(ert-deftest ardentia-test-mode-line-string ()
  (let ((ardentia--review-count nil))
    (should (equal (ardentia--mode-line-string) "")))
  (let ((ardentia--review-count 0))
    (should (equal (ardentia--mode-line-string) "")))
  (let* ((ardentia--review-count 3)
         (s (ardentia--mode-line-string)))
    (should (equal s " AR:3"))
    (should (eq (get-text-property 1 'face s) 'ardentia-mode-line))))

(ert-deftest ardentia-test-mode-line-mode ()
  (let ((global-mode-string '("")) (ardentia--mode-line-timer nil)
        (ardentia-org-tool "/nonexistent/org-tool"))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (&rest _) (timer-create))))
            (ardentia-mode-line-mode 1))
          (should (member ardentia--mode-line-construct global-mode-string))
          (should (timerp ardentia--mode-line-timer))
          (ardentia-mode-line-mode 1)
          (should (= 1 (cl-count ardentia--mode-line-construct global-mode-string
                                 :test #'equal)))
          (ardentia-mode-line-mode -1)
          (should-not (member ardentia--mode-line-construct global-mode-string))
          (should (null ardentia--mode-line-timer)))
      (ardentia-mode-line-mode -1))))

(ert-deftest ardentia-test-mode-line-interval-restarts-timer ()
  (let ((global-mode-string '("")) (ardentia--mode-line-timer nil)
        (ardentia-org-tool "/nonexistent/org-tool")
        (old (default-value 'ardentia-mode-line-interval))
        calls cancelled)
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (&rest args) (push args calls) (timer-create)))
                  ((symbol-function 'cancel-timer)
                   (lambda (tm) (push tm cancelled))))
          (ardentia-mode-line-mode 1)
          (let ((first ardentia--mode-line-timer))
            (setq calls nil)
            (customize-set-variable 'ardentia-mode-line-interval 30)
            (should (equal cancelled (list first)))
            (should (= 1 (length calls)))
            (should (equal (cl-subseq (car calls) 0 2) '(30 30)))
            (should (timerp ardentia--mode-line-timer))
            (should-not (eq ardentia--mode-line-timer first))))
      (ardentia-mode-line-mode -1)
      (set-default 'ardentia-mode-line-interval old))))

(ert-deftest ardentia-test-mode-line-reload-keeps-timer ()
  (let ((global-mode-string '("")) (ardentia--mode-line-timer nil)
        (ardentia-org-tool "/nonexistent/org-tool")
        (old (default-value 'ardentia-mode-line-interval))
        (file (locate-library "ardentia-modeline.el")))
    (should file)
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (&rest _) (timer-create)))
                  ((symbol-function 'cancel-timer) #'ignore))
          (ardentia-mode-line-mode 1)
          (should (timerp ardentia--mode-line-timer))
          ;; Re-evaluating the defcustom calls its :set with the timer running.
          ;; The helper is already defined here; undefine it so that only
          ;; its position in the file decides whether the reload works.
          (fmakunbound 'ardentia--mode-line-timer-restart)
          (load file nil t t)
          (should (timerp ardentia--mode-line-timer)))
      (ardentia-mode-line-mode -1)
      (set-default 'ardentia-mode-line-interval old))))

(ert-deftest ardentia-test-mode-line-interval-mode-off ()
  (let ((global-mode-string '("")) (ardentia--mode-line-timer nil)
        (old (default-value 'ardentia-mode-line-interval))
        calls)
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (&rest args) (push args calls) (timer-create))))
          (ardentia-mode-line-mode -1)
          (customize-set-variable 'ardentia-mode-line-interval 30)
          (should (null calls))
          (should (null ardentia--mode-line-timer)))
      (ardentia-mode-line-mode -1)
      (set-default 'ardentia-mode-line-interval old))))

(ert-deftest ardentia-test-mode-line-tick-never-errors ()
  (let ((ardentia--mode-line-busy nil))
    (cl-letf (((symbol-function 'ardentia--mode-line-refresh)
               (lambda () (error "Boom"))))
      (ardentia--mode-line-tick)
      (should-not ardentia--mode-line-busy))))

;;;; Review alert

(defun ardentia-test--rt (id &optional title)
  "Return a review task with ID and TITLE."
  (list :queue 'review :file "/o/r.org" :id id :title (or title id) :tags nil))

(defmacro ardentia-test--with-alert (opts &rest body)
  "Run BODY with a fresh alert state, OPTS bound, and no real processes.
`runs' collects the skip command calls, `skip-status' (an alist from ID
to status) answers them, `alerts' collects the alerted task lists."
  (declare (indent 1))
  `(let ((ardentia--review-alert-seen nil)
         (ardentia--review-alert-baseline nil)
         (ardentia-review-alert-ding nil)
         (ardentia-review-alert-skip-command "/bin/sh")
         (ardentia-review-alert nil)
         (runs nil) (skip-status nil) (alerts nil))
     (ignore skip-status)
     (let ,opts
       (cl-letf (((symbol-function 'ardentia--run-async)
                  (lambda (program args cb)
                    (push (cons program args) runs)
                    (funcall cb (alist-get (cadr args) skip-status 1
                                           nil #'equal)
                             "" "")))
                 ((symbol-function 'ding) #'ignore))
         (setq ardentia-review-alert
               (lambda (tasks) (push (mapcar (lambda (tk) (plist-get tk :id))
                                             tasks)
                                     alerts)))
         ,@body))))

(ert-deftest ardentia-test-alert-defaults ()
  (should (null (default-value 'ardentia-review-alert)))
  (should (null (default-value 'ardentia-review-alert-ding)))
  (should (string-suffix-p "ai/tools/notify-skip"
                           (default-value
                            'ardentia-review-alert-skip-command))))

(ert-deftest ardentia-test-alert-baseline-silent ()
  (ardentia-test--with-alert nil
    (ardentia--review-alert-check (list (ardentia-test--rt "a")))
    (should-not alerts)
    (should-not runs)))

(ert-deftest ardentia-test-alert-new-task ()
  (ardentia-test--with-alert nil
    (ardentia--review-alert-check (list (ardentia-test--rt "a")))
    (ardentia--review-alert-check (list (ardentia-test--rt "a")
                                        (ardentia-test--rt "b")))
    (should (equal alerts '(("b"))))
    (ardentia--review-alert-check (list (ardentia-test--rt "a")
                                        (ardentia-test--rt "b")))
    (should (= (length alerts) 1))))

(ert-deftest ardentia-test-alert-returning-task-alerts-again ()
  (ardentia-test--with-alert nil
    (ardentia--review-alert-check (list (ardentia-test--rt "a")))
    (ardentia--review-alert-check nil)
    (ardentia--review-alert-check (list (ardentia-test--rt "a")))
    (should (equal alerts '(("a"))))))

(ert-deftest ardentia-test-alert-skip-exit-0 ()
  (ardentia-test--with-alert nil
    (setq skip-status '(("b" . 0)))
    (ardentia--review-alert-check nil)
    (ardentia--review-alert-check (list (ardentia-test--rt "a")
                                        (ardentia-test--rt "b")))
    (should (equal alerts '(("a"))))
    (should (equal (car (last (car runs))) "b"))
    (should (equal (cdr (car runs)) '("/o/r.org" "b")))))

(ert-deftest ardentia-test-alert-skip-all-no-alert ()
  (ardentia-test--with-alert nil
    (setq skip-status '(("b" . 0)))
    (ardentia--review-alert-check nil)
    (ardentia--review-alert-check (list (ardentia-test--rt "b")))
    (should-not alerts)))

(ert-deftest ardentia-test-alert-skip-exit-other-alerts ()
  (ardentia-test--with-alert nil
    (setq skip-status '(("a" . 1) ("b" . 2)))
    (ardentia--review-alert-check nil)
    (ardentia--review-alert-check (list (ardentia-test--rt "a")
                                        (ardentia-test--rt "b")))
    (should (equal alerts '(("a" "b"))))))

(ert-deftest ardentia-test-alert-no-id-not-checked ()
  (ardentia-test--with-alert nil
    (ardentia--review-alert-check nil)
    (ardentia--review-alert-check (list (ardentia-test--rt nil "Plain")))
    (should (= (length alerts) 1))
    (should-not runs)))

(ert-deftest ardentia-test-alert-missing-command-alerts ()
  (ardentia-test--with-alert ((ardentia-review-alert-skip-command
                               "/nonexistent/notify-skip"))
    (setq skip-status '(("a" . 0)))
    (ardentia--review-alert-check nil)
    (ardentia--review-alert-check (list (ardentia-test--rt "a")))
    (should (equal alerts '(("a"))))
    (should-not runs)))

(ert-deftest ardentia-test-alert-nil-command-alerts ()
  (ardentia-test--with-alert ((ardentia-review-alert-skip-command nil))
    (ardentia--review-alert-check nil)
    (ardentia--review-alert-check (list (ardentia-test--rt "a")))
    (should (equal alerts '(("a"))))
    (should-not runs)))

(ert-deftest ardentia-test-alert-off-does-nothing-keeps-baseline ()
  (ardentia-test--with-alert nil
    (setq ardentia-review-alert nil)
    (ardentia--review-alert-check nil)
    (ardentia--review-alert-check (list (ardentia-test--rt "a")))
    (should-not runs)
    (should-not alerts)
    (setq ardentia-review-alert (lambda (tasks) (push tasks alerts)))
    (ardentia--review-alert-check (list (ardentia-test--rt "a")))
    (should-not alerts)))

(ert-deftest ardentia-test-alert-message ()
  (ardentia-test--with-alert nil
    (setq ardentia-review-alert 'message)
    (let (msgs)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) msgs))))
        (ardentia--review-alert-check nil)
        (ardentia--review-alert-check (list (ardentia-test--rt "a" "One")))
        (ardentia--review-alert-check (list (ardentia-test--rt "a" "One")
                                            (ardentia-test--rt "b" "Two")
                                            (ardentia-test--rt "c" "Three")))
        (should (equal (reverse msgs)
                       '("ardentia: for review: One"
                         "ardentia: 2 new tasks for review: Two; Three")))))))

(ert-deftest ardentia-test-alert-ding ()
  (ardentia-test--with-alert nil
    (let ((ardentia-review-alert-ding t) dings)
      (cl-letf (((symbol-function 'ding) (lambda (&rest _) (push t dings))))
        (ardentia--review-alert-check nil)
        (ardentia--review-alert-check (list (ardentia-test--rt "a")))
        (should (= (length dings) 1))))))

(ert-deftest ardentia-test-alert-error-does-not-propagate ()
  (ardentia-test--with-alert nil
    (setq ardentia-review-alert (lambda (_tasks) (error "Boom")))
    (cl-letf (((symbol-function 'message) #'ignore))
      (ardentia--review-alert-check nil)
      (ardentia--review-alert-check (list (ardentia-test--rt "a")))
      (cl-letf (((symbol-function 'ardentia--task-key)
                 (lambda (_tk) (error "Boom"))))
        (ardentia--review-alert-check (list (ardentia-test--rt "z")))))))

(ert-deftest ardentia-test-alert-hook-registered ()
  (should (memq #'ardentia--review-alert-check
                ardentia-review-tasks-functions)))

(ert-deftest ardentia-test-alert-hook-from-refresh ()
  (let ((ardentia--state nil) (ardentia--review-count nil)
        (ardentia--refresh-callbacks nil) got
        (ardentia-review-tasks-functions nil))
    (add-hook 'ardentia-review-tasks-functions (lambda (r) (push r got)))
    (cl-letf (((symbol-function 'ardentia--fetch)
               (lambda (cb)
                 (funcall cb (list (ardentia-test--rt "a")
                                   (list :queue 'ready :id "r" :file "/o/x"))
                          nil)))
              ((symbol-function 'ardentia--fetch-host)
               (lambda (cb) (funcall cb "h")))
              ((symbol-function 'ardentia--fetch-timer-status)
               (lambda (cb) (funcall cb nil)))
              ((symbol-function 'ardentia--list-merges) #'ignore))
      (ardentia-refresh))
    (should (= (length got) 1))
    (should (equal (mapcar (lambda (tk) (plist-get tk :id)) (car got))
                   '("a")))
    (should (= ardentia--review-count 1))))

(ert-deftest ardentia-test-alert-hook-not-run-on-refresh-error ()
  (let ((ardentia--state nil) (ardentia--review-count nil)
        (ardentia--refresh-callbacks nil) got
        (ardentia-review-tasks-functions nil))
    (add-hook 'ardentia-review-tasks-functions (lambda (r) (push r got)))
    (cl-letf (((symbol-function 'ardentia--fetch)
               (lambda (cb) (funcall cb nil "failed")))
              ((symbol-function 'ardentia--fetch-host)
               (lambda (cb) (funcall cb "h")))
              ((symbol-function 'ardentia--fetch-timer-status)
               (lambda (cb) (funcall cb nil)))
              ((symbol-function 'ardentia--list-merges) #'ignore))
      (ardentia-refresh))
    (should-not got)))

(ert-deftest ardentia-test-alert-hook-from-mode-line-refresh ()
  (let ((ardentia--mode-line-busy nil) (ardentia--review-count nil)
        got (ardentia-review-tasks-functions nil))
    (add-hook 'ardentia-review-tasks-functions (lambda (r) (push r got)))
    (cl-letf (((symbol-function 'ardentia--fetch-tags)
               (lambda (_tags cb)
                 (funcall cb (list (ardentia-test--rt "a")
                                   (list :queue 'ready :id "r" :file "/o/x"))
                          nil)))
              ((symbol-function 'force-mode-line-update) #'ignore))
      (ardentia--mode-line-refresh))
    (should (= (length got) 1))
    (should (equal (mapcar (lambda (tk) (plist-get tk :id)) (car got))
                   '("a")))
    (should (= ardentia--review-count 1))))

(ert-deftest ardentia-test-alert-hook-not-run-on-mode-line-error ()
  (let ((ardentia--mode-line-busy nil) got
        (ardentia-review-tasks-functions nil))
    (add-hook 'ardentia-review-tasks-functions (lambda (r) (push r got)))
    (cl-letf (((symbol-function 'ardentia--fetch-tags)
               (lambda (_tags cb) (funcall cb nil "failed")))
              ((symbol-function 'message) #'ignore))
      (ardentia--mode-line-refresh))
    (should-not got)))

;;;; Run a task now

(defun ardentia-test--run-and-wait (command task)
  "Run the ready TASK with COMMAND; return the messages as a string."
  (let ((ardentia-run-task-command command) msgs proc)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when fmt (push (apply #'format fmt args) msgs)))))
      (setq proc (ardentia-run-task task))
      (ardentia-test--wait (lambda () (and (not (process-live-p proc)) msgs))))
    (mapconcat #'identity (reverse msgs) "\n")))

(defconst ardentia-test--ready-task
  (list :queue 'ready :title "Do it" :file "/o/a.org" :id "id-r" :tags '("aiready")))

(ert-deftest ardentia-test-run-substitute ()
  (should (equal (ardentia-run--substitute
                  '("p" "--e=%i" "%f" "a%ib%fc" "x") "ID1" "/d/f.org")
                 '("p" "--e=ID1" "/d/f.org" "aID1b/d/f.orgc" "x"))))

(ert-deftest ardentia-test-run-default-command ()
  (should (member "--setenv=ORG_AIREADY_ONLY_ID=%i" ardentia-run-task-command))
  (should (equal (car (last ardentia-run-task-command)) "%f")))

(ert-deftest ardentia-test-run-refusals ()
  (cl-letf (((symbol-function 'y-or-n-p)
             (lambda (&rest _) (ert-fail "must not ask"))))
    (let ((ardentia-run-task-command '("true")) (ardentia--local-host "here"))
      (should-error (ardentia-run-task
                     (plist-put (copy-sequence ardentia-test--ready-task)
                                :queue 'review))
                    :type 'user-error)
      (let ((err (should-error
                  (ardentia-run-task
                   (plist-put (copy-sequence ardentia-test--ready-task)
                              :tags '("aiready" "other_only")))
                  :type 'user-error)))
        (should (equal (cadr err) "Task belongs to host other"))))
    (let ((ardentia-run-task-command nil))
      (should-error (ardentia-run-task ardentia-test--ready-task)
                    :type 'user-error))))

(ert-deftest ardentia-test-run-unknown-local-host-does-not-block ()
  (let ((ardentia--local-host nil) (ardentia--state nil))
    (should-not (ardentia-run--check-host '("aiready" "other_only")))))

(ert-deftest ardentia-test-run-own-host-and-later-pass ()
  (let ((ardentia--local-host "here"))
    (should-not (ardentia-run--check-host '("aiready" "here_only")))
    (should (string-match-p
             "runner started for \"Do it\""
             (ardentia-test--run-and-wait
              '("true")
              (plist-put (copy-sequence ardentia-test--ready-task)
                         :tags '("aiready" "here_only")))))))

(ert-deftest ardentia-test-run-success-and-failure ()
  (let ((ardentia--local-host nil))
    (should (string-match-p "runner started for \"Do it\""
                            (ardentia-test--run-and-wait
                             '("true") ardentia-test--ready-task)))
    (let ((msg (ardentia-test--run-and-wait
                '("false") ardentia-test--ready-task)))
      (should (string-match-p "failed (exit 1)" msg))
      (should (string-match-p (regexp-quote ardentia-run--buffer-name) msg)))))

(ert-deftest ardentia-test-run-passes-id-and-file ()
  (let* ((out (make-temp-file "ardentia-run-out"))
         (ardentia--local-host nil))
    (unwind-protect
        (progn
          (ardentia-test--run-and-wait
           (list "sh" "-c" "echo \"$0|$1\" > \"$2\"" "%i" "%f" out)
           ardentia-test--ready-task)
          (should (equal (string-trim
                          (with-temp-buffer (insert-file-contents out)
                                            (buffer-string)))
                         "id-r|/o/a.org")))
      (delete-file out))))

(ert-deftest ardentia-test-run-declined ()
  (let ((ardentia-run-task-command '("true")) (ardentia--local-host nil))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (should-not (ardentia-run-task ardentia-test--ready-task)))))

(ert-deftest ardentia-test-run-dashboard-key ()
  (should (eq (lookup-key ardentia-dashboard-mode-map "x")
              #'ardentia-dashboard-run)))

(ert-deftest ardentia-test-run-at-point-refusals ()
  (let ((ardentia-run-task-command '("true")))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (&rest _) (ert-fail "must not ask"))))
      (dolist (text '("* TODO Plain\n"
                      "* DONE Closed :aiready:\n"
                      "* TODO Other :aiready:other_only:\n"))
        (ardentia-test--with-org-file f text
          (with-current-buffer (find-file-noselect f)
            (let ((ardentia--local-host "here"))
              (should-error (ardentia-run-task-at-point) :type 'user-error))))))))

(ert-deftest ardentia-test-run-at-point-creates-id-and-saves ()
  (ardentia-test--with-org-file f "* TODO Go :aiready:\nbody\n"
    (let ((ardentia-run-task-command
           (list "sh" "-c" "test -f \"$0\"" "%f"))
          (ardentia--local-host nil) msgs proc)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (push (apply #'format fmt args) msgs))))
                ((symbol-function 'make-process)
                 (let ((orig (symbol-function 'make-process)))
                   (lambda (&rest args)
                     (setq proc (apply orig args))))))
        (with-current-buffer (find-file-noselect f)
          (goto-char (point-max))
          (ardentia-run-task-at-point)
          (should-not (buffer-modified-p)))
        (ardentia-test--wait (lambda () (and proc (not (process-live-p proc)) msgs))))
      (should (string-match-p "^:ID: +[-0-9a-f]+$"
                              (ardentia-test--file-text f)))
      (should (string-match-p "runner started" (car msgs))))))

(ert-deftest ardentia-test-run-at-point-asks-before-saving ()
  (ardentia-test--with-org-file f "* TODO Go :aiready:\n:PROPERTIES:\n:ID: x-1\n:END:\n"
    (let ((ardentia-run-task-command '("true")) (asked nil))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt)
                   (push prompt asked)
                   (not (string-match-p "Save" prompt)))))
        (with-current-buffer (find-file-noselect f)
          (goto-char (point-max))
          (insert "edit\n")
          (should-error (ardentia-run-task-at-point) :type 'user-error)
          (should (buffer-modified-p))
          (should (seq-some (lambda (p) (string-match-p "Save" p)) asked)))))))


;;;; Stop a running task

(defconst ardentia-test--running-task
  (list :queue 'running :title "Busy" :file "/o/a.org" :id "id-k"
        :tags '("ai_in_progress")
        :props '(("AI_CLAIMED_BY" . "here"))))

(defun ardentia-test--stop-and-wait (command task)
  "Stop the running TASK with COMMAND; return the messages as a string."
  (let ((ardentia-stop-task-command command) (refreshed 0) msgs proc)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'ardentia-run--refresh)
               (lambda () (setq refreshed (1+ refreshed))))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when fmt (push (apply #'format fmt args) msgs)))))
      (setq proc (ardentia-stop-task task))
      (ardentia-test--wait (lambda () (and (not (process-live-p proc)) msgs))))
    (cons refreshed (mapconcat #'identity (reverse msgs) "\n"))))

(ert-deftest ardentia-test-stop-refuses-not-running ()
  (cl-letf (((symbol-function 'y-or-n-p)
             (lambda (&rest _) (ert-fail "must not ask"))))
    (let ((ardentia-stop-task-command '("true")) (ardentia--local-host nil))
      (should-error (ardentia-stop-task
                     (plist-put (copy-sequence ardentia-test--running-task)
                                :queue 'review))
                    :type 'user-error))))

(ert-deftest ardentia-test-stop-refuses-other-host ()
  (cl-letf (((symbol-function 'y-or-n-p)
             (lambda (&rest _) (ert-fail "must not ask"))))
    (let ((ardentia-stop-task-command '("true")) (ardentia--local-host "there"))
      (let ((err (should-error (ardentia-stop-task ardentia-test--running-task)
                               :type 'user-error)))
        (should (string-match-p "Claimed by here" (cadr err)))))))

(ert-deftest ardentia-test-stop-unknown-local-host-allowed ()
  (let ((ardentia--local-host nil) (ardentia--state nil))
    (should (string-match-p
             "stopped \"Busy\""
             (cdr (ardentia-test--stop-and-wait
                   '("true") ardentia-test--running-task))))))

(ert-deftest ardentia-test-stop-command-nil-and-no-id ()
  (let ((ardentia-stop-task-command nil))
    (should-error (ardentia-stop-task ardentia-test--running-task)
                  :type 'user-error))
  (let ((ardentia-stop-task-command '("true")) (ardentia--local-host nil))
    (should-error (ardentia-stop-task
                   (plist-put (copy-sequence ardentia-test--running-task)
                              :id nil))
                  :type 'user-error)))

(ert-deftest ardentia-test-stop-declined ()
  (let ((ardentia-stop-task-command '("true")) (ardentia--local-host nil))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (should-not (ardentia-stop-task ardentia-test--running-task)))))

(ert-deftest ardentia-test-stop-default-command ()
  (should (equal (car (last ardentia-stop-task-command)) "%i"))
  (should (string-suffix-p "ai/tools/aiready-stop"
                           (car ardentia-stop-task-command))))

(ert-deftest ardentia-test-stop-passes-id ()
  (let* ((out (make-temp-file "ardentia-stop-out"))
         (ardentia--local-host nil))
    (unwind-protect
        (progn
          (ardentia-test--stop-and-wait
           (list "sh" "-c" "printf '%s' \"$1\" > \"$2\"" "sh" "%i" out)
           ardentia-test--running-task)
          (should (equal (with-temp-buffer (insert-file-contents out)
                                           (buffer-string))
                         "id-k")))
      (delete-file out))))

(ert-deftest ardentia-test-stop-sentinel-messages ()
  (let ((ardentia--local-host nil))
    (pcase-dolist (`(,code ,regexp ,refreshes)
                   '((0 "stopped \"Busy\"" 1)
                     (3 "not running on this computer" 0)
                     (4 "signal sent.*still closing" 1)
                     (2 "failed (exit 2), see buffer" 0)))
      (let ((res (ardentia-test--stop-and-wait
                  (list "sh" "-c" (format "exit %d" code))
                  ardentia-test--running-task)))
        (should (string-match-p regexp (cdr res)))
        (should (= (car res) refreshes))))))

(ert-deftest ardentia-test-stop-keys ()
  (should (eq (keymap-lookup ardentia-dashboard-mode-map "k")
              #'ardentia-dashboard-stop))
  (should (eq (keymap-lookup ardentia-session-mode-map "k")
              #'ardentia-session-stop)))

(ert-deftest ardentia-test-stop-session-refuses-not-running ()
  (with-temp-buffer
    (let ((ardentia--state nil)
          (ardentia-session--task (list :queue 'review :title "T")))
      (should-error (ardentia-session-stop) :type 'user-error))))

;;;; Async fetch with a fake org-tool

(defun ardentia-test--fake-env (dir &optional no-multi-tag)
  "Create the fake org directory with tools in DIR.
The fake org-tool answers a `a|b|c' tag list with the combined output,
or, with NO-MULTI-TAG, fails on it like an org-tool that predates the form."
  (make-directory (expand-file-name "ai/tools" dir) t)
  (dolist (f '("a.org" "b.org" "c.sync-conflict-20261010-ABC.org" "notes.txt"))
    (write-region "" nil (expand-file-name f dir)))
  (let ((tool (expand-file-name "ai/tools/org-tool" dir))
        (log (expand-file-name "args.log" dir)))
    (with-temp-file tool
      (insert "#!/bin/sh\n"
              "echo \"$*\" >> " log "\n"
              "case \"$2\" in\n"
              (if no-multi-tag
                  "*'|'*) echo 'Invalid tag' >&2; exit 1 ;;\n"
                (concat "*'|'*) cat <<'EOF'\n"
                        "[" (string-trim (string-trim ardentia-test--json "\\[" "\\]"))
                        ","
                        "{\"file\":\"/o/b.org\",\"line\":5,\"level\":1,\"todo\":\"null\",\"title\":\"Plain\","
                        "\"tags\":[\"aiready\"],\"id\":\"null\",\"created\":\"2026-10-09 08:00\",\"done\":false,"
                        "\"properties\":{}}]\nEOF\n;;\n"))
              "humanreview) cat <<'EOF'\n" ardentia-test--json "\nEOF\n;;\n"
              "aiready) cat <<'EOF'\n"
              "[{\"file\":\"/o/b.org\",\"line\":5,\"level\":1,\"todo\":\"null\",\"title\":\"Plain\","
              "\"tags\":[\"aiready\"],\"id\":\"null\",\"created\":\"2026-10-09 08:00\",\"done\":false,"
              "\"properties\":{}}]\nEOF\n;;\n"
              "ai_in_progress) echo '[]' ;;\n"
              "esac\n"))
    (set-file-modes tool #o755)
    (with-temp-file (expand-file-name "ai/tools/aiready-host" dir)
      (insert "#!/bin/sh\necho desktop\n"))
    (set-file-modes (expand-file-name "ai/tools/aiready-host" dir) #o755)
    (with-temp-file (expand-file-name "ai/tools/fail-tool" dir)
      (insert "#!/bin/sh\necho boom >&2\nexit 3\n"))
    (set-file-modes (expand-file-name "ai/tools/fail-tool" dir) #o755)))

(ert-deftest ardentia-test-fetch-async ()
  (let* ((dir (make-temp-file "ardentia-fake-" t))
         (ardentia-org-directory dir)
         (ardentia-org-tool (expand-file-name "ai/tools/org-tool" dir))
         got-tasks got-err done)
    (unwind-protect
        (progn
          (ardentia-test--fake-env dir)
          (ardentia--fetch (lambda (tasks err)
                             (setq got-tasks tasks got-err err done t)))
          (should-not done)                 ; asynchronous
          (ardentia-test--wait (lambda () done))
          (should done)
          (should (null got-err))
          ;; aiready (1) + humanreview (1 open, 1 done dropped; the aiready entry
          ;; of b.org line 5 is not duplicated) = 2
          (should (= (length got-tasks) 2))
          ;; the order is the one org-tool printed (one call, document order)
          (should (equal (mapcar (lambda (x) (plist-get x :queue)) got-tasks)
                         '(review ready)))
          (let ((log (with-temp-buffer
                       (insert-file-contents (expand-file-name "args.log" dir))
                       (buffer-string))))
            ;; one combined call, no per-tag calls
            (should (= (length (split-string log "\n" t)) 1))
            (should (string-match-p
                     "find-tagged aiready|ai_in_progress|humanreview|auto_error|syncmerge|gitsync .*a\\.org" log))
            (should-not (string-match-p "sync-conflict" log))
            (should-not (string-match-p "notes\\.txt" log))))
      (delete-directory dir t))))

(ert-deftest ardentia-test-fetch-falls-back-to-per-tag-calls ()
  (let* ((dir (make-temp-file "ardentia-fake-" t))
         (ardentia-org-directory dir)
         (ardentia-org-tool (expand-file-name "ai/tools/org-tool" dir))
         got-tasks got-err done)
    (unwind-protect
        (progn
          (ardentia-test--fake-env dir t)
          (ardentia--fetch (lambda (tasks err)
                             (setq got-tasks tasks got-err err done t)))
          (ardentia-test--wait (lambda () done))
          (should (null got-err))
          (should (= (length got-tasks) 2))
          (should (equal (mapcar (lambda (x) (plist-get x :queue)) got-tasks)
                         '(ready review)))
          (let ((log (with-temp-buffer
                       (insert-file-contents (expand-file-name "args.log" dir))
                       (buffer-string))))
            ;; the combined call first, then one call per tag
            (should (string-match-p
                     "find-tagged aiready|ai_in_progress|humanreview|auto_error|syncmerge|gitsync"
                     log))
            (should (string-match-p "find-tagged gitsync .*a\\.org" log))
            (should (string-match-p "find-tagged humanreview .*a\\.org" log))
            (should (string-match-p "find-tagged aiready .*a\\.org" log))
            (should (string-match-p "find-tagged ai_in_progress .*a\\.org" log))))
      (delete-directory dir t))))

(ert-deftest ardentia-test-fetch-failure-reported ()
  (let* ((dir (make-temp-file "ardentia-fake-" t))
         (ardentia-org-directory dir)
         (ardentia-org-tool (expand-file-name "ai/tools/fail-tool" dir))
         got-err done)
    (unwind-protect
        (progn
          (ardentia-test--fake-env dir)
          (ardentia--fetch (lambda (_tasks err) (setq got-err err done t)))
          (ardentia-test--wait (lambda () done))
          (should (string-match-p "failed: boom" got-err)))
      (delete-directory dir t))))

(ert-deftest ardentia-test-fetch-missing-program ()
  (let* ((dir (make-temp-file "ardentia-fake-" t))
         (ardentia-org-directory dir)
         (ardentia-org-tool (expand-file-name "nonexistent" dir))
         got-err done)
    (unwind-protect
        (progn
          (ardentia-test--fake-env dir)
          (ardentia--fetch (lambda (_tasks err) (setq got-err err done t)))
          (ardentia-test--wait (lambda () done))
          (should (stringp got-err)))
      (delete-directory dir t))))

(ert-deftest ardentia-test-fetch-host ()
  (let* ((dir (make-temp-file "ardentia-fake-" t))
         (ardentia-org-directory dir)
         (ardentia-host-command (expand-file-name "ai/tools/aiready-host" dir))
         (ardentia--local-host nil)
         got done)
    (unwind-protect
        (progn
          (ardentia-test--fake-env dir)
          (ardentia--fetch-host (lambda (h) (setq got h done t)))
          (ardentia-test--wait (lambda () done))
          (should (equal got "desktop"))
          (should (equal ardentia--local-host "desktop")))
      (delete-directory dir t))))

(ert-deftest ardentia-test-refresh-state ()
  (let* ((dir (make-temp-file "ardentia-fake-" t))
         (ardentia-org-directory dir)
         (ardentia-org-tool (expand-file-name "ai/tools/org-tool" dir))
         (ardentia-host-command (expand-file-name "ai/tools/aiready-host" dir))
         (ardentia-timer-unit "ardentia-no-such.timer")
         (ardentia--local-host nil) (ardentia--state nil)
         (ardentia--review-count nil) (ardentia--refresh-callbacks nil)
         done)
    (unwind-protect
        (progn
          (ardentia-test--fake-env dir)
          (ardentia-refresh (lambda () (setq done t)))
          (ardentia-refresh)               ; joins the running refresh
          (ardentia-test--wait (lambda () done))
          (should done)
          (should (equal (plist-get ardentia--state :host) "desktop"))
          (should (= (length (plist-get ardentia--state :tasks)) 2))
          (should (= ardentia--review-count 1))
          (should-not (plist-get ardentia--state :refreshing)))
      (delete-directory dir t))))

;;;; Usage and completed overview

(ert-deftest ardentia-test-keep-done ()
  (let* ((json (ardentia--parse-json ardentia-test--json))
         (kept (ardentia--tasks-from-json json t)))
    (should (= (length (ardentia--tasks-from-json json)) 2))
    (should (= (length kept) 3))
    (should (equal (mapcar (lambda (x) (plist-get x :done)) kept)
                   '(nil t nil)))
    (should (equal (plist-get (nth 1 kept) :title) "Finished"))
    (should (null (plist-get (car (ardentia--tasks-from-json json)) :done)))))

(ert-deftest ardentia-test-parse-number ()
  (should (= (ardentia--parse-number "2.19") 2.19))
  (should (= (ardentia--parse-number "5400000") 5400000))
  (should (= (ardentia--parse-number 7) 7))
  (should (null (ardentia--parse-number nil)))
  (should (null (ardentia--parse-number "null")))
  (should (null (ardentia--parse-number "")))
  (should (null (ardentia--parse-number "12abc")))
  (should (null (ardentia--parse-number "-3")))
  (should (null (ardentia--parse-number "1e3"))))

(ert-deftest ardentia-test-format-tokens ()
  (should (equal (ardentia--format-tokens "950") "950"))
  (should (equal (ardentia--format-tokens "0") "0"))
  (should (equal (ardentia--format-tokens "1000") "1.0k"))
  (should (equal (ardentia--format-tokens "12345") "12.3k"))
  (should (equal (ardentia--format-tokens "999999") "1.0M"))
  (should (equal (ardentia--format-tokens "5400000") "5.4M"))
  (should (equal (ardentia--format-tokens nil) ""))
  (should (equal (ardentia--format-tokens "null") ""))
  (should (equal (ardentia--format-tokens "lots") "")))

(ert-deftest ardentia-test-format-cost ()
  (should (equal (ardentia--format-cost "4.09") "$4.09"))
  (should (equal (ardentia--format-cost "2.1") "$2.10"))
  (should (equal (ardentia--format-cost "3") "$3.00"))
  (should (equal (ardentia--format-cost "0") "$0.00"))
  (should (equal (ardentia--format-cost nil) ""))
  (should (equal (ardentia--format-cost "null") ""))
  (should (equal (ardentia--format-cost "n/a") "")))

(ert-deftest ardentia-test-usage-line ()
  (should (equal (ardentia--usage-line
                  '(("AI_TOKENS_TOTAL" . "5400000") ("AI_COST_USD_TOTAL" . "4.09")
                    ("AI_USAGE_RUNS" . "2") ("AI_TOKENS" . "3000000")
                    ("AI_COST_USD" . "2.19")))
                 "5.4M tokens, $4.09 over 2 runs (last run 3.0M tokens, $2.19)"))
  (should (equal (ardentia--usage-line
                  '(("AI_TOKENS_TOTAL" . "950") ("AI_USAGE_RUNS" . "1")))
                 "950 tokens over 1 run"))
  (should (equal (ardentia--usage-line '(("AI_COST_USD" . "1.5")))
                 "last run $1.50"))
  (should (null (ardentia--usage-line nil)))
  (should (null (ardentia--usage-line '(("AI_STATUS" . "completed")
                                        ("AI_TOKENS_TOTAL" . "null"))))))

(ert-deftest ardentia-test-review-usage-line ()
  (let ((text (ardentia-review--render-string
               (list :title "T" :file "/o/a.org" :tags nil
                     :props '(("AI_STATUS" . "completed")
                              ("AI_TOKENS_TOTAL" . "5400000")
                              ("AI_COST_USD_TOTAL" . "4.09")
                              ("AI_USAGE_RUNS" . "2"))
                     :body ""))))
    (should (string-match-p
             "\\*\\* Agent properties\n- Usage :: 5.4M tokens, \\$4.09 over 2 runs\n- AI_STATUS"
             text)))
  (should-not (string-match-p
               "Usage ::"
               (ardentia-review--render-string
                (list :title "T" :file "/o/a.org" :tags nil
                      :props '(("AI_STATUS" . "completed")) :body "")))))

(defconst ardentia-test--now "20261010-1200"
  "Reference time of the completed view tests.")

(ert-deftest ardentia-test-completed-period ()
  (let* ((now (ardentia-test--t ardentia-test--now))
         (at (lambda (stamp) (ardentia-test--task
                              :props `(("AI_SESSION" . ,stamp)))))
         (none (ardentia-test--task)))
    (should (equal (mapcar #'ardentia-completed--period-label
                           ardentia-completed--periods)
                   '("all" "last 30 days" "last 7 days" "today")))
    (should (eq (ardentia-completed--next-period 'all) 'last-30-days))
    (should (eq (ardentia-completed--next-period 'today) 'all))
    (should (ardentia-completed--in-period-p none 'all now))
    (should-not (ardentia-completed--in-period-p none 'today now))
    (should-not (ardentia-completed--in-period-p none 'last-7-days now))
    ;; today: from 00:00 on, inclusive
    (should (ardentia-completed--in-period-p (funcall at "20261010-0000") 'today now))
    (should-not (ardentia-completed--in-period-p (funcall at "20261009-2359") 'today now))
    ;; 7 days: exactly 7 days before NOW is still inside
    (should (ardentia-completed--in-period-p (funcall at "20261003-1200") 'last-7-days now))
    (should-not (ardentia-completed--in-period-p (funcall at "20261003-1159") 'last-7-days now))
    (should (ardentia-completed--in-period-p (funcall at "20260910-1200") 'last-30-days now))
    (should-not (ardentia-completed--in-period-p (funcall at "20260910-1159") 'last-30-days now))
    (should-not (ardentia-completed--in-period-p
                 (ardentia-test--task :props '(("AI_SESSION" . "garbage")))
                 'last-30-days now))))

(ert-deftest ardentia-test-completed-select-and-sort ()
  (let* ((now (ardentia-test--t ardentia-test--now))
         (mk (lambda (title stamp done)
               (ardentia-test--task :title title :done done
                                    :props (and stamp `(("AI_SESSION" . ,stamp)))))))
    (let ((tasks (list (funcall mk "old" "20260101-0800" t)
                       (funcall mk "none" nil nil)
                       (funcall mk "new" "20261010-0900" nil)
                       (funcall mk "mid" "20261008-0900" t))))
      (should (equal (mapcar (lambda (e) (plist-get (car e) :title))
                             (ardentia-completed--entries tasks 'all nil now))
                     '("new" "mid" "old" "none")))
      (should (equal (mapcar (lambda (e) (plist-get (car e) :title))
                             (ardentia-completed--entries tasks 'last-7-days nil now))
                     '("new" "mid")))
      (should (equal (mapcar (lambda (e) (plist-get (car e) :title))
                             (ardentia-completed--entries tasks 'all 'review now))
                     '("new" "none")))
      (should (equal (mapcar (lambda (e) (plist-get (car e) :title))
                             (ardentia-completed--entries tasks 'all 'accepted now))
                     '("mid" "old")))
      (should (equal (mapcar (lambda (e) (plist-get (car e) :title))
                             (ardentia-completed--entries tasks 'all 'all now))
                     '("new" "mid" "old" "none")))))
  (should (equal (mapcar #'ardentia-completed--state-label
                         ardentia-completed--states)
                 '("all" "waiting for review" "accepted")))
  (should (eq (ardentia-completed--next-state 'all) 'review))
  (should (eq (ardentia-completed--next-state 'accepted) 'all)))

(ert-deftest ardentia-test-completed-totals ()
  (let* ((a (ardentia-test--task
             :props '(("AI_STATUS" . "completed") ("AI_TOKENS_TOTAL" . "1000000")
                      ("AI_COST_USD_TOTAL" . "1.50"))))
         (b (ardentia-test--task
             :props '(("AI_STATUS" . "blocked") ("AI_TOKENS_TOTAL" . "500")
                      ("AI_COST_USD_TOTAL" . "null"))))
         (c (ardentia-test--task :props '(("AI_STATUS" . "partial"))))
         (d (ardentia-test--task))
         (tot (ardentia-completed--totals (list a b c d))))
    (should (= (plist-get tot :count) 4))
    (should (= (plist-get tot :completed) 1))
    (should (= (plist-get tot :blocked) 1))
    (should (= (plist-get tot :partial) 1))
    (should (= (plist-get tot :tokens) 1000500))
    (should (= (plist-get tot :cost) 1.5))
    (should (= (plist-get tot :with-usage) 2))
    (should (equal (ardentia-completed--summary (list a b c d))
                   " 4 tasks (1 completed, 1 blocked, 1 partial) | 1.0M tokens, $1.50 (2 with usage data)"))
    (should (equal (ardentia-completed--summary (list c))
                   " 1 task (0 completed, 0 blocked, 1 partial) | no usage data"))
    (should (= (plist-get (ardentia-completed--totals nil) :count) 0))))

(ert-deftest ardentia-test-completed-row ()
  (should (equal (ardentia-completed--row
                  (ardentia-test--task
                   :todo "DONE" :title "Hi" :file "/o/a.org"
                   :props '(("AI_SESSION" . "20261010-1953") ("AI_STATUS" . "completed")
                            ("AI_CLAIMED_BY" . "alpha") ("AI_USAGE_RUNS" . "2")
                            ("AI_TOKENS_TOTAL" . "5400000")
                            ("AI_COST_USD_TOTAL" . "4.09"))))
                 ["2026-10-10 19:53" "DONE" "completed" "alpha" "2" "5.4M" "$4.09"
                  "a.org" "Hi"]))
  (should (equal (ardentia-completed--row
                  (ardentia-test--task :todo "TODO" :title "Old" :file "/o/a.org"))
                 ["" "TODO" "" "" "" "" "" "a.org" "Old"])))

(ert-deftest ardentia-test-completed-number-sort ()
  (let ((pred (ardentia-completed--number-less #'ardentia--task-tokens-total))
        (small (list (ardentia-test--task :props '(("AI_TOKENS_TOTAL" . "950")))))
        (big (list (ardentia-test--task :props '(("AI_TOKENS_TOTAL" . "12300")))))
        (none (list (ardentia-test--task))))
    ;; 950 < 12300 numerically (a string sort would say otherwise)
    (should (funcall pred small big))
    (should-not (funcall pred big small))
    (should (funcall pred none small))
    (should-not (funcall pred none none))))

(ert-deftest ardentia-test-completed-fetch-async ()
  (let* ((dir (make-temp-file "ardentia-fake-" t))
         (ardentia-org-directory dir)
         (ardentia-org-tool (expand-file-name "ai/tools/org-tool" dir))
         got-tasks got-err done)
    (unwind-protect
        (progn
          (ardentia-test--fake-env dir)
          (ardentia-completed--fetch (lambda (tasks err)
                                       (setq got-tasks tasks got-err err done t)))
          (should-not done)
          (ardentia-test--wait (lambda () done))
          (should (null got-err))
          ;; the canned output holds the open and the done task (plus one
          ;; untagged element the fake returns as is); aiready is not asked
          (should (equal (mapcar (lambda (x) (plist-get x :title)) got-tasks)
                         '("Review me" "Finished" "Plain")))
          (should (equal (mapcar (lambda (x) (plist-get x :done)) got-tasks)
                         '(nil t nil)))
          (let ((log (with-temp-buffer
                       (insert-file-contents (expand-file-name "args.log" dir))
                       (buffer-string))))
            (should (string-match-p "find-tagged humanreview .*a\\.org" log))
            (should-not (string-match-p "aiready" log))))
      (delete-directory dir t))))

(ert-deftest ardentia-test-completed-fetch-failure ()
  (let* ((dir (make-temp-file "ardentia-fake-" t))
         (ardentia-org-directory dir)
         (ardentia-org-tool (expand-file-name "ai/tools/fail-tool" dir))
         got-tasks got-err done)
    (unwind-protect
        (progn
          (ardentia-test--fake-env dir)
          (ardentia-completed--fetch (lambda (tasks err)
                                       (setq got-tasks tasks got-err err done t)))
          (ardentia-test--wait (lambda () done))
          (should (null got-tasks))
          (should (string-match-p "failed: boom" got-err)))
      (delete-directory dir t))))

;;;; Session transcripts

(defvar agent-shell-session-restore-verbosity)

(defconst ardentia-test--sid-1 "session-one")
(defconst ardentia-test--sid-2 "session-two")
(defconst ardentia-test--sid-3 "session-three")

(defun ardentia-test--json-line (rec)
  "Return REC (an alist) as one JSON line."
  (concat (json-serialize rec) "\n"))

(defun ardentia-test--write (file text &optional append mtime)
  "Write TEXT to FILE (append with APPEND); set MTIME (a time) if given."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'utf-8))
    (write-region text nil file append 'silent))
  (when mtime (set-file-times file mtime)))

(defun ardentia-test--prompt-rec (id &optional ts)
  "Return the first user record of a session for the task ID."
  `((type . "user") (timestamp . ,(or ts "2026-10-10T10:00:00.000Z"))
    (message . ((role . "user")
                (content . ,(format "Process aiready task FILE=/o/x.org ID=%s" id))))))

(defun ardentia-test--assistant-rec (ts &rest blocks)
  "Return an assistant record at TS with content BLOCKS."
  `((type . "assistant") (timestamp . ,ts)
    (message . ((role . "assistant") (content . ,(vconcat blocks))))))

(defun ardentia-test--result-rec (ts content &optional err)
  "Return a user record at TS holding a tool result with CONTENT."
  `((type . "user") (timestamp . ,ts)
    (message . ((role . "user")
                (content . [((type . "tool_result") (tool_use_id . "toolu_1")
                             (is_error . ,(if err t :false))
                             (content . ,content))])))))

(defmacro ardentia-test--with-sessions (dir &rest body)
  "Run BODY with DIR a temporary transcript directory."
  (declare (indent 1))
  `(let* ((,dir (make-temp-file "ardentia-sess-" t))
          (ardentia-session-directory ,dir)
          (ardentia--state nil)
          (message-log-max nil))
     (unwind-protect (progn ,@body)
       (dolist (b (ardentia-session--live-buffers)) (kill-buffer b))
       (ardentia-session--cancel-timer)
       (delete-directory ,dir t))))

(ert-deftest ardentia-test-session-slug ()
  (should (equal (ardentia-session--slug "/nonexistent-xyz/my.org dir/")
                 "-nonexistent-xyz-my-org-dir"))
  (let* ((real (make-temp-file "ardentia-real-" t))
         (link (concat real "-link")))
    (unwind-protect
        (progn
          (make-symbolic-link real link)
          (should (equal (ardentia-session--slug link)
                         (ardentia-session--slug real))))
      (delete-file link)
      (delete-directory real t))))

(ert-deftest ardentia-test-session-directory-derived ()
  (let ((ardentia-session-directory nil)
        (ardentia-org-directory "/nonexistent-xyz/org/"))
    (should (equal (ardentia-session--dir)
                   (expand-file-name "-nonexistent-xyz-org"
                                     "~/.claude/projects/")))))

(ert-deftest ardentia-test-session-find ()
  (ardentia-test--with-sessions dir
    (let* ((now (current-time))
           (old (time-subtract now 3600))
           (a (expand-file-name (concat ardentia-test--sid-1 ".jsonl") dir))
           (b (expand-file-name (concat ardentia-test--sid-2 ".jsonl") dir))
           (c (expand-file-name (concat ardentia-test--sid-3 ".jsonl") dir))
           (task (list :id "id-1" :title "T" :file "/o/x.org")))
      (ardentia-test--write a (ardentia-test--json-line (ardentia-test--prompt-rec "id-1")) nil old)
      (ardentia-test--write b (ardentia-test--json-line (ardentia-test--prompt-rec "id-1")) nil now)
      (ardentia-test--write c (ardentia-test--json-line (ardentia-test--prompt-rec "id-10")) nil now)
      ;; Newest wins; the other task's session (id-10) is not a match.
      (should (equal (ardentia-session-find task) b))
      (should (equal (ardentia-session-find (list :id "id-10")) c))
      (should-not (ardentia-session-find (list :id "id-2")))
      (should-not (ardentia-session-find (list :title "no id")))
      ;; Running: only files newer than AI_CLAIMED minus 5 minutes.
      (let* ((claimed (format-time-string "%Y%m%d-%H%M" (time-subtract now 600)))
             (running (list :id "id-1" :queue 'running
                            :props `(("AI_CLAIMED" . ,claimed)))))
        (should (equal (ardentia-session-find running) b))
        (delete-file b)
        (should-not (ardentia-session-find running))
        (should (equal (ardentia-session-find task) a))))))

(ert-deftest ardentia-test-session-entry-navigation ()
  (with-temp-buffer
    (ardentia-session-mode)
    (let ((inhibit-read-only t) heads)
      (dolist (e '(("10:00:01" "" "one\ntwo")
                   ("10:00:02" "[qa]" "three\nfour")
                   ("--:--:--" "" "five")))
        (push (point) heads)
        (insert (ardentia-session--entry (nth 0 e) (nth 1 e) "" (nth 2 e)
                                         'default)))
      (setq heads (nreverse heads))
      (let ((h1 (nth 0 heads)) (h2 (nth 1 heads)) (h3 (nth 2 heads)))
        (goto-char (point-min))
        (ardentia-session-next-entry)
        (should (= (point) h2))
        (ardentia-session-next-entry)
        (should (= (point) h3))
        (goto-char (+ h1 4))
        (ardentia-session-next-entry)
        (should (= (point) h2))
        (goto-char (1+ h1))
        (forward-line 1)
        (ardentia-session-next-entry)
        (should (= (point) h2))
        (forward-line 1)
        (ardentia-session-previous-entry)
        (should (= (point) h2))
        (ardentia-session-previous-entry)
        (should (= (point) h1))
        (goto-char h1)
        (ardentia-session-next-entry 2)
        (should (= (point) h3))
        (ardentia-session-previous-entry 2)
        (should (= (point) h1))
        (should-error (ardentia-session-previous-entry) :type 'user-error)
        (should (= (point) h1))
        (goto-char h3)
        (forward-line 1)
        (let ((p (point)))
          (should-error (ardentia-session-next-entry) :type 'user-error)
          (should (= (point) p)))
        (goto-char h3)
        (should-error (ardentia-session-next-entry) :type 'user-error)
        (should (= (point) h3))))
    (should (eq (key-binding (kbd "C-c C-n")) 'ardentia-session-next-entry))
    (should (eq (key-binding (kbd "C-c C-p"))
                'ardentia-session-previous-entry))))

(ert-deftest ardentia-test-session-render-records ()
  (let ((ardentia-session-result-lines 2))
    (let ((s (ardentia-session--render-record
              (ardentia-test--prompt-rec "id-1" "2026-10-10T10:00:00.000Z") "")))
      (should (string-match-p "\\`[0-9:-]\\{8\\} Prompt: Process aiready task" s)))
    (let ((s (ardentia-session--render-record
              (ardentia-test--assistant-rec
               "2026-10-10T10:00:01.000Z"
               '((type . "thinking") (thinking . ""))
               '((type . "text") (text . "Hello\nworld"))
               '((type . "tool_use") (id . "toolu_1") (name . "Bash")
                 (input . ((command . "ls -l\necho  hi") (description . "List files"))))
               '((type . "tool_use") (id . "toolu_2") (name . "Agent")
                 (input . ((description . "Check it") (subagent_type . "qa")
                           (prompt . "Do it")))))
              "")))
      (should (string-match-p "Hello\n +world" s))
      (should (string-match-p "^[0-9:-]+ > Bash: List files$" s))
      (should (string-match-p "> Agent (qa): Check it" s))
      (should-not (string-match-p "thinking" s)))
    ;; Without description the command is used, on one line.
    (should (string-match-p
             "> Bash: ls -l echo hi"
             (ardentia-session--render-record
              (ardentia-test--assistant-rec
               "2026-10-10T10:00:01.000Z"
               '((type . "tool_use") (name . "Bash")
                 (input . ((command . "ls -l\necho  hi")))))
              "")))
    ;; Results: truncated, errors get the error face.
    (let ((s (ardentia-session--render-record
              (ardentia-test--result-rec "2026-10-10T10:00:02.000Z" "a\nb\nc\nd") "")))
      (should (string-match-p "< a\n +b\n +\\.\\.\\. (2 more lines)" s))
      (should-not (string-match-p "c" (substring s 8))))
    (let ((s (ardentia-session--render-record
              (ardentia-test--result-rec "2026-10-10T10:00:02.000Z"
                                         [((type . "text") (text . "boom"))] t)
              "[qa]")))
      (should (string-match-p "\\[qa\\] ! boom" s))
      (should (eq (get-text-property (string-search "boom" s) 'face s)
                  'ardentia-session-error)))
    ;; Unknown kinds show nothing and never signal.
    (dolist (rec `(((type . "attachment") (timestamp . "2026-10-10T10:00:00Z"))
                   ((type . "queue-operation") (content . "x"))
                   ((type . 5))
                   ((type . "assistant") (message . ((content . 7))))
                   ((type . "user") (timestamp . "garbage")
                    (message . ((content . [5 "x" ((type . "tool_result"))]))))))
      (ardentia-session--render-record rec ""))
    (should-not (ardentia-session--render-record
                 '((type . "attachment")) ""))))

(ert-deftest ardentia-test-session-queued-command ()
  "A message sent into a running turn shows once, as a prompt."
  (let* ((file (make-temp-file "ardentia-queued" nil ".jsonl"))
         (msg "Second message injected: reply with just INJECTED2."))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert
             "{\"type\":\"queue-operation\",\"operation\":\"enqueue\",\"timestamp\":\"2026-10-10T21:22:17.558Z\",\"sessionId\":\"s-1\",\"content\":\"" msg "\"}\n"
             "{\"parentUuid\":\"u-0\",\"isSidechain\":false,\"attachment\":{\"type\":\"queued_command\",\"prompt\":\"" msg "\",\"source_uuid\":\"u-1\",\"delivery_id\":\"d-1\",\"commandMode\":\"prompt\",\"timestamp\":\"2026-10-10T21:22:17.558Z\"},\"type\":\"attachment\",\"uuid\":\"u-2\"}\n"
             "{\"type\":\"queue-operation\",\"operation\":\"remove\",\"timestamp\":\"2026-10-10T21:22:18.000Z\",\"sessionId\":\"s-1\",\"content\":\"" msg "\",\"reason\":\"absorbed_mid_turn\"}\n"))
          (let* ((recs (car (ardentia-session--read-new file 0)))
                 (texts (delq nil (mapcar (lambda (r)
                                            (ardentia-session--render-record r ""))
                                          recs)))
                 (all (apply #'concat texts)))
            (should (= 3 (length recs)))
            (should (= 1 (length texts)))
            (should (string-match-p "\\`[0-9:-]\\{8\\} Prompt: Second message" all))
            (should (eq 'ardentia-session-prompt
                        (get-text-property (string-match "Second" all) 'face all)))
            (should (= 1 (with-temp-buffer
                           (insert all)
                           (how-many "INJECTED2" (point-min) (point-max)))))))
      (delete-file file))))

(ert-deftest ardentia-test-session-read-new-partial-and-malformed ()
  (ardentia-test--with-sessions dir
    (let ((f (expand-file-name "s.jsonl" dir))
          (l1 (ardentia-test--json-line (ardentia-test--prompt-rec "id-1"))))
      (ardentia-test--write f (concat l1 "{not json\n[1,2]\n{\"type\":\"user\""))
      (let ((res (ardentia-session--read-new f 0)))
        (should (= (length (car res)) 1))
        (should (= (cdr res) (+ (string-bytes l1) (length "{not json\n[1,2]\n"))))
        ;; Complete the partial line: it shows up now, exactly once.
        (ardentia-test--write f "}\n" t)
        (let ((res2 (ardentia-session--read-new f (cdr res))))
          (should (= (length (car res2)) 1))
          (should (equal (alist-get 'type (car (car res2))) "user"))
          (should (equal (ardentia-session--read-new f (cdr res2))
                         (cons nil (cdr res2)))))))))

(ert-deftest ardentia-test-session-read-new-multibyte ()
  (ardentia-test--with-sessions dir
    (let ((f (expand-file-name "s.jsonl" dir)))
      (ardentia-test--write
       f (ardentia-test--json-line
          (ardentia-test--assistant-rec "2026-10-10T10:00:01.000Z"
                                        '((type . "text") (text . "Grüße ✓")))))
      (let ((res (ardentia-session--read-new f 0)))
        (should (string-match-p
                 "Grüße ✓"
                 (ardentia-session--render-record (car (car res)) "")))))))

(ert-deftest ardentia-test-session-buffer-incremental ()
  (ardentia-test--with-sessions dir
    (let* ((f (expand-file-name (concat ardentia-test--sid-1 ".jsonl") dir))
           (task (list :id "id-1" :title "Fake task" :file "/o/x.org" :queue 'running
                       :props nil))
           (ardentia-session-follow-interval 1000))
      (ardentia-test--write
       f (concat (ardentia-test--json-line (ardentia-test--prompt-rec "id-1"))
                 (ardentia-test--json-line
                  (ardentia-test--assistant-rec
                   "2026-10-10T10:00:01.000Z" '((type . "text") (text . "first step"))))))
      (let ((buf (ardentia-session-open task)))
        (with-current-buffer buf
          (should (derived-mode-p 'ardentia-session-mode))
          (should buffer-read-only)
          (should (string-match-p "first step" (buffer-string)))
          (should (string-match-p "following" (ardentia-session--header)))
          (should (string-match-p "Fake task" (ardentia-session--header)))
          (should (timerp ardentia-session--timer))
          (should (= (point) (point-max)))
          (ardentia-test--write
           f (ardentia-test--json-line
              (ardentia-test--assistant-rec
               "2026-10-10T10:00:02.000Z" '((type . "text") (text . "second step"))))
           t)
          (ardentia-session--poll)
          (ardentia-session--poll)
          (should (= (length (split-string (buffer-string) "second step")) 2))
          (should (= (length (split-string (buffer-string) "first step")) 2))
          (should (= (point) (point-max)))
          ;; Point not at the end stays where it is.
          (goto-char (point-min))
          (ardentia-test--write
           f (ardentia-test--json-line
              (ardentia-test--assistant-rec
               "2026-10-10T10:00:03.000Z" '((type . "text") (text . "third step"))))
           t)
          (ardentia-session--poll)
          (should (= (point) (point-min)))
          (should (string-match-p "third step" (buffer-string)))
          ;; g re-reads from scratch without duplicating.
          (ardentia-session-reread)
          (should (= (length (split-string (buffer-string) "second step")) 2)))
        (kill-buffer buf)
        (should-not ardentia-session--timer)))))

(ert-deftest ardentia-test-session-finished-detection ()
  (ardentia-test--with-sessions dir
    (let* ((f (expand-file-name (concat ardentia-test--sid-1 ".jsonl") dir))
           (running (list :id "id-1" :title "T" :file "/o/x.org" :queue 'running))
           (ardentia-session-follow-interval 1000))
      (ardentia-test--write f (ardentia-test--json-line (ardentia-test--prompt-rec "id-1")))
      ;; No refresh yet: the queue of the task decides, a fresh file is live.
      (let ((buf (ardentia-session-open running)))
        (with-current-buffer buf
          (should-not ardentia-session--finished)
          ;; The task left the running queue on the last refresh.
          (let ((ardentia--state (list :time (current-time) :tasks nil)))
            (ardentia-session--poll)
            (should ardentia-session--finished)
            (should (string-match-p "finished" (ardentia-session--header)))
            (should-not (ardentia-session--wanted-p buf)))))
      ;; An old file is finished even if listed as running.
      (set-file-times f (time-subtract (current-time) 3600))
      (let ((ardentia--state (list :time (current-time) :tasks (list running))))
        (let ((buf (ardentia-session-open running)))
          (should (buffer-local-value 'ardentia-session--finished buf))
          (should-not (buffer-local-value 'ardentia-session--follow buf))))
      ;; A subagent that still writes keeps an idle main file live.
      (let ((sub (expand-file-name (concat ardentia-test--sid-1 "/subagents/agent-a1.jsonl") dir))
            (ardentia--state (list :time (current-time) :tasks (list running))))
        (make-directory (file-name-directory sub) t)
        (ardentia-test--write sub "")
        (let ((buf (ardentia-session-open running)))
          (should-not (buffer-local-value 'ardentia-session--finished buf))
          (kill-buffer buf))
        (delete-directory (expand-file-name ardentia-test--sid-1 dir) t))
      ;; A review task shows the transcript, finished, without a timer.
      (ardentia-session--cancel-timer)
      (ardentia-test--write f "" t (current-time))
      (let ((buf (ardentia-session-open (list :id "id-1" :title "R" :queue 'review))))
        (should (buffer-local-value 'ardentia-session--finished buf))
        (should-not ardentia-session--timer)))))

(ert-deftest ardentia-test-session-subagents ()
  (ardentia-test--with-sessions dir
    (let* ((f (expand-file-name (concat ardentia-test--sid-1 ".jsonl") dir))
           (sub (expand-file-name (concat ardentia-test--sid-1 "/subagents") dir))
           (a (expand-file-name "agent-abc123.jsonl" sub))
           (task (list :id "id-1" :title "T" :file "/o/x.org" :queue 'running))
           (ardentia-session-follow-interval 1000))
      (ardentia-test--write
       f (concat (ardentia-test--json-line (ardentia-test--prompt-rec "id-1"))
                 (ardentia-test--json-line
                  (ardentia-test--assistant-rec
                   "2026-10-10T10:00:01.000Z" '((type . "text") (text . "main one"))))))
      (let ((buf (ardentia-session-open task)))
        (with-current-buffer buf
          (should-not (string-match-p "\\[qa\\]" (buffer-string)))
          ;; A subagent appears later and sorts by time stamp.
          (ardentia-test--write
           (concat (file-name-sans-extension a) ".meta.json")
           "{\"agentType\":\"qa\",\"description\":\"Re-QA\",\"toolUseId\":\"toolu_1\"}")
          (ardentia-test--write
           a (ardentia-test--json-line
              (ardentia-test--assistant-rec
               "2026-10-10T10:00:03.000Z" '((type . "text") (text . "sub work")))))
          (ardentia-test--write
           f (ardentia-test--json-line
              (ardentia-test--assistant-rec
               "2026-10-10T10:00:04.000Z" '((type . "text") (text . "main two"))))
           t)
          (ardentia-session--poll)
          (let ((text (buffer-string)))
            (should (string-match-p "\\[qa\\] sub work" text))
            (should (< (string-search "main one" text)
                       (string-search "sub work" text)
                       (string-search "main two" text))))
          ;; Subagent without meta file gets a generic prefix.
          (ardentia-test--write
           (expand-file-name "agent-def456.jsonl" sub)
           (ardentia-test--json-line
            (ardentia-test--assistant-rec
             "2026-10-10T10:00:05.000Z" '((type . "text") (text . "other")))))
          (ardentia-session--poll)
          (should (string-match-p "\\[agent\\] other" (buffer-string))))))))

(ert-deftest ardentia-test-session-no-transcript ()
  (ardentia-test--with-sessions dir
    (should (file-directory-p dir))
    (should-error (ardentia-session-open (list :id "id-9" :title "T"))
                  :type 'user-error)))

(ert-deftest ardentia-test-session-agent-shell-optional ()
  (ardentia-test--with-sessions dir
    (ardentia-test--write
     (expand-file-name (concat ardentia-test--sid-1 ".jsonl") dir)
     (ardentia-test--json-line (ardentia-test--prompt-rec "id-1")))
    (let ((task (list :id "id-1" :title "T" :queue 'review))
          (org (make-temp-file "ardentia-org-" t))
          calls)
      (unwind-protect
          (let ((ardentia-org-directory org))
            ;; Not installed: a clear user-error.
            (cl-letf (((symbol-function 'require)
                       (lambda (feature &rest _) (not (eq feature 'agent-shell)))))
              (should-error (ardentia-session-open-agent-shell task)
                            :type 'user-error))
            ;; Installed (faked): session id, directory and verbosity are passed.
            (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
                      ((symbol-function 'agent-shell-resume-session)
                       (lambda (id) (push (list id default-directory
                                                agent-shell-session-restore-verbosity)
                                          calls)))
                      ((symbol-function 'yes-or-no-p)
                       (lambda (&rest _) (error "Must not ask for a finished task"))))
              (ardentia-session-open-agent-shell task)
              (should (equal calls
                             (list (list ardentia-test--sid-1
                                         (file-name-as-directory (file-truename org))
                                         'full))))
              ;; Running: asks, and a "no" cancels.
              (setq calls nil)
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
                (should-error (ardentia-session-open-agent-shell
                               (list :id "id-1" :title "T" :queue 'running))
                              :type 'user-error))
              (should-not calls)
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                (ardentia-session-open-agent-shell
                 (list :id "id-1" :title "T" :queue 'running))
                (should (= (length calls) 1)))))
        (delete-directory org t)))))

(ert-deftest ardentia-test-session-viewer-choice ()
  (let (called)
    (cl-letf (((symbol-function 'ardentia-session-open)
               (lambda (_) (push 'builtin called)))
              ((symbol-function 'ardentia-session-open-agent-shell)
               (lambda (_) (push 'shell called))))
      (let ((ardentia-session-viewer 'ardentia))
        (ardentia-session-open-viewer '(:id "x")))
      (let ((ardentia-session-viewer 'agent-shell))
        (ardentia-session-open-viewer '(:id "x"))))
    (should (equal called '(shell builtin)))))

(ert-deftest ardentia-test-dashboard-enter-dispatch ()
  (let ((buf (get-buffer-create " *ardentia-test-enter*"))
        (tasks (list (ardentia-test--task :queue 'review :id "r")
                     (ardentia-test--task :queue 'running :id "u")
                     (ardentia-test--task :queue 'ready :id "y")))
        (ardentia--state nil)
        calls)
    (unwind-protect
        (with-current-buffer buf
          (ardentia-dashboard-mode)
          (cl-letf (((symbol-function 'ardentia-dashboard--task-at-point)
                     (lambda () (car tasks)))
                    ((symbol-function 'ardentia-review-task)
                     (lambda (tk) (push (list 'review (plist-get tk :id)) calls)))
                    ((symbol-function 'ardentia-session-open-viewer)
                     (lambda (tk) (push (list 'viewer (plist-get tk :id)) calls)))
                    ((symbol-function 'ardentia-session-open)
                     (lambda (tk) (push (list 'session (plist-get tk :id)) calls)))
                    ((symbol-function 'ardentia-session-attach)
                     (lambda (tk) (push (list 'attach (plist-get tk :id)) calls)))
                    ((symbol-function 'ardentia-session-open-agent-shell)
                     (lambda (tk) (push (list 'shell (plist-get tk :id)) calls))))
            (dolist (tk tasks)
              (cl-letf (((symbol-function 'ardentia-dashboard--task-at-point)
                         (lambda () tk)))
                (if (eq (plist-get tk :queue) 'ready)
                    (should-error (ardentia-dashboard-enter) :type 'user-error)
                  (ardentia-dashboard-enter))
                (ardentia-dashboard-transcript)
                (ardentia-dashboard-agent-shell)))
            (should (equal (reverse calls)
                           '((review "r") (session "r") (shell "r")
                             (viewer "u") (session "u") (attach "u")
                             (session "y") (shell "y"))))
            (should (eq (lookup-key ardentia-dashboard-mode-map (kbd "RET"))
                        #'ardentia-dashboard-enter))
            (should (eq (lookup-key ardentia-dashboard-mode-map "t")
                        #'ardentia-dashboard-transcript))
            (should (eq (lookup-key ardentia-dashboard-mode-map "A")
                        #'ardentia-dashboard-agent-shell))
            (should (eq (lookup-key ardentia-review-mode-map "t")
                        #'ardentia-review-transcript))))
      (kill-buffer buf))))

;;;; Live input

(defmacro ardentia-test--with-say (dir script-var exit-check exit-send &rest body)
  "Run BODY with a stub say command in DIR; SCRIPT-VAR names its log file.
The stub exits with EXIT-CHECK for \"--check\" and with EXIT-SEND
otherwise.  It logs the arguments, and the stdin text of a send."
  (declare (indent 4))
  `(let* ((,script-var (expand-file-name "say.log" ,dir))
          (script (expand-file-name "say" ,dir))
          (ardentia-session-say-command script))
     (ardentia-test--write
      script
      (format "#!/bin/sh\necho \"ARGS: $*\" >> '%s'\nif [ \"$1\" = --check ]; then exit %d; fi\ncat >> '%s'\nexit %d\n"
              ,script-var ,exit-check ,script-var ,exit-send))
     (set-file-modes script #o755)
     ,@body))

(defun ardentia-test--slurp (file)
  "Return the contents of FILE, or nil when it does not exist."
  (when (file-exists-p file)
    (with-temp-buffer (insert-file-contents file) (buffer-string))))

(defun ardentia-test--running-session (dir)
  "Write a session for task id-1 into DIR and return the task."
  (ardentia-test--write
   (expand-file-name (concat ardentia-test--sid-1 ".jsonl") dir)
   (ardentia-test--json-line (ardentia-test--prompt-rec "id-1")))
  (list :id "id-1" :title "Live" :queue 'running))

(ert-deftest ardentia-test-session-attach-live ()
  (ardentia-test--with-sessions dir
    (ardentia-test--with-say dir log 0 0
      (let ((task (ardentia-test--running-session dir))
            (ardentia-session-follow-interval 1000)
            shell)
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (&rest _) (error "Must not ask")))
                  ((symbol-function 'ardentia-session-open-agent-shell)
                   (lambda (_) (setq shell t))))
          (save-window-excursion
            (let ((orig (selected-window))
                  (buf (progn (switch-to-buffer (get-buffer-create " *ardentia-test-origin*"))
                              nil)))
              (ignore buf)
              (ardentia-session-attach task)
              (should-not shell)
              (should (eq (selected-window) orig))
              (should (eq (window-buffer orig)
                          (get-buffer "*ardentia-session: Live*")))
              (with-current-buffer (window-buffer orig)
                (should ardentia-session--input)
                (should (string-match-p "input: i" (ardentia-session--header)))
                (should (eq (key-binding "i") #'ardentia-session-compose))))))
        (should (string-match-p "--check session-one" (ardentia-test--slurp log)))
        (kill-buffer " *ardentia-test-origin*")))))

(ert-deftest ardentia-test-session-attach-not-live ()
  (ardentia-test--with-sessions dir
    (ardentia-test--with-say dir log 3 0
      (let ((task (ardentia-test--running-session dir))
            shell msgs)
        (cl-letf (((symbol-function 'ardentia-session-open-agent-shell)
                   (lambda (tk) (setq shell tk)))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) msgs))))
          (ardentia-session-attach task))
        (should (eq shell task))
        (should (string-match-p "does not accept live input" (car msgs)))
        (should-not (get-buffer "*ardentia-session: Live*"))))))

(ert-deftest ardentia-test-session-send ()
  (ardentia-test--with-sessions dir
    (ardentia-test--with-say dir log 0 0
      (let ((text "line one\n  line two \u00e4\u00f6\n"))
        (should (ardentia-session-send "sid-9" text))
        (should (equal (ardentia-test--slurp log)
                       (concat "ARGS: sid-9\n" text)))))
    (ardentia-test--with-say dir log 0 3
      (let ((err (should-error (ardentia-session-send "sid-9" "hi")
                               :type 'user-error)))
        (should (string-match-p "resumes it in agent-shell" (cadr err)))))
    (ardentia-test--with-say dir log 0 4
      (should-error (ardentia-session-send "sid-9" "hi") :type 'user-error))
    (should-error (ardentia-session-send "sid-9" "  \n") :type 'user-error)))

(ert-deftest ardentia-test-session-compose-send-and-abort ()
  (ardentia-test--with-sessions dir
    (ardentia-test--with-say dir log 0 0
      (let ((task (ardentia-test--running-session dir))
            (ardentia-session-follow-interval 1000))
        (save-window-excursion
          (let ((src (ardentia-session-open task t)))
            (with-current-buffer src (setq ardentia-session--follow nil))
            (progn (switch-to-buffer src) (ardentia-session-compose))
            (should (derived-mode-p 'ardentia-session-compose-mode))
            (should (string-match-p "C-c C-c" (format "%s" header-line-format)))
            (insert "multi\nline")
            (ardentia-session-compose-send)
            (should (eq (current-buffer) src))
            (should (buffer-local-value 'ardentia-session--follow src))
            (should (equal (ardentia-test--slurp log)
                           "ARGS: --check session-one\nARGS: session-one\nmulti\nline"))
            ;; Abort sends nothing.
            (progn (switch-to-buffer src) (ardentia-session-compose))
            (insert "never")
            (ardentia-session-compose-abort)
            (should (eq (current-buffer) src))
            (should-not (string-match-p "never" (ardentia-test--slurp log)))))))))

(ert-deftest ardentia-test-session-compose-not-live ()
  (ardentia-test--with-sessions dir
    (ardentia-test--with-say dir log 3 3
      (let ((task (ardentia-test--running-session dir)))
        (save-window-excursion
          (let ((src (ardentia-session-open task t)))
            (progn (switch-to-buffer src) (ardentia-session-compose))
            (insert "hello")
            (should-error (ardentia-session-compose-send) :type 'user-error)
            (kill-buffer (current-buffer))))))))

;;;; Priority

(ert-deftest ardentia-test-priority-from-json ()
  (let ((tasks (ardentia--tasks-from-json
                (ardentia--parse-json
                 "[{\"file\":\"/o/a.org\",\"line\":1,\"title\":\"P\",\"priority\":\"A\",\"tags\":[\"aiready\"],\"id\":\"p1\",\"done\":false,\"properties\":{}},{\"file\":\"/o/a.org\",\"line\":2,\"title\":\"N\",\"priority\":\"null\",\"tags\":[\"aiready\"],\"id\":\"p2\",\"done\":false,\"properties\":{}}]"))))
    (should (equal (plist-get (nth 0 tasks) :priority) "A"))
    (should (null (plist-get (nth 1 tasks) :priority)))
    (should (= (ardentia--task-priority (nth 0 tasks)) ?A))
    (should (= (ardentia--task-priority (nth 1 tasks)) ?B))))

(ert-deftest ardentia-test-priority-sort-ready ()
  (let* ((now (ardentia-test--t "20261010-1200"))
         (mk (lambda (title prio created &optional queue sched)
               (ardentia-test--task :title title :queue (or queue 'ready)
                                    :priority prio :created created
                                    :scheduled sched)))
         (tasks (list (funcall mk "c-old" "C" "20261001-0000")
                      (funcall mk "none-new" nil "20261009-0000")
                      (funcall mk "a-new" "A" "20261009-0000")
                      (funcall mk "none-old" nil "20261002-0000")
                      (funcall mk "b-mid" "B" "20261005-0000")
                      (funcall mk "a-old" "A" "20261003-0000")
                      (funcall mk "later-a" "A" "20261001-0000" 'ready "<2026-10-20 Tue>")
                      (funcall mk "later-c" "C" "20261001-0000" 'ready "<2026-10-12 Mon>")
                      (funcall mk "running-c" "C" "20261001-0000" 'running)))
         (sorted (mapcar (lambda (x) (plist-get x :title))
                         (ardentia--sort-tasks tasks now))))
    ;; Ready: priority first, no cookie = B, then oldest first.  Later
    ;; tasks keep soonest-due-first, priority does not reorder them.
    (should (equal sorted '("running-c" "a-old" "a-new" "none-old" "b-mid"
                            "none-new" "c-old" "later-c" "later-a")))))

(ert-deftest ardentia-test-priority-default-option ()
  (let ((ardentia-default-priority ?D)
        (tasks (list (ardentia-test--task :title "none" :queue 'ready :created "20261001-0000")
                     (ardentia-test--task :title "c" :queue 'ready :priority "C"
                                          :created "20261009-0000"))))
    (should (equal (mapcar (lambda (x) (plist-get x :title))
                           (ardentia--sort-tasks tasks))
                   '("c" "none")))))

(ert-deftest ardentia-test-priority-title-cookie ()
  (let ((now (ardentia-test--t "20261010-1200")))
    (should (equal (aref (ardentia--task-row
                          (ardentia-test--task :queue 'ready :priority "A" :title "Do it")
                          now)
                         8)
                   "[#A] Do it"))
    (should (equal (aref (ardentia--task-row
                          (ardentia-test--task :queue 'ready :title "Plain")
                          now)
                         8)
                   "Plain"))))

(defconst ardentia-test--prio-org
  "* TODO [#C] Ready one :aiready:\n:PROPERTIES:\n:ID:       prio-1\n:END:\nbody\n* TODO Review one :humanreview:\n:PROPERTIES:\n:ID:       prio-2\n:END:\n"
  "Org text for the priority tests.")

(ert-deftest ardentia-test-priority-set ()
  (ardentia-test--with-org-file f ardentia-test--prio-org
    (ardentia-set-priority-1 f "prio-1" ?A)
    (should (string-match-p "^\\* TODO \\[#A\\] Ready one :aiready:$"
                            (ardentia-test--file-text f)))
    (should-not (buffer-modified-p (find-buffer-visiting f)))
    (ardentia-set-priority-1 f "prio-1" 'down)
    (should (string-match-p "^\\* TODO \\[#B\\] Ready one :aiready:$"
                            (ardentia-test--file-text f)))
    (ardentia-set-priority-1 f "prio-1" 'remove)
    (should (string-match-p "^\\* TODO Ready one :aiready:$"
                            (ardentia-test--file-text f)))
    ;; Only :aiready: headings; the review task stays untouched.
    (should-error (ardentia-set-priority-1 f "prio-2" ?A) :type 'user-error)
    (should (string-match-p "^\\* TODO Review one :humanreview:$"
                            (ardentia-test--file-text f)))))

(ert-deftest ardentia-test-priority-dashboard-keys ()
  (with-temp-buffer
    (ardentia-dashboard-mode)
    (should (eq (keymap-lookup ardentia-dashboard-mode-map ",")
                #'ardentia-dashboard-priority))
    (should (eq (keymap-lookup ardentia-dashboard-mode-map "+")
                #'ardentia-dashboard-priority-up))
    (should (eq (keymap-lookup ardentia-dashboard-mode-map "-")
                #'ardentia-dashboard-priority-down))
    (should (eq (keymap-lookup ardentia-dashboard-mode-map "S-<up>")
                #'ardentia-dashboard-priority-up))
    (let ((called nil))
      (cl-letf (((symbol-function 'ardentia-set-priority-1)
                 (lambda (&rest args) (setq called args)))
                ((symbol-function 'read-char-exclusive) (lambda (&rest _) ?a)))
        ;; Review and running tasks are refused before any prompt or write.
        (dolist (queue '(review running))
          (cl-letf (((symbol-function 'ardentia-dashboard--task-at-point)
                     (lambda (&rest _) (list :queue queue :file "/o/x.org" :id "i"))))
            (should (equal (cadr (should-error (ardentia-dashboard-priority)
                                               :type 'user-error))
                           "Priority can only be changed on ready or later tasks"))
            (should-error (ardentia-dashboard-priority-up) :type 'user-error)))
        (should-not called)
        (cl-letf (((symbol-function 'ardentia-dashboard--task-at-point)
                   (lambda (&rest _) (list :queue 'ready :file "/o/x.org" :id "i"))))
          (ardentia-dashboard-priority)
          (should (equal called '("/o/x.org" "i" ?A nil nil)))
          (ardentia-dashboard-priority-down)
          (should (equal called '("/o/x.org" "i" down nil nil))))
        (cl-letf (((symbol-function 'ardentia-dashboard--task-at-point)
                   (lambda (&rest _) (list :queue 'ready :file "/o/x.org" :id "i")))
                  ((symbol-function 'read-char-exclusive) (lambda (&rest _) ?\s)))
          (ardentia-dashboard-priority)
          (should (equal called '("/o/x.org" "i" remove nil nil))))
        (cl-letf (((symbol-function 'ardentia-dashboard--task-at-point)
                   (lambda (&rest _) (list :queue 'ready :file "/o/x.org" :id "i")))
                  ((symbol-function 'read-char-exclusive) (lambda (&rest _) ?z)))
          (should-error (ardentia-dashboard-priority) :type 'user-error))))))

;;;; Tasks without an ID (just captured, the runner adds IDs later)

(defconst ardentia-test--noid-org
  "* TODO [#C] First :aiready:\n* TODO Second :aiready:\nbody\n"
  "Org text of two ready headings without ID.")

(ert-deftest ardentia-test-priority-no-id-by-line ()
  (ardentia-test--with-org-file f ardentia-test--noid-org
    (ardentia-set-priority-1 f nil ?A 2 "Second")
    (should (equal (ardentia-test--file-text f)
                   "* TODO [#C] First :aiready:\n* TODO [#A] Second :aiready:\nbody\n"))
    (should-not (buffer-modified-p (find-buffer-visiting f)))
    ;; The "null" string of the JSON counts as no ID, too.
    (ardentia-set-priority-1 f "null" 'remove 2 "Second")
    (should (string-match-p "^\\* TODO Second :aiready:$"
                            (ardentia-test--file-text f)))))

(ert-deftest ardentia-test-priority-no-id-title-fallback ()
  (ardentia-test--with-org-file f ardentia-test--noid-org
    ;; Line 1 holds another heading: fall back to the unique title.
    (ardentia-set-priority-1 f nil ?B 1 "Second")
    (should (equal (ardentia-test--file-text f)
                   "* TODO [#C] First :aiready:\n* TODO [#B] Second :aiready:\nbody\n"))
    ;; A stale line past the end works the same.
    (ardentia-set-priority-1 f nil ?A 99 "First")
    (should (string-match-p "^\\* TODO \\[#A\\] First :aiready:$"
                            (ardentia-test--file-text f)))))

(ert-deftest ardentia-test-priority-no-id-ambiguous-or-missing ()
  (dolist (case '(("* TODO Same :aiready:\n* TODO Same :aiready:\n" 2 "Other")
                  ("* TODO Same :aiready:\n* TODO Same :aiready:\n" 3 "Same")
                  ("* TODO A :aiready:\n" 1 "Gone")))
    (ardentia-test--with-org-file f (car case)
      (should-error (ardentia-set-priority-1 f nil ?A (nth 1 case) (nth 2 case))
                    :type 'user-error)
      (should (equal (ardentia-test--file-text f) (car case)))
      (should-not (buffer-modified-p (find-buffer-visiting f))))))

(ert-deftest ardentia-test-priority-no-id-line-picks-among-duplicates ()
  ;; Two headings share the title; the line says which one is meant.
  (ardentia-test--with-org-file f "* TODO Same :aiready:\n* TODO Same :aiready:\n"
    (ardentia-set-priority-1 f nil ?A 2 "Same")
    (should (equal (ardentia-test--file-text f)
                   "* TODO Same :aiready:\n* TODO [#A] Same :aiready:\n"))))

(ert-deftest ardentia-test-priority-no-id-needs-aiready-tag ()
  (ardentia-test--with-org-file f "* TODO Review :humanreview:\n"
    (should-error (ardentia-set-priority-1 f nil ?A 1 "Review")
                  :type 'user-error)
    (should (equal (ardentia-test--file-text f) "* TODO Review :humanreview:\n"))))

(ert-deftest ardentia-test-priority-dashboard-passes-line-and-title ()
  (let (called)
    (cl-letf (((symbol-function 'ardentia-set-priority-1)
               (lambda (&rest args) (setq called args)))
              ((symbol-function 'ardentia-dashboard--task-at-point)
               (lambda (&rest _)
                 (list :queue 'ready :file "/o/x.org" :id nil :line 7
                       :title "Fresh"))))
      (ardentia-dashboard--set-priority 'up)
      (should (equal called '("/o/x.org" nil up 7 "Fresh"))))))

(ert-deftest ardentia-test-run-no-id-creates-id-and-saves ()
  (ardentia-test--with-org-file f "* TODO Other\n* TODO Go :aiready:\nbody\n"
    (let ((ardentia-run-task-command '("true")) (ardentia--local-host nil)
          got)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'ardentia-run-task-1)
                 (lambda (file id title) (setq got (list file id title)))))
        ;; Stale line: the title fallback finds the heading.
        (ardentia-run-task (list :queue 'ready :title "Go" :file f :id nil
                                 :line 1 :tags '("aiready"))))
      (let ((text (ardentia-test--file-text f)))
        (should (string-match "^:ID: +\\([-0-9a-f]+\\)$" text))
        (should (equal (nth 0 got) f))
        (should (equal (nth 1 got) (match-string 1 text)))
        (should (equal (nth 2 got) "Go"))
        (should (string-match-p "\\* TODO Go :aiready:\n:PROPERTIES:" text)))
      (should-not (buffer-modified-p (find-buffer-visiting f))))))

(ert-deftest ardentia-test-run-no-id-ambiguous-leaves-file ()
  (let ((text "* TODO Go :aiready:\n* TODO Go :aiready:\n"))
    (ardentia-test--with-org-file f text
      (let ((ardentia-run-task-command '("true")) (ardentia--local-host nil))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                  ((symbol-function 'ardentia-run-task-1)
                   (lambda (&rest _) (ert-fail "must not run"))))
          (should-error (ardentia-run-task
                         (list :queue 'ready :title "Go" :file f :id nil
                               :line 5 :tags '("aiready")))
                        :type 'user-error)))
      (should (equal (ardentia-test--file-text f) text)))))

;;;; Dashboard auto-refresh

(defmacro ardentia-test--with-timer-sandbox (&rest body)
  "Run BODY with no auto-refresh timer, and clean up whatever it creates."
  `(let ((ardentia-dashboard--refresh-timer nil)
         (ardentia-dashboard-refresh-interval 60))
     (unwind-protect (progn ,@body)
       (when (timerp ardentia-dashboard--refresh-timer)
         (cancel-timer ardentia-dashboard--refresh-timer)))))

(defmacro ardentia-test--with-dashboard (&rest body)
  "Run BODY with a live dashboard buffer; kill it afterwards."
  `(let ((buf (get-buffer-create " *ardentia-test-auto*")))
     (unwind-protect
         (progn (with-current-buffer buf (ardentia-dashboard-mode)) ,@body)
       (kill-buffer buf))))

(defun ardentia-test--tick (visible &optional state)
  "Run the tick with VISIBLE window result and STATE; return refresh count."
  (let ((n 0) (ardentia--state state))
    (cl-letf (((symbol-function 'get-buffer-window)
               (lambda (&rest _) visible))
              ((symbol-function 'ardentia-dashboard-refresh-all)
               (lambda () (cl-incf n))))
      (ardentia-dashboard--refresh-tick))
    n))

(ert-deftest ardentia-test-auto-refresh-visible ()
  (ardentia-test--with-timer-sandbox
   (ardentia-test--with-dashboard
    (should (= 1 (ardentia-test--tick t)))
    (should (= 0 (ardentia-test--tick nil))))))

(ert-deftest ardentia-test-auto-refresh-skips-running ()
  (ardentia-test--with-timer-sandbox
   (ardentia-test--with-dashboard
    (should (= 0 (ardentia-test--tick t '(:refreshing t)))))))

(ert-deftest ardentia-test-auto-refresh-skips-minibuffer ()
  (ardentia-test--with-timer-sandbox
   (ardentia-test--with-dashboard
    (cl-letf (((symbol-function 'active-minibuffer-window)
               (lambda () (selected-window))))
      (should (= 0 (ardentia-test--tick t)))))))

(ert-deftest ardentia-test-auto-refresh-cancels-without-dashboard ()
  (ardentia-test--with-timer-sandbox
   (ardentia-test--with-dashboard
    (should (timerp ardentia-dashboard--refresh-timer)))
   (should (timerp ardentia-dashboard--refresh-timer))
   (should (= 0 (ardentia-test--tick t)))
   (should (null ardentia-dashboard--refresh-timer))))

(ert-deftest ardentia-test-auto-refresh-single-timer ()
  (ardentia-test--with-timer-sandbox
   (ardentia-dashboard--refresh-timer-start)
   (let ((first ardentia-dashboard--refresh-timer))
     (should (timerp first))
     (ardentia-dashboard--refresh-timer-start)
     (with-temp-buffer (ardentia-dashboard-mode))
     (should (eq first ardentia-dashboard--refresh-timer)))))

(ert-deftest ardentia-test-auto-refresh-reopen-starts-timer ()
  "Opening an existing dashboard buffer starts a missing timer."
  (ardentia-test--with-timer-sandbox
   (let ((buf (get-buffer-create " *ardentia-test-reopen*")))
     (unwind-protect
         (cl-letf (((symbol-function 'pop-to-buffer-same-window) #'ignore)
                   ((symbol-function 'ardentia-dashboard-refresh-all) #'ignore))
           (with-current-buffer buf (ardentia-dashboard-mode))
           (cancel-timer ardentia-dashboard--refresh-timer)
           (setq ardentia-dashboard--refresh-timer nil)
           (ardentia-dashboard--open 'dashboard " *ardentia-test-reopen*")
           (should (timerp ardentia-dashboard--refresh-timer)))
       (kill-buffer buf)))))

(ert-deftest ardentia-test-auto-refresh-disabled ()
  (dolist (val '(nil 0))
    (ardentia-test--with-timer-sandbox
     (let ((ardentia-dashboard-refresh-interval val))
       (ardentia-dashboard--refresh-timer-start)
       (should (null ardentia-dashboard--refresh-timer))))))

(ert-deftest ardentia-test-auto-refresh-error-contained ()
  (ardentia-test--with-timer-sandbox
   (ardentia-test--with-dashboard
    (let ((ardentia--state nil) (msg nil))
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) t))
                ((symbol-function 'ardentia-refresh)
                 (lambda (&rest _) (error "boom")))
                ((symbol-function 'message)
                 (lambda (f &rest a) (setq msg (apply #'format f a)))))
        (ardentia-dashboard--refresh-tick))
      (should (string-match-p "boom" msg))))))

(ert-deftest ardentia-test-auto-refresh-keeps-point ()
  (ardentia-test--with-timer-sandbox
   (let* ((ardentia--state
           (list :tasks (ardentia--tasks-from-json
                         (ardentia--parse-json ardentia-test--json))
                 :host "desktop" :time (current-time)))
          (buf (get-buffer-create " *ardentia-test-point*")))
     (unwind-protect
         (save-window-excursion
           (set-window-buffer (selected-window) buf)
           (with-current-buffer buf
             (ardentia-dashboard-mode)
             (ardentia-dashboard--render))
           (set-window-point (selected-window) (with-current-buffer buf
                                                 (goto-char (point-min))
                                                 (forward-line 2)
                                                 (point)))
           (let ((title (with-current-buffer buf
                          (plist-get (ardentia-dashboard--task-at-point) :title)))
                 (start (window-start)))
             (with-current-buffer buf (ardentia-dashboard--render))
             (should (equal title
                            (with-current-buffer buf
                              (goto-char (window-point))
                              (plist-get (ardentia-dashboard--task-at-point)
                                         :title))))
             (should (= start (window-start)))))
       (kill-buffer buf)))))

;;;; Task list views and filters

(defun ardentia-test--stamp-ago (days)
  "Return an AI_SESSION stamp DAYS days before now."
  (format-time-string "%Y%m%d-%H%M" (time-subtract (current-time)
                                                    (* days 86400))))

(defun ardentia-test--lt (title queue host &rest plist)
  "Return a task plist TITLE in QUEUE claimed by HOST (or nil).
PLIST adds or overrides keys; :days is the age of AI_SESSION in days."
  (let* ((days (or (plist-get plist :days) 1))
         (plist (let ((c (copy-sequence plist))) (cl-remf c :days) c)))
    (append plist
            (list :title title :queue queue :file "/o/l.org"
                  :id (format "id-%s" (downcase title)) :line 1
                  :tags (list (if (eq queue 'ready) "aiready" "humanreview"))
                  :props (append (and host `(("AI_CLAIMED_BY" . ,host)))
                                 `(("AI_SESSION" . ,(ardentia-test--stamp-ago days))
                                   ("AI_STATUS" . "completed")))))))

(defun ardentia-test--open-tasks ()
  "Return the open tasks of the fixture, as the shared state holds them."
  (list (ardentia-test--lt "Alpha" 'review "alpha" :days 1)
        (ardentia-test--lt "Beta" 'review "beta" :days 10)
        (ardentia-test--lt "Gamma" 'ready nil :id nil :line 7)
        (ardentia-test--lt "Delta" 'ready nil :scheduled "<2999-01-01 Mon 10:00>")
        (ardentia-test--lt "Eps" 'running "alpha")))

(defun ardentia-test--done-tasks ()
  "Return the accepted tasks of the fixture."
  (list (ardentia-test--lt "Zeta" 'review "alpha" :done t :todo "DONE" :days 2)
        (ardentia-test--lt "Eta" 'review "beta" :done t :todo "DONE" :days 40)))

(defmacro ardentia-test--with-list (&rest body)
  "Run BODY in a task list buffer filled with the fixture tasks.
The completed cache holds the open review tasks and the accepted ones;
fetching and the hosts file are replaced by fixtures, the timer is boxed."
  (declare (indent 0))
  `(ardentia-test--with-timer-sandbox
    (let* ((ardentia--state (list :tasks (ardentia-test--open-tasks)
                                  :host "alpha" :time (current-time)))
           (ardentia-completed-period 'all)
           (ardentia-dashboard-show-later t)
           (buf (get-buffer-create " *ardentia-test-list*")))
      (cl-letf (((symbol-function 'ardentia-hosts)
                 (lambda () '(:hosts ("alpha" "beta" "gamma")
                              :default-host "alpha")))
                ((symbol-function 'ardentia-dashboard--completed-refresh)
                 #'ignore))
        (unwind-protect
            (with-current-buffer buf
              (ardentia-dashboard-mode)
              (setq ardentia-dashboard--completed-tasks
                    (append (list (nth 0 (ardentia-test--open-tasks))
                                  (nth 1 (ardentia-test--open-tasks)))
                            (ardentia-test--done-tasks)))
              (ardentia-dashboard--render)
              ,@body)
          (kill-buffer buf))))))

(defun ardentia-test--shown ()
  "Return the titles of the rows of the current task list, top to bottom."
  (mapcar (lambda (e) (plist-get (car e) :title)) tabulated-list-entries))

(defun ardentia-test--header ()
  "Return the header line text of the current task list."
  (substring-no-properties (ardentia-dashboard--info-line)))

(defun ardentia-test--at-title ()
  "Return the title of the row at point."
  (plist-get (ardentia-dashboard--task-at-point) :title))

(defun ardentia-test--goto (title)
  "Move point to the row titled TITLE."
  (goto-char (point-min))
  (while (not (equal (ardentia-test--at-title) title))
    (when (eobp) (ert-fail (format "no row %s" title)))
    (forward-line 1)))

(ert-deftest ardentia-test-list-view-entries-and-columns ()
  (ardentia-test--with-list
    ;; dashboard: every open task, the later one last
    (should (eq (ardentia-dashboard--view-now) 'dashboard))
    (should (equal (sort (copy-sequence (ardentia-test--shown)) #'string<)
                   '("Alpha" "Beta" "Delta" "Eps" "Gamma")))
    (should (equal (car (last (ardentia-test--shown))) "Delta"))
    (should (equal (car (aref tabulated-list-format 0)) "Queue"))
    (should (string-match-p "Queue +Status +Host" (buffer-string)))
    ;; review: only the review queue, same columns
    (ardentia-dashboard--switch-view 'review)
    (should (equal (sort (copy-sequence (ardentia-test--shown)) #'string<)
                   '("Alpha" "Beta")))
    (should (equal (car (aref tabulated-list-format 0)) "Queue"))
    ;; completed: open and accepted, newest finish first, own columns
    (ardentia-dashboard--switch-view 'completed)
    (should (equal (ardentia-test--shown) '("Alpha" "Zeta" "Beta" "Eta")))
    (should (equal (mapcar #'car (append tabulated-list-format nil))
                   '("Finished" "State" "Status" "Host" "Runs" "Tokens" "Cost"
                     "File" "Title")))
    (should (string-match-p "Finished +State +Status" (buffer-string)))
    (should (string-match-p "DONE +completed +alpha" (buffer-string)))
    ;; back: the dashboard columns again
    (ardentia-dashboard--switch-view 'dashboard)
    (should (equal (car (aref tabulated-list-format 0)) "Queue"))
    (should (string-match-p "Queue +Status +Host" (buffer-string)))))

(ert-deftest ardentia-test-list-completed-sorting ()
  (ardentia-test--with-list
    (ardentia-dashboard--switch-view 'completed)
    ;; header click sorting stays available, numerically for Tokens
    (should (functionp (nth 2 (aref tabulated-list-format 5))))
    (should (eq (nth 2 (aref tabulated-list-format 0)) t))
    (setq tabulated-list-sort-key '("Finished" . nil))
    ;; leaving a view starts the sort over
    (ardentia-dashboard--switch-view 'review)
    (should (null tabulated-list-sort-key))
    (setq tabulated-list-sort-key '("Queue"))
    (ardentia-dashboard--switch-view 'completed)
    (should (null tabulated-list-sort-key))
    (should (equal (ardentia-test--shown) '("Alpha" "Zeta" "Beta" "Eta")))))

(ert-deftest ardentia-test-list-switch-keys-and-cycle ()
  (ardentia-test--with-list
    (dolist (pair '(("1" . ardentia-dashboard) ("2" . ardentia-review-inbox)
                    ("3" . ardentia-completed) ("c" . ardentia-completed)
                    ("r" . ardentia-review-inbox)
                    ("TAB" . ardentia-dashboard-next-view)
                    ("<backtab>" . ardentia-dashboard-previous-view)
                    ("f" . ardentia-dashboard-filter-menu)
                    ("h" . ardentia-dashboard-cycle-host)
                    ("/" . ardentia-dashboard-filter-title)
                    ("l" . ardentia-dashboard-toggle-later)
                    ("P" . ardentia-dashboard-cycle-period)
                    ("w" . ardentia-dashboard-cycle-state)))
      (should (eq (keymap-lookup ardentia-dashboard-mode-map (car pair))
                  (cdr pair))))
    ;; p and n stay line navigation, S still sorts
    (dolist (key '("p" "n"))
      (should-not (string-match-p "ardentia-dashboard-\\(cycle\\|filter\\|toggle\\)"
                                  (format "%s" (keymap-lookup
                                                ardentia-dashboard-mode-map key)))))
    (should (eq (keymap-lookup ardentia-dashboard-mode-map "S")
                'tabulated-list-sort))
    (ardentia-dashboard-next-view)
    (should (eq (ardentia-dashboard--view-now) 'review))
    (ardentia-dashboard-next-view)
    (should (eq (ardentia-dashboard--view-now) 'completed))
    (ardentia-dashboard-next-view)
    (should (eq (ardentia-dashboard--view-now) 'dashboard))
    (ardentia-dashboard-previous-view)
    (should (eq (ardentia-dashboard--view-now) 'completed))
    (ardentia-dashboard-previous-view)
    (should (eq (ardentia-dashboard--view-now) 'review))))

(ert-deftest ardentia-test-list-point-memory-per-view ()
  (ardentia-test--with-list
    (ardentia-test--goto "Eps")
    (ardentia-dashboard--switch-view 'review)
    ;; never seen: the top of the list
    (should (equal (ardentia-test--at-title) (car (ardentia-test--shown))))
    (ardentia-test--goto "Beta")
    (ardentia-dashboard--switch-view 'completed)
    (should (equal (ardentia-test--at-title) "Alpha"))
    (ardentia-test--goto "Eta")
    (ardentia-dashboard--switch-view 'dashboard)
    (should (equal (ardentia-test--at-title) "Eps"))
    (ardentia-dashboard--switch-view 'completed)
    (should (equal (ardentia-test--at-title) "Eta"))
    (ardentia-dashboard--switch-view 'review)
    (should (equal (ardentia-test--at-title) "Beta"))
    ;; a remembered row that is gone: the top again
    (ardentia-dashboard--switch-view 'dashboard)
    (setq ardentia--state (list :tasks (list (ardentia-test--lt "New" 'ready nil))
                                :host "alpha"))
    (ardentia-dashboard--render)
    (ardentia-dashboard--switch-view 'review)
    (ardentia-dashboard--switch-view 'dashboard)
    (should (equal (ardentia-test--at-title) "New"))))

(ert-deftest ardentia-test-list-host-filter ()
  (ardentia-test--with-list
    ;; all -> local -> the other hosts -> all
    (should (null (ardentia-dashboard--next-host nil nil nil)))
    (should (equal (ardentia-dashboard--next-host nil "alpha" '("alpha" "beta"))
                   "alpha"))
    (should (equal (ardentia-dashboard--next-host "alpha" "alpha" '("alpha" "beta"))
                   "beta"))
    (should (null (ardentia-dashboard--next-host "beta" "alpha" '("alpha" "beta"))))
    (should (null (ardentia-dashboard--next-host "gone" "alpha" '("alpha"))))
    (ardentia-dashboard-cycle-host)
    (should (equal ardentia-dashboard--filter-host "alpha"))
    ;; Gamma and Delta are unclaimed: they count for the default host
    (should (equal (sort (copy-sequence (ardentia-test--shown)) #'string<)
                   '("Alpha" "Delta" "Eps" "Gamma")))
    (ardentia-dashboard--switch-view 'review)
    (should (equal (ardentia-test--shown) '("Alpha")))
    (ardentia-dashboard--switch-view 'completed)
    (should (equal (ardentia-test--shown) '("Alpha" "Zeta")))
    (ardentia-dashboard-cycle-host)
    (should (equal ardentia-dashboard--filter-host "beta"))
    (should (equal (ardentia-test--shown) '("Beta" "Eta")))
    (ardentia-dashboard-cycle-host)
    (should (equal ardentia-dashboard--filter-host "gamma"))
    (should (null (ardentia-test--shown)))
    (ardentia-dashboard-cycle-host)
    (should (null ardentia-dashboard--filter-host))
    (should (= (length (ardentia-test--shown)) 4))))

(ert-deftest ardentia-test-list-title-filter ()
  (ardentia-test--with-list
    (ardentia-dashboard-filter-title "^[ab]")
    (should (equal (sort (copy-sequence (ardentia-test--shown)) #'string<)
                   '("Alpha" "Beta")))
    (ardentia-dashboard--switch-view 'completed)
    (should (equal (ardentia-test--shown) '("Alpha" "Beta")))
    (ardentia-dashboard-filter-title "ZETA")
    (should (equal (ardentia-test--shown) '("Zeta")))
    ;; an invalid regexp is refused and keeps the filter
    (should-error (ardentia-dashboard-filter-title "[") :type 'user-error)
    (should (equal ardentia-dashboard--filter-title "ZETA"))
    ;; empty input clears
    (ardentia-dashboard-filter-title "")
    (should (null ardentia-dashboard--filter-title))
    (should (= (length (ardentia-test--shown)) 4))))

(ert-deftest ardentia-test-list-merges-follow-filters ()
  (ardentia-test--with-list
    (setq ardentia--state
          (list :tasks (ardentia-test--open-tasks) :host "alpha"
                :merges (list (list :merge t :dir "/tmp/m1" :file "/tmp/m1"
                                    :title "Merge one" :queue 'running))))
    (ardentia-dashboard--render)
    (should (member "Merge one" (ardentia-test--shown)))
    (ardentia-dashboard-filter-title "alpha")
    (should-not (member "Merge one" (ardentia-test--shown)))
    (ardentia-dashboard-filter-title "")
    (setq ardentia-dashboard--filter-host "beta")
    (ardentia-dashboard--render)
    (should-not (member "Merge one" (ardentia-test--shown)))
    (setq ardentia-dashboard--filter-host "alpha")
    (ardentia-dashboard--render)
    (should (member "Merge one" (ardentia-test--shown)))
    ;; merges belong to the dashboard view only
    (ardentia-dashboard--switch-view 'review)
    (should-not (member "Merge one" (ardentia-test--shown)))))

(ert-deftest ardentia-test-list-later-filter ()
  (ardentia-test--with-list
    (should (member "Delta" (ardentia-test--shown)))
    (ardentia-dashboard-toggle-later)
    (should-not (member "Delta" (ardentia-test--shown)))
    (should (member "Gamma" (ardentia-test--shown)))
    ;; only in the dashboard view, and the error names it
    (dolist (view '(review completed))
      (ardentia-dashboard--switch-view view)
      (let ((err (should-error (ardentia-dashboard-toggle-later)
                               :type 'user-error)))
        (should (string-match-p "Dashboard" (cadr err)))))
    ;; the filter persists over the switches
    (ardentia-dashboard--switch-view 'dashboard)
    (should-not (member "Delta" (ardentia-test--shown)))
    (ardentia-dashboard-toggle-later)
    (should (member "Delta" (ardentia-test--shown)))))

(ert-deftest ardentia-test-list-period-and-state-filters ()
  (ardentia-test--with-list
    (dolist (cmd '(ardentia-dashboard-cycle-period ardentia-dashboard-cycle-state))
      (dolist (view '(dashboard review))
        (ardentia-dashboard--switch-view view)
        (let ((err (should-error (funcall cmd) :type 'user-error)))
          (should (string-match-p "Completed" (cadr err))))))
    (ardentia-dashboard--switch-view 'completed)
    ;; period: all -> 30 days (drops Eta, 40 days) -> 7 days (drops Beta, 10)
    (ardentia-dashboard-cycle-period)
    (should (equal (ardentia-test--shown) '("Alpha" "Zeta" "Beta")))
    (ardentia-dashboard-cycle-period)
    (should (equal (ardentia-test--shown) '("Alpha" "Zeta")))
    (ardentia-dashboard-cycle-period)                 ; today
    (should (null (ardentia-test--shown)))
    (ardentia-dashboard-cycle-period)
    (should (equal (ardentia-test--shown) '("Alpha" "Zeta" "Beta" "Eta")))
    ;; state: waiting for review -> accepted -> all
    (ardentia-dashboard-cycle-state)
    (should (equal (ardentia-test--shown) '("Alpha" "Beta")))
    (ardentia-dashboard-cycle-state)
    (should (equal (ardentia-test--shown) '("Zeta" "Eta")))
    (ardentia-dashboard-cycle-state)
    (should (= (length (ardentia-test--shown)) 4))
    ;; they persist over a switch away and back
    (ardentia-dashboard-cycle-state)
    (ardentia-dashboard--switch-view 'dashboard)
    (should (= (length (ardentia-test--shown)) 5))
    (ardentia-dashboard--switch-view 'completed)
    (should (equal (ardentia-test--shown) '("Alpha" "Beta")))))

(ert-deftest ardentia-test-list-period-default-from-option ()
  (ardentia-test--with-list
    (let ((ardentia-completed-period 'last-7-days))
      (ardentia-dashboard--switch-view 'completed)
      (should (equal (ardentia-test--shown) '("Alpha" "Zeta")))
      (should (equal (ardentia-dashboard--filter-parts) nil))
      (ardentia-dashboard-cycle-period)               ; today
      (should (equal (ardentia-dashboard--filter-parts) '("period today"))))))

(ert-deftest ardentia-test-list-filters-combined-and-cleared ()
  (ardentia-test--with-list
    (setq ardentia-dashboard--filter-host "alpha")
    (ardentia-dashboard-filter-title "a$")
    (ardentia-dashboard-toggle-later)
    (should (equal (sort (copy-sequence (ardentia-test--shown)) #'string<)
                   '("Alpha" "Gamma")))
    (ardentia-dashboard--switch-view 'completed)
    (ardentia-dashboard-cycle-state)                  ; waiting for review
    (should (equal (ardentia-test--shown) '("Alpha")))
    (ardentia-dashboard-cycle-state)                  ; accepted
    (should (equal (ardentia-test--shown) '("Zeta")))
    (ardentia-dashboard-clear-filters)
    (should (null ardentia-dashboard--filter-host))
    (should (null ardentia-dashboard--filter-title))
    (should (eq ardentia-dashboard--state-filter 'all))
    (should (eq ardentia-dashboard--show-later 'default))
    (should (= (length (ardentia-test--shown)) 4))
    (should (null (ardentia-dashboard--filter-parts)))
    (ardentia-dashboard--switch-view 'dashboard)
    (should (= (length (ardentia-test--shown)) 5))))

(ert-deftest ardentia-test-list-header-shows-view-and-filters ()
  (ardentia-test--with-list
    (should (string-prefix-p " [Dashboard] Review Completed" (ardentia-test--header)))
    (should-not (string-match-p "filter:" (ardentia-test--header)))
    (ardentia-dashboard--switch-view 'review)
    (should (string-prefix-p " Dashboard [Review] Completed" (ardentia-test--header)))
    (ardentia-dashboard--switch-view 'completed)
    (should (string-prefix-p " Dashboard Review [Completed] | 4 tasks"
                             (ardentia-test--header)))
    (should (string-match-p "Refreshed: never" (ardentia-test--header)))
    (ardentia-dashboard--switch-view 'dashboard)
    (setq ardentia-dashboard--filter-host "beta")
    (ardentia-dashboard-filter-title "rev")
    (ardentia-dashboard-toggle-later)
    (should (string-prefix-p
             " [Dashboard] Review Completed | filter: host beta, /rev/, later hidden"
             (ardentia-test--header)))
    ;; a filter that does not apply to a view is not claimed there
    (ardentia-dashboard--switch-view 'review)
    (should (string-match-p "| filter: host beta, /rev/ |" (ardentia-test--header)))
    (ardentia-dashboard--switch-view 'completed)
    (ardentia-dashboard-cycle-state)
    (ardentia-dashboard-cycle-period)
    (should (string-match-p
             "| filter: host beta, /rev/, period last 30 days, state waiting for review |"
             (ardentia-test--header)))
    ;; the view indicator is part of the narrow layout: the first segment
    (pcase-let ((`(,count ,compact ,lines) (ardentia-dashboard--layout 40)))
      (should compact)
      (should (>= count 1))
      (should (equal (substring-no-properties
                      (car (ardentia-dashboard--info-segments nil t)))
                     " Dashboard Review [Completed]"))
      (should lines))))

(ert-deftest ardentia-test-list-header-completed-error-and-refreshing ()
  (ardentia-test--with-list
    (ardentia-dashboard--switch-view 'completed)
    (setq ardentia-dashboard--completed-error "boom"
          ardentia-dashboard--completed-refreshing t)
    (should (string-match-p "ERROR: boom | refreshing\\.\\.\\.\\'"
                            (ardentia-test--header)))))

(ert-deftest ardentia-test-list-completed-fetch-and-failure ()
  (let* ((dir (make-temp-file "ardentia-fake-" t))
         (ardentia-org-directory dir)
         (ardentia-org-tool (expand-file-name "ai/tools/org-tool" dir))
         (ardentia--state (list :tasks nil :host "alpha"))
         (buf (get-buffer-create " *ardentia-test-fetch*")))
    (ardentia-test--with-timer-sandbox
     (unwind-protect
         (with-current-buffer buf
           (ardentia-test--fake-env dir)
           (ardentia-dashboard-mode)
           (ardentia-dashboard--render)
           ;; no fetch in the other views
           (ardentia-dashboard--switch-view 'review)
           (should-not ardentia-dashboard--completed-refreshing)
           ;; switching in fetches, asynchronously
           (ardentia-dashboard--switch-view 'completed)
           (should ardentia-dashboard--completed-refreshing)
           (should (string-match-p "refreshing" (ardentia-test--header)))
           (ardentia-test--wait (lambda () (not ardentia-dashboard--completed-refreshing)))
           (should (null ardentia-dashboard--completed-error))
           (should (equal (mapcar (lambda (x) (plist-get x :title))
                                  ardentia-dashboard--completed-tasks)
                          '("Review me" "Finished" "Plain")))
           (should (= (length (ardentia-test--shown)) 3))
           (should (member "Finished" (ardentia-test--shown)))
           ;; a failing fetch keeps the old list and reports in the header
           (setq ardentia-org-tool (expand-file-name "ai/tools/fail-tool" dir))
           (ardentia-dashboard--completed-refresh)
           (ardentia-test--wait (lambda () (not ardentia-dashboard--completed-refreshing)))
           (should (string-match-p "failed: boom" ardentia-dashboard--completed-error))
           (should (string-match-p "ERROR: org-tool humanreview failed: boom"
                                   (ardentia-test--header)))
           (should (= (length (ardentia-test--shown)) 3))
           ;; the next good fetch clears the error
           (setq ardentia-org-tool (expand-file-name "ai/tools/org-tool" dir))
           (ardentia-dashboard--completed-refresh)
           (ardentia-test--wait (lambda () (not ardentia-dashboard--completed-refreshing)))
           (should (null ardentia-dashboard--completed-error)))
       (kill-buffer buf)
       (delete-directory dir t)))))

(ert-deftest ardentia-test-list-refresh-all-fetches-completed-only-there ()
  "`g' and the timer tick fetch the completed data while in that view."
  (ardentia-test--with-list
    (let ((fetches 0))
      (cl-letf (((symbol-function 'ardentia-dashboard--completed-refresh)
                 (lambda () (cl-incf fetches)))
                ((symbol-function 'ardentia-refresh) #'ignore)
                ((symbol-function 'ardentia-usage-maybe-refresh) #'ignore))
        (ardentia-dashboard-refresh-all)
        (should (= fetches 0))
        (ardentia-dashboard--switch-view 'completed)
        (should (= fetches 1))                    ; the switch
        (ardentia-dashboard-refresh-all)
        (should (= fetches 2))))))

(ert-deftest ardentia-test-list-older-buffer-without-view ()
  "A buffer made by code without views renders as dashboard or review."
  (ardentia-test--with-list
    (setq ardentia-dashboard--view nil
          ardentia-dashboard--format-kind nil)
    (ardentia-dashboard--render)
    (should (eq (ardentia-dashboard--view-now) 'dashboard))
    (should (= (length (ardentia-test--shown)) 5))
    (should (string-prefix-p " [Dashboard]" (ardentia-test--header)))
    (should (equal (car (aref tabulated-list-format 0)) "Queue"))
    ;; the old review inbox: filter = review, no view
    (setq ardentia-dashboard--view nil
          ardentia-dashboard--format-kind nil
          ardentia-dashboard--filter 'review)
    (ardentia-dashboard--render)
    (should (equal (sort (copy-sequence (ardentia-test--shown)) #'string<)
                   '("Alpha" "Beta")))
    (should (string-prefix-p " Dashboard [Review]" (ardentia-test--header)))
    ;; a stale format of the other kind is replaced
    (setq ardentia-dashboard--view nil
          ardentia-dashboard--filter nil
          ardentia-dashboard--format-kind nil
          tabulated-list-format (copy-tree ardentia-completed--format t))
    (ardentia-dashboard--render)
    (should (equal (car (aref tabulated-list-format 0)) "Queue"))
    (should (= (length (ardentia-test--shown)) 5))))

(ert-deftest ardentia-test-list-commands-share-one-buffer ()
  (ardentia-test--with-timer-sandbox
   (let ((ardentia--state (list :tasks (ardentia-test--open-tasks) :host "alpha"))
         (refreshes 0))
     (when (get-buffer "*ardentia*") (kill-buffer "*ardentia*"))
     (unwind-protect
         (cl-letf (((symbol-function 'pop-to-buffer-same-window) #'ignore)
                   ((symbol-function 'ardentia-dashboard-refresh-all)
                    (lambda () (cl-incf refreshes)))
                   ((symbol-function 'ardentia-dashboard--completed-refresh)
                    #'ignore))
           (ardentia-review-inbox)
           (let ((buf (get-buffer "*ardentia*")))
             (should buf)
             (should (= refreshes 1))
             (with-current-buffer buf
               (should (derived-mode-p 'ardentia-dashboard-mode))
               (should (eq (ardentia-dashboard--view-now) 'review)))
             (ardentia-completed)
             (should (eq (get-buffer "*ardentia*") buf))
             (should (eq (buffer-local-value 'ardentia-dashboard--view buf)
                         'completed))
             (ardentia-dashboard)
             (should (eq (buffer-local-value 'ardentia-dashboard--view buf)
                         'dashboard))
             (should (= refreshes 3))
             ;; from inside the buffer the shared data is current: no refresh
             (with-current-buffer buf
               (ardentia-review-inbox)
               (should (= refreshes 3))
               (should (eq (ardentia-dashboard--view-now) 'review)))
             (should-not (get-buffer "*ardentia-review*"))
             (should-not (get-buffer "*ardentia-completed*"))
             (should (timerp ardentia-dashboard--refresh-timer))))
       (when (get-buffer "*ardentia*") (kill-buffer "*ardentia*"))))))

(ert-deftest ardentia-test-list-accept-and-send-back-refuse-accepted ()
  (ardentia-test--with-list
    (ardentia-dashboard--switch-view 'completed)
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (&rest _) (ert-fail "must not ask")))
              ((symbol-function 'read-string)
               (lambda (&rest _) (ert-fail "must not ask")))
              ((symbol-function 'ardentia-accept-1)
               (lambda (&rest _) (ert-fail "must not accept")))
              ((symbol-function 'ardentia-send-back-1)
               (lambda (&rest _) (ert-fail "must not send back"))))
      (ardentia-test--goto "Zeta")
      (dolist (cmd '(ardentia-dashboard-accept ardentia-dashboard-send-back))
        (let ((err (should-error (funcall cmd) :type 'user-error)))
          (should (string-match-p "already accepted" (cadr err))))))
    ;; an open task of the same view can be accepted
    (let (accepted)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'ardentia-accept-1)
                 (lambda (file id) (setq accepted (list file id)))))
        (ardentia-test--goto "Alpha")
        (ardentia-dashboard-accept)
        (should (equal accepted '("/o/l.org" "id-alpha")))))))

(ert-deftest ardentia-test-list-enter-opens-accepted-review ()
  (ardentia-test--with-list
    (ardentia-dashboard--switch-view 'completed)
    (ardentia-test--goto "Zeta")
    (let (opened)
      (cl-letf (((symbol-function 'ardentia-review-task)
                 (lambda (task) (setq opened (plist-get task :title)))))
        (ardentia-dashboard-enter))
      (should (equal opened "Zeta")))))

(ert-deftest ardentia-test-list-ready-without-id-through-the-list ()
  "A ready task without :ID: is found by line and title from the list."
  (ardentia-test--with-org-file f "* TODO Other\n* TODO Fresh :aiready:\nbody\n"
    (ardentia-test--with-timer-sandbox
     (let ((ardentia--state
            (list :tasks (list (list :file f :id nil :line 2 :title "Fresh"
                                     :queue 'ready :tags '("aiready")
                                     :props nil))
                  :host "alpha"))
           (buf (get-buffer-create " *ardentia-test-noid*")))
       (unwind-protect
           (with-current-buffer buf
             (ardentia-dashboard-mode)
             (ardentia-dashboard--render)
             (ardentia-test--goto "Fresh")
             (cl-letf (((symbol-function 'ardentia-dashboard-refresh-all) #'ignore))
               (ardentia-dashboard-priority-up))
             (should (string-match-p "^\\* TODO \\[#C\\] Fresh :aiready:$"
                                     (ardentia-test--file-text f))))
         (kill-buffer buf))))))

(ert-deftest ardentia-test-list-filter-menu-shows-values ()
  (ardentia-test--with-list
    (let ((desc (lambda (key)
                  (let ((suffix (transient-get-suffix
                                 'ardentia-dashboard-filter-menu key)))
                    (plist-get (if (consp (car (last suffix))) (car (last suffix))
                                 (cdr suffix))
                               :description)))))
      (setq ardentia-dashboard--filter-host "beta")
      (ardentia-dashboard-filter-title "x")
      (should (equal (funcall (funcall desc "h")) "Host: beta"))
      (should (equal (funcall (funcall desc "/")) "Title: /x/"))
      (should (equal (funcall (funcall desc "l")) "Later tasks: shown"))
      (should (equal (funcall (funcall desc "P")) "Period: all"))
      (should (equal (funcall (funcall desc "w")) "State: all"))
      ;; greyed out where a filter does not apply
      (should-not (ardentia-dashboard--menu-inapt-p 'dashboard))
      (should (ardentia-dashboard--menu-inapt-p 'completed))
      (ardentia-dashboard--switch-view 'completed)
      (should (ardentia-dashboard--menu-inapt-p 'dashboard))
      (should-not (ardentia-dashboard--menu-inapt-p 'completed))
      (should (eq (ardentia-test--suffix-command 'ardentia-dashboard-filter-menu "c")
                  'ardentia-dashboard-clear-filters))
      (should (eq (ardentia-test--suffix-command 'ardentia-dashboard-menu "f")
                  'ardentia-dashboard-filter-menu)))))

(provide 'ardentia-test)
;;;; Usage

(defconst ardentia-test--usage-json
  "{\"five_hour\":{\"utilization\":99.0,\"resets_at\":\"2026-01-01T14:00:00.1+00:00\"},
 \"extra_usage\":{\"is_enabled\":true,\"monthly_limit\":50.0,\"used_credits\":12.5,\"currency\":\"EUR\"},
 \"limits\":[{\"kind\":\"session\",\"percent\":42,\"severity\":\"normal\",\"resets_at\":\"2026-01-01T14:00:00.1+00:00\",\"scope\":null,\"is_active\":true},
  {\"kind\":\"weekly_all\",\"percent\":7,\"severity\":\"normal\",\"resets_at\":\"2026-01-05T04:00:00+00:00\",\"scope\":null},
  {\"kind\":\"weekly_scoped\",\"percent\":0,\"severity\":\"normal\",\"resets_at\":null,\"scope\":{\"model\":{\"id\":null,\"display_name\":\"IdleModel\"}}},
  {\"kind\":\"weekly_scoped\",\"percent\":30,\"severity\":\"normal\",\"resets_at\":null,\"scope\":{\"model\":{\"id\":null,\"display_name\":\"SomeModel\"}}}]}"
  "Invented usage response.")

(defconst ardentia-test--usage-now (encode-time '(0 0 10 1 1 2026 nil nil t))
  "2026-01-01 10:00 UTC.")

(defun ardentia-test--usage-text (state)
  "Format STATE in UTC at the fixed test time, without properties."
  (let ((ardentia-usage--zone "UTC"))
    (substring-no-properties
     (ardentia-usage--format state ardentia-test--usage-now))))

(ert-deftest ardentia-test-usage-parse-limits ()
  (let* ((state (ardentia-usage--parse ardentia-test--usage-json))
         (limits (plist-get state :limits)))
    (should (= (length limits) 4))
    (should (equal (mapcar (lambda (l) (plist-get l :label)) limits)
                   '("5h" "week" "IdleModel week" "SomeModel week")))
    (should (= (plist-get (car limits) :percent) 42))
    (should (plist-get (car limits) :resets-at))
    (should (plist-get (car limits) :active))
    (should (equal (plist-get (plist-get state :extra) :currency) "EUR"))))

(ert-deftest ardentia-test-usage-format ()
  (let ((state (append (list :time ardentia-test--usage-now)
                       (ardentia-usage--parse ardentia-test--usage-json))))
    (should (equal (ardentia-test--usage-text state)
                   (concat "5h 42% (resets 14:00) | week 7% (resets Mon 04:00)"
                           " | SomeModel week 30% | extra 12.50/50.00 EUR")))
    (let ((ardentia-usage-hide-idle-scoped nil))
      (should (string-match-p "IdleModel week 0%"
                              (ardentia-test--usage-text state))))))

(ert-deftest ardentia-test-usage-fallback-without-limits ()
  (let ((state (ardentia-usage--parse
                "{\"five_hour\":{\"utilization\":10.0,\"resets_at\":\"2026-01-01T12:30:00+00:00\"},
 \"seven_day\":{\"utilization\":3.0,\"resets_at\":null},\"seven_day_opus\":null}")))
    (should (equal (ardentia-test--usage-text state)
                   "5h 10% (resets 12:30) | week 3%"))))

(ert-deftest ardentia-test-usage-garbage ()
  (dolist (json '("" "garbage" "null" "[]" "{}" "{\"limits\":null}"
                  "{\"five_hour\":null,\"seven_day\":null}"
                  "{\"limits\":[null,1,\"x\"]}"))
    (should-not (ardentia-usage--parse json))))

(ert-deftest ardentia-test-usage-extra-disabled ()
  (let ((state (ardentia-usage--parse
                "{\"limits\":[{\"kind\":\"session\",\"percent\":1}],
 \"extra_usage\":{\"is_enabled\":false,\"used_credits\":0.0}}")))
    (should (equal (ardentia-test--usage-text state) "5h 1%"))))

(ert-deftest ardentia-test-usage-faces ()
  (let ((ardentia-usage-warn-percent 80))
    (should-not (ardentia-usage--face 79 "normal"))
    (should (eq (ardentia-usage--face 80 "normal") 'warning))
    (should (eq (ardentia-usage--face 10 "warning") 'warning))
    (should (eq (ardentia-usage--face 100 "normal") 'error))
    (should (eq (ardentia-usage--face 10 "exceeded") 'error))
    (should (eq (get-text-property
                 0 'face (ardentia-usage--format-limit
                          (list :label "5h" :percent 85)))
                'warning))))

(ert-deftest ardentia-test-usage-time-format ()
  (let ((ardentia-usage--zone "UTC"))
    (should (equal (ardentia-usage--format-time
                    (encode-time '(0 0 23 1 1 2026 nil nil t))
                    ardentia-test--usage-now)
                   "23:00"))
    (should (equal (ardentia-usage--format-time
                    (encode-time '(0 30 1 2 1 2026 nil nil t))
                    ardentia-test--usage-now)
                   "Fri 01:30"))))

(ert-deftest ardentia-test-usage-stale ()
  (let ((state (append (list :time (time-subtract ardentia-test--usage-now 720))
                       (ardentia-usage--parse
                        "{\"limits\":[{\"kind\":\"session\",\"percent\":1}]}")))
        (ardentia-usage-min-interval 300))
    (should (string-suffix-p "(stale 12m)" (ardentia-test--usage-text state)))
    (let ((ardentia-usage-min-interval 600))
      (should-not (string-match-p "stale" (ardentia-test--usage-text state))))))

(ert-deftest ardentia-test-usage-result ()
  (let ((ok (ardentia-usage--result
             (concat "{\"limits\":[{\"kind\":\"session\",\"percent\":5}]}\n200") 0)))
    (should-not (plist-get ok :error))
    (should (plist-get ok :limits)))
  (should (string-match-p "claude once"
                          (plist-get (ardentia-usage--result "x\n401" 0) :error)))
  (should (equal (plist-get (ardentia-usage--result "x\n429" 0) :error) "HTTP 429"))
  (should (equal (plist-get (ardentia-usage--result "" 6) :error) "no network"))
  (should (equal (plist-get (ardentia-usage--result "nonsense\n200" 0) :error)
                 "unexpected response"))
  (should (string-match-p "unavailable (HTTP 429)"
                          (ardentia-test--usage-text
                           (ardentia-usage--result "x\n429" 0)))))

(ert-deftest ardentia-test-usage-pause ()
  (let ((file (make-temp-file "ardentia-limit"))
        (ardentia-usage--zone "UTC"))
    (unwind-protect
        (progn
          (with-temp-file file (insert "1767272400\tsession limit\n"))
          (let ((pause (ardentia-usage--read-pause file)))
            (should (equal pause '(1767272400 . "session limit")))
            (should (equal (substring-no-properties
                            (ardentia-usage--pause-text
                             pause (seconds-to-time 1767268800)))
                           "runner paused until 13:00"))
            (should-not (ardentia-usage--pause-text
                         pause (seconds-to-time 1767272401))))
          (with-temp-file file (insert "soon\ttext\n"))
          (should-not (ardentia-usage--read-pause file))
          (with-temp-file file (insert ""))
          (should-not (ardentia-usage--read-pause file))
          (should-not (ardentia-usage--read-pause (concat file ".absent"))))
      (delete-file file))))

(ert-deftest ardentia-test-usage-throttle ()
  (let ((ardentia-usage-enabled t)
        (ardentia-usage-min-interval 300)
        (ardentia-usage-limit-state-file nil)
        (ardentia-usage--last-attempt nil)
        (ardentia-usage--process nil)
        (calls 0))
    (cl-letf (((symbol-function 'ardentia-usage--fetch)
               (lambda () (cl-incf calls))))
      (ardentia-usage-maybe-refresh)
      (should (= calls 1))
      (ardentia-usage-maybe-refresh)
      (should (= calls 1))
      (ardentia-usage-maybe-refresh t)
      (should (= calls 2))
      (setq ardentia-usage--last-attempt
            (time-subtract (current-time) 301))
      (ardentia-usage-maybe-refresh)
      (should (= calls 3))
      (let ((ardentia-usage-enabled nil))
        (setq ardentia-usage--last-attempt nil)
        (ardentia-usage-maybe-refresh t)
        (should (= calls 3))))))

(ert-deftest ardentia-test-usage-fetch-without-credentials ()
  (let ((ardentia-usage-credentials-file "/nonexistent/credentials.json")
        (ardentia-usage--state nil)
        (ardentia-usage-updated-hook nil))
    (ardentia-usage--fetch)
    (should (equal (plist-get ardentia-usage--state :error)
                   "no credentials"))))

(ert-deftest ardentia-test-usage-info-line ()
  (let* ((ardentia-usage-enabled t)
         (ardentia-usage--zone "UTC")
         (ardentia-usage--pause nil)
         (st (list :host "desktop" :time (current-time)))
         (ardentia-usage--state nil))
    (should (string-match-p "Host: desktop"
                            (ardentia-dashboard--info-line st)))
    (should-not (string-match-p "5h" (ardentia-dashboard--info-line st)))
    (setq ardentia-usage--state
          (append (list :time (current-time))
                  (ardentia-usage--parse ardentia-test--usage-json))
          ardentia-usage--pause
          (cons (+ (floor (float-time)) 3600) "session limit"))
    (let ((line (substring-no-properties (ardentia-dashboard--info-line st))))
      (should (string-match-p "Host: desktop" line))
      (should (string-match-p " | 5h 42% (resets " line))
      (should (string-match-p "SomeModel week 30%" line))
      (should (string-match-p "runner paused until " line)))
    (let ((ardentia-usage-enabled nil))
      (should-not (string-match-p "5h" (ardentia-dashboard--info-line st))))))


;;;; Dashboard header line layout

(defun ardentia-test--layout-state ()
  "Return a state with long timer stamps and usage data."
  (setq ardentia-usage--state
        (append (list :time (current-time))
                (ardentia-usage--parse ardentia-test--usage-json)))
  (list :host "desktop" :time (current-time)
        :timer '(:last "Sun 2026-10-11 08:45:17 CEST"
                 :next "Sun 2026-10-11 09:00:12 CEST")))

(defun ardentia-test--squash (string)
  "Return STRING without whitespace and bars, for content comparison."
  (replace-regexp-in-string "[ \t\n|]" "" (substring-no-properties string)))

(ert-deftest ardentia-test-short-time ()
  (let ((now (encode-time '(0 0 12 11 10 2026 nil -1 nil))))
    (should (equal (ardentia-dashboard--short-time
                    "Sun 2026-10-11 08:45:17 CEST" now)
                   "08:45"))
    (should (equal (ardentia-dashboard--short-time
                    "Mon 2026-10-12 09:00:12 CEST" now)
                   "Mon 09:00"))
    (should (null (ardentia-dashboard--short-time nil now)))
    (should (equal (ardentia-dashboard--short-time "after the current run" now)
                   "after the current run"))
    (should (equal (ardentia-dashboard--short-time "L" now) "L"))))

(ert-deftest ardentia-test-info-segments ()
  (let* ((ardentia-usage-enabled t)
         (ardentia-usage--zone "UTC")
         (ardentia-usage--pause nil)
         (ardentia-usage--state nil)
         (st (ardentia-test--layout-state))
         (segs (ardentia-dashboard--info-segments st)))
    (should (equal (substring-no-properties (car segs))
                   " [Dashboard] Review Completed"))
    (should (string-prefix-p " | Host: desktop" (cadr segs)))
    (dolist (seg (cdr segs))
      (should (string-prefix-p " | " seg)))
    (should (equal (apply #'concat segs)
                   (ardentia-dashboard--info-line st)))
    (should (> (length segs) 4))))

(ert-deftest ardentia-test-layout-wide ()
  (let* ((ardentia-usage-enabled t)
         (ardentia-usage--zone "UTC")
         (ardentia-usage--pause nil)
         (ardentia-usage--state nil)
         (st (ardentia-test--layout-state))
         (n (length (ardentia-dashboard--info-segments st))))
    (should (equal (ardentia-dashboard--layout nil st) (list n nil nil)))
    (should (equal (ardentia-dashboard--layout 500 st) (list n nil nil)))))

(ert-deftest ardentia-test-layout-narrow ()
  (let* ((ardentia-usage-enabled t)
         (ardentia-usage--zone "UTC")
         (ardentia-usage--pause nil)
         (ardentia-usage--state nil)
         (st (ardentia-test--layout-state)))
    (dolist (width '(80 30))
      (pcase-let* ((`(,count ,compact ,lines)
                    (ardentia-dashboard--layout width st))
                   (segs (ardentia-dashboard--info-segments st t))
                   (head (apply #'concat (seq-take segs count))))
        (should compact)
        (should lines)
        (should (>= count 1))
        (when (> count 1) (should (<= (string-width head) width)))
        (dolist (l lines)
          (should (string-prefix-p " " l))
          (should-not (string-prefix-p " |" l))
          (when (> (length (split-string l " | ")) 1)
            (should (<= (string-width l) width))))
        ;; Nothing lost or duplicated.
        (should (equal (ardentia-test--squash
                        (concat head (apply #'concat lines)))
                       (ardentia-test--squash (apply #'concat segs))))))
    (let ((segs (ardentia-dashboard--info-segments st t)))
      (should (equal (nth 2 segs) " | Timer last: 08:45 next: 09:00"))
      (should (= (length (ardentia-dashboard--layout 30 st))
                 3)))))

(ert-deftest ardentia-test-escape-percent ()
  (let* ((s (concat "a " (propertize "5%" 'face 'warning) " b%"))
         (e (ardentia-dashboard--escape-percent s)))
    (should (equal (substring-no-properties e) "a 5%% b%%"))
    (should (eq (get-text-property 2 'face e) 'warning))
    (should (eq (get-text-property 4 'face e) 'warning))
    (should (equal (ardentia-dashboard--escape-percent "") ""))))

(ert-deftest ardentia-test-header-line-cached-count ()
  (let* ((ardentia-usage-enabled t)
         (ardentia-usage--zone "UTC")
         (ardentia-usage--pause nil)
         (ardentia-usage--state nil)
         (ardentia--state (ardentia-test--layout-state)))
    (let ((ardentia-dashboard--layout-cache nil))
      (should (string-match-p "5h 42%% (resets "
                              (ardentia-dashboard--header-line))))
    (let ((ardentia-dashboard--layout-cache '(3 . t)))
      (let ((line (ardentia-dashboard--header-line)))
        (should (equal (substring-no-properties line)
                       (concat " [Dashboard] Review Completed"
                               " | Host: desktop"
                               " | Timer last: 08:45 next: 09:00")))))))

(ert-deftest ardentia-test-update-layout-overlay ()
  (let* ((ardentia-usage-enabled t)
         (ardentia-usage--zone "UTC")
         (ardentia-usage--pause nil)
         (ardentia-usage--state nil)
         (ardentia--state (ardentia-test--layout-state)))
    (with-temp-buffer
      (ardentia-dashboard-mode)
      (cl-letf (((symbol-function 'ardentia-dashboard--width) (lambda () 40)))
        (ardentia-dashboard--update-layout)
        (should (overlayp ardentia-dashboard--overflow-overlay))
        (should (= (overlay-start ardentia-dashboard--overflow-overlay)
                   (point-min)))
        (should (string-suffix-p "\n" (overlay-get
                                       ardentia-dashboard--overflow-overlay
                                       'before-string))))
      (cl-letf (((symbol-function 'ardentia-dashboard--width) (lambda () nil)))
        (ardentia-dashboard--update-layout)
        (should-not ardentia-dashboard--overflow-overlay)))))

;;;; Browsers

(defmacro ardentia-test--with-browse-dirs (&rest body)
  "Run BODY with the browser directories bound to fresh temp dirs.
`kdir' and `adir' name the knowledge and the agents directory."
  (declare (indent 0))
  `(let* ((root (make-temp-file "ardentia-browse" t))
          (kdir (file-name-as-directory (expand-file-name "k" root)))
          (adir (file-name-as-directory (expand-file-name "a" root)))
          (ardentia-knowledge-directory kdir)
          (ardentia-agents-directory adir)
          (ardentia-knowledge-large-file-size 100))
     (make-directory kdir)
     (make-directory adir)
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

(defun ardentia-test--bwrite (dir name text)
  "Write TEXT to NAME in DIR and return the full file name."
  (let ((file (expand-file-name name dir)))
    (with-temp-file file (insert text))
    file))

(ert-deftest ardentia-test-frontmatter-full ()
  (let ((fm (ardentia-browse--frontmatter
             "---\nname: worker\nmodel: sonnet\nmaxTurns: 30\n---\nbody: x\n")))
    (should (equal (cdr (assoc "name" fm)) "worker"))
    (should (equal (cdr (assoc "maxTurns" fm)) "30"))
    (should-not (assoc "body" fm))))

(ert-deftest ardentia-test-frontmatter-missing-keys ()
  (let ((fm (ardentia-browse--frontmatter "---\nname: a\n---\n")))
    (should (equal fm '(("name" . "a"))))
    (should-not (assoc "model" fm))))

(ert-deftest ardentia-test-frontmatter-none ()
  (should-not (ardentia-browse--frontmatter "# Title\nname: a\n"))
  (should-not (ardentia-browse--frontmatter "---\nname: a\n"))
  (should-not (ardentia-browse--frontmatter "")))

(ert-deftest ardentia-test-frontmatter-colon-in-value ()
  (let ((fm (ardentia-browse--frontmatter
             "---\ndescription: Does a: b -- and c\n---\n")))
    (should (equal (cdr (assoc "description" fm)) "Does a: b -- and c"))))

(ert-deftest ardentia-test-knowledge-rows ()
  (ardentia-test--with-browse-dirs
    (ardentia-test--bwrite kdir "small.md" "intro\n# The Title\ntext\n")
    (ardentia-test--bwrite kdir "big.md"
                          (concat "# Big\n" (make-string 200 ?x)))
    (ardentia-test--bwrite kdir ".hidden.md" "# H\n")
    (ardentia-test--bwrite kdir "small.md~" "# B\n")
    (let ((entries (ardentia-knowledge--entries)))
      (should (equal (mapcar (lambda (e) (file-name-nondirectory (car e)))
                             entries)
                     '("big.md" "small.md")))
      (let ((big (cadr (nth 0 entries)))
            (small (cadr (nth 1 entries))))
        (should (equal (aref small 4) "The Title"))
        (should (equal (aref small 3) ""))
        (should (equal (aref big 3) "grep only"))
        (should (equal (aref big 4) "Big"))
        (should (string-match-p "\\`[0-9]+\\(\\.[0-9]\\)?[kM]?\\'"
                                (aref small 1)))
        (should (string-match-p "\\`[0-9-]+ [0-9:]+\\'" (aref small 2)))))))

(ert-deftest ardentia-test-knowledge-ret-large-greps ()
  (ardentia-test--with-browse-dirs
    (let ((big (ardentia-test--bwrite kdir "big.md" (make-string 500 ?x)))
          greps visits)
      (with-temp-buffer
        (ardentia-knowledge-mode)
        (ardentia-knowledge-refresh)
        (goto-char (point-min))
        (cl-letf (((symbol-function 'ardentia-knowledge--grep)
                   (lambda (re files) (push (list re files) greps)))
                  ((symbol-function 'read-regexp) (lambda (&rest _) "foo"))
                  ((symbol-function 'view-file)
                   (lambda (f) (push f visits)))
                  ((symbol-function 'find-file)
                   (lambda (f) (push f visits))))
          (ardentia-knowledge-open)))
      (should-not visits)
      (should (equal greps (list (list "foo" (list big))))))))

(ert-deftest ardentia-test-knowledge-ret-small-views ()
  (ardentia-test--with-browse-dirs
    (let ((small (ardentia-test--bwrite kdir "small.md" "# S\n"))
          greps visits)
      (with-temp-buffer
        (ardentia-knowledge-mode)
        (ardentia-knowledge-refresh)
        (goto-char (point-min))
        (cl-letf (((symbol-function 'ardentia-knowledge--grep)
                   (lambda (&rest a) (push a greps)))
                  ((symbol-function 'view-file)
                   (lambda (f) (push f visits))))
          (ardentia-knowledge-open)))
      (should-not greps)
      (should (equal visits (list small))))))

(ert-deftest ardentia-test-agents-rows ()
  (ardentia-test--with-browse-dirs
    (ardentia-test--bwrite
     adir "worker.md"
     (concat "---\nname: worker\ndescription: Does x: y\nmodel: sonnet\n"
             "tools: Read, Write\nomitClaudeMd: true\n"
             "permissionMode: acceptEdits\nmaxTurns: 30\n---\nbody\n"))
    (ardentia-test--bwrite adir "plain.md" "no frontmatter\n")
    (ardentia-test--bwrite adir "notes.txt" "ignored\n")
    (let ((entries (ardentia-agents--entries)))
      (should (equal (length entries) 2))
      (should (equal (cadr (nth 0 entries))
                     ["plain" "-" "-" "-" "-" "-" "-"]))
      (should (equal (cadr (nth 1 entries))
                     ["worker" "sonnet" "Read, Write" "acceptEdits" "30"
                      "true" "Does x: y"])))))

(ert-deftest ardentia-test-browse-missing-directory ()
  (let ((ardentia-knowledge-directory "/nonexistent-ardentia-dir/")
        (ardentia-agents-directory "/nonexistent-ardentia-dir/"))
    (should-error (ardentia-knowledge--entries) :type 'user-error)
    (should-error (ardentia-agents--entries) :type 'user-error)
    (should-error (ardentia-knowledge) :type 'user-error)
    (should-error (ardentia-agents) :type 'user-error)))


;;; Per-buffer help menus

(defun ardentia-test--suffix-command (prefix key)
  "Return the command of the suffix on KEY in transient PREFIX, or nil.
Uses `transient-get-suffix' and copes with both suffix shapes (object, or
\(LEVEL CLASS PLIST) list)."
  (let ((suffix (ignore-errors (transient-get-suffix prefix key))))
    (cond ((null suffix) nil)
          ((consp suffix)
           ;; (CLASS . PLIST) or (LEVEL CLASS PLIST)
           (plist-get (if (consp (car (last suffix))) (car (last suffix)) (cdr suffix))
                      :command))
          (t (eieio-oref suffix 'command)))))

(defconst ardentia-test--menus
  '((ardentia-dashboard-mode-map ardentia-dashboard-menu 25)
    (ardentia-review-mode-map ardentia-review-menu 7)
    (ardentia-knowledge-mode-map ardentia-knowledge-menu 5)
    (ardentia-agents-mode-map ardentia-agents-menu 3)
    (ardentia-session-mode-map ardentia-session-menu 9))
  "Each element: (MAP PREFIX MINIMUM-ENTRY-COUNT).")

(defun ardentia-test--map-bindings (map &optional prefix)
  "Return an alist (KEY-DESCRIPTION . COMMAND) of all bindings in MAP.
Nested prefix keymaps are flattened; PREFIX is the key vector so far.
Bindings inherited from a parent keymap are not included."
  (let ((map (let ((copy (copy-keymap map))) (set-keymap-parent copy nil) copy))
        acc)
    (map-keymap (lambda (ev def)
                  (let ((keys (vconcat prefix (vector ev))))
                    (if (keymapp def)
                        (setq acc (append (ardentia-test--map-bindings def keys) acc))
                      (push (cons (key-description keys) def) acc))))
                map)
    acc))

(ert-deftest ardentia-test-help-menus-match-keymaps ()
  (dolist (spec ardentia-test--menus)
    (pcase-let* ((`(,map-sym ,prefix ,minimum) spec)
                 (map (symbol-value map-sym))
                 (data (get prefix 'ardentia-menu))
                 (entries (mapcan (lambda (g) (copy-sequence (cdr g)))
                                  (plist-get data :groups)))
                 (alternates (plist-get data :alternates)))
      (should (commandp prefix))
      (should (>= (length entries) minimum))
      (should (eq (keymap-lookup map "?") prefix))
      ;; menu -> keymap, via the public transient API
      (dolist (e entries)
        (should (eq (ardentia-test--suffix-command prefix (nth 0 e)) (nth 2 e)))
        (should (eq (keymap-lookup map (nth 0 e)) (nth 2 e))))
      ;; global dispatch entry, also bound directly in the mode map
      (should (eq (ardentia-test--suffix-command prefix ".") 'ardentia))
      (should (eq (keymap-lookup map ".") 'ardentia))
      ;; keymap -> menu: every binding is in the menu, an alternate or "?"
      (let ((bindings (ardentia-test--map-bindings map)))
        (should (>= (length bindings) (+ 2 minimum)))
        (dolist (b bindings)
          (unless (equal (car b) "?")
            (should (or (equal (ardentia-test--suffix-command prefix (car b))
                               (cdr b))
                        (equal (cdr (assoc (car b) alternates)) (cdr b))))))))))

(ert-deftest ardentia-test-compose-map-keeps-dot-free ()
  "The compose buffer is for typing text, so `.' must self-insert there."
  (should-not (eq (keymap-lookup ardentia-session-compose-mode-map ".")
                  'ardentia)))

(ert-deftest ardentia-test-help-menus-alternates-agree ()
  "Alternate keys are bound to a command the menu offers."
  (let ((n 0))
    (dolist (spec ardentia-test--menus)
      (pcase-let* ((`(,map-sym ,prefix ,_) spec)
                   (data (get prefix 'ardentia-menu))
                   (cmds (mapcar #'caddr
                                 (mapcan (lambda (g) (copy-sequence (cdr g)))
                                         (plist-get data :groups)))))
        (dolist (a (plist-get data :alternates))
          (setq n (1+ n))
          (should (memq (cdr a) cmds))
          (should (eq (keymap-lookup (symbol-value map-sym) (car a)) (cdr a))))))
    (should (= n 6))))

;;;; The "For you" line of the run report

(ert-deftest ardentia-test-for-you-forms ()
  (let ((head "- AI-Lauf [2026-10-11 Sun 18:59] completed: Done \\\\"))
    (should (equal (ardentia--for-you (list head "  - For you: nothing to do"))
                   '(:class nothing :text "nothing to do" :lines nil)))
    (should (equal (ardentia--for-you
                    (list head "  - For you: REQUIRED -- [[id:x1][Approve it]]"
                          "  - Details"))
                   '(:class required :text "REQUIRED -- [[id:x1][Approve it]]"
                            :lines nil)))
    (should (equal (plist-get
                    (ardentia--for-you
                     (list head "  - For you: optional only -- rename it"))
                    :class)
                   'optional))
    (should (equal (ardentia--for-you
                    (list head "  - For you: 2 REQUIRED, 1 optional"
                          "    - REQUIRED: [[id:a][First]]"
                          "    - optional: keep the name"
                          "  - Details"))
                   '(:class required :text "2 REQUIRED, 1 optional"
                            :lines ("REQUIRED: [[id:a][First]]"
                                    "optional: keep the name"))))))

(ert-deftest ardentia-test-for-you-absent ()
  (let ((head "- AI-Lauf [2026-10-11 Sun 18:59] completed: Done \\\\"))
    (should (null (ardentia--for-you nil)))
    (should (null (ardentia--for-you (list head))))
    (should (null (ardentia--for-you (list head "  - Details"))))
    ;; not the first sub-item
    (should (null (ardentia--for-you
                   (list head "  - Details" "  - For you: REQUIRED -- x"))))
    ;; unrecognised wording
    (should (null (ardentia--for-you (list head "  - For you: hmm"))))))

(ert-deftest ardentia-test-for-you-older-report-ignored ()
  (let* ((log "- AI-Lauf [2026-10-11 Sun 18:59] completed: New \\\\
  - Details
- AI-Lauf [2026-10-10 Sat 10:00] completed: Old \\\\
  - For you: REQUIRED -- stale
")
         (report (plist-get (ardentia--extract-run-report log) :report)))
    (should (equal (length report) 2))
    (should (null (ardentia--for-you report)))))

(defconst ardentia-test--for-you-org "* TODO Task fy :humanreview:
:PROPERTIES:
:ID:       id-fy
:AI_STATUS: completed
:AI_SESSION: s1
:END:
:LOGBOOK:
- AI-Lauf [2026-10-11 Sun 18:59] completed: Done \\\\
  - For you: 2 REQUIRED, 1 optional
    - REQUIRED: [[id:a][First]]
    - optional: keep the name
  - Details
- AI-Lauf [2026-10-10 Sat 10:00] completed: Old \\\\
  - something
:END:
Text
")

(ert-deftest ardentia-test-for-you-review-header ()
  (ardentia-test--with-org-file f ardentia-test--for-you-org
    (let ((text (ardentia-review--render-string (ardentia-review--collect f "id-fy"))))
      (should (string-match-p
               "^- Status :: TODO, agents: completed\n- For you :: 2 REQUIRED, 1 optional\n  - REQUIRED: \\[\\[id:a\\]\\[First\\]\\]\n  - optional: keep the name\n\\*\\* Agent properties"
               text))))
  (ardentia-test--with-org-file f ardentia-test--org
    (should-not (string-match-p
                 "For you ::"
                 (ardentia-review--render-string (ardentia-review--collect f "id-one"))))))

(ert-deftest ardentia-test-for-you-flag ()
  (let ((now (ardentia-test--t "20261011-1200")))
    (should (equal (aref (ardentia--task-row
                          (ardentia-test--task :queue 'review) now 'required)
                         5)
                   "you!"))
    (should (equal (aref (ardentia--task-row
                          (ardentia-test--task :queue 'review) now 'optional)
                         5)
                   "opt"))
    (dolist (c '(nothing nil))
      (should (equal (aref (ardentia--task-row
                            (ardentia-test--task :queue 'review) now c)
                           5)
                     "")))
    ;; combined with other flags
    (should (equal (aref (ardentia--task-row
                          (ardentia-test--task :queue 'review :trouble "error")
                          now 'required)
                         5)
                   "error:review you!"))))

(ert-deftest ardentia-test-for-you-class-cache-and-entries ()
  (clrhash ardentia-review--for-you-cache)
  (ardentia-test--with-org-file f ardentia-test--for-you-org
    (let* ((task (list :title "Task fy" :file f :id "id-fy" :queue 'review :tags nil
                       :props '(("AI_SESSION" . "s1"))))
           (now (ardentia-test--t "20261011-1200")))
      (should (eq (ardentia-review-for-you-class task) 'required))
      (should (equal (aref (cadr (car (ardentia-dashboard--entries
                                       (list task) nil now)))
                           5)
                     "you!"))
      ;; cached: the file may change without a new read for the same session
      (with-temp-file f (insert "* TODO other\n"))
      (should (eq (ardentia-review-for-you-class task) 'required))
      ;; a new session reads again
      (should (null (ardentia-review-for-you-class
                     (list :file f :id "id-fy" :queue 'review
                           :props '(("AI_SESSION" . "s2"))))))
      ;; errors give no flag
      (should (null (ardentia-review-for-you-class
                     (list :file "/nonexistent/x.org" :id "q" :queue 'review)))))))

;;;; Dashboard colors

(defun ardentia-test--faces-at (string pos)
  "Return the list of faces of STRING at POS."
  (let ((f (get-text-property pos 'face string)))
    (if (listp f) f (list f))))

(defun ardentia-test--has-face (string face)
  "Return non-nil when all of STRING has FACE."
  (and (> (length string) 0)
       (cl-every (lambda (i) (memq face (ardentia-test--faces-at string i)))
                 (number-sequence 0 (1- (length string))))))

(defun ardentia-test--any-face (string)
  "Return non-nil when STRING has a face property anywhere."
  (text-property-not-all 0 (length string) 'face nil string))

(defun ardentia-test--colored (task now &optional for-you)
  "Return the colorized row of TASK at NOW."
  (ardentia-dashboard--colorize-row
   task (ardentia--task-row task now for-you) now))

(ert-deftest ardentia-test-colors-queue-status ()
  (let ((now (ardentia-test--t "20261011-1200"))
        (ardentia-dashboard-colors t))
    (let ((row (ardentia-test--colored
                (ardentia-test--task :queue 'review
                                     :props '(("AI_STATUS" . "blocked")))
                now)))
      (should (ardentia-test--has-face (aref row 0) 'ardentia-queue-review))
      (should (ardentia-test--has-face (aref row 1) 'ardentia-status-blocked)))
    (let ((row (ardentia-test--colored
                (ardentia-test--task :queue 'review
                                     :props '(("AI_STATUS" . "completed")))
                now)))
      (should (ardentia-test--has-face (aref row 1) 'ardentia-status-completed)))
    (let ((row (ardentia-test--colored
                (ardentia-test--task :queue 'review
                                     :props '(("AI_STATUS" . "partial")))
                now)))
      (should (ardentia-test--has-face (aref row 1) 'ardentia-status-partial)))
    (let ((row (ardentia-test--colored
                (ardentia-test--task :queue 'review :trouble "error") now)))
      (should (ardentia-test--has-face (aref row 0) 'ardentia-queue-trouble)))
    (let ((row (ardentia-test--colored (ardentia-test--task :queue 'running) now)))
      (should (ardentia-test--has-face (aref row 0) 'ardentia-queue-running)))
    (let ((row (ardentia-test--colored
                (ardentia-test--task
                 :queue 'ready :scheduled "<2026-10-12 Mon 10:00>")
                now)))
      (should (equal (substring-no-properties (aref row 0)) "later"))
      (should (ardentia-test--has-face (aref row 0) 'ardentia-queue-later)))
    (let ((row (ardentia-test--colored (ardentia-test--task :queue 'ready) now)))
      (should (equal (substring-no-properties (aref row 0)) "ready"))
      (should (ardentia-test--has-face (aref row 0) 'ardentia-queue-ready)))))

(ert-deftest ardentia-test-colors-priority-token ()
  (let ((now (ardentia-test--t "20261011-1200"))
        (ardentia-dashboard-colors t))
    (let* ((row (ardentia-test--colored
                 (ardentia-test--task :queue 'ready :priority "A") now))
           (title (aref row 8)))
      (should (equal (substring-no-properties title) "[#A] T"))
      (should (ardentia-test--has-face (substring title 0 4)
                                       'ardentia-priority-highest))
      (should-not (ardentia-test--any-face (substring title 4))))
    (let* ((row (ardentia-test--colored
                 (ardentia-test--task :queue 'ready :priority "B") now))
           (title (aref row 8)))
      (should (ardentia-test--has-face (substring title 0 4) 'ardentia-priority))
      (should-not (ardentia-test--any-face (substring title 4))))
    (let ((row (ardentia-test--colored
                (ardentia-test--task :queue 'ready) now)))
      (should-not (ardentia-test--any-face (aref row 8))))))

(ert-deftest ardentia-test-colors-flags-att-meta ()
  (let ((now (ardentia-test--t "20261011-1200"))
        (ardentia-dashboard-colors t))
    (let* ((row (ardentia-test--colored
                 (ardentia-test--task :queue 'review :trouble "auto_error"
                                      :props '(("AI_ATTEMPTS" . "3")
                                               ("AI_LIMIT_INTERRUPTED" . "t")))
                 now 'required))
           (flags (aref row 5)))
      (should (equal (substring-no-properties flags)
                     "auto_error:review limit you!"))
      (should (ardentia-test--has-face (substring flags 0 17)
                                       'ardentia-flag-trouble))
      (should (ardentia-test--has-face (substring flags 18 23)
                                       'ardentia-flag-attention))
      (should (ardentia-test--has-face (substring flags 24)
                                       'ardentia-flag-attention))
      (should-not (ardentia-test--any-face (substring flags 17 18)))
      (should (equal (substring-no-properties (aref row 3)) "3!"))
      (should (ardentia-test--has-face (aref row 3) 'ardentia-flag-attention))
      (should (ardentia-test--has-face (aref row 2) 'ardentia-dashboard-meta))
      (should (ardentia-test--has-face (aref row 7) 'ardentia-dashboard-meta)))
    (let ((row (ardentia-test--colored
                (ardentia-test--task :queue 'review) now 'optional)))
      (should (ardentia-test--has-face (aref row 5) 'ardentia-flag-info)))
    (let* ((row (ardentia-test--colored
                 (ardentia-test--task
                  :queue 'ready :scheduled "<2026-10-11 Sun 15:05>")
                 now))
           (flags (aref row 5)))
      (should (string-prefix-p "due " (substring-no-properties flags)))
      (should (ardentia-test--has-face flags 'ardentia-flag-info)))
    (progn
      (let ((s (ardentia-dashboard--colorize-flags "due in 3h05m" nil)))
        (should (ardentia-test--has-face s 'ardentia-flag-info)))
      (let ((s (ardentia-dashboard--colorize-flags "stale?" nil)))
        (should (ardentia-test--has-face s 'ardentia-flag-attention)))
      (let ((s (ardentia-dashboard--colorize-flags "merging" t)))
        (should (ardentia-test--has-face s 'ardentia-flag-trouble))))))

(ert-deftest ardentia-test-colors-off-and-no-mutation ()
  (let* ((now (ardentia-test--t "20261011-1200"))
         (title (copy-sequence "Title"))
         (task (ardentia-test--task :queue 'review :priority "A" :title title
                                    :trouble "error"
                                    :props '(("AI_STATUS" . "blocked")
                                             ("AI_ATTEMPTS" . "4"))))
         (plain (ardentia--task-row task now 'required)))
    (let ((ardentia-dashboard-colors t))
      (let ((row (ardentia-dashboard--colorize-row task plain now)))
        (should (equal row plain))
        (should (ardentia-test--any-face (aref row 0)))
        (should-not (ardentia-test--any-face title))
        (should-not (cl-some #'ardentia-test--any-face plain))))
    (let ((ardentia-dashboard-colors nil))
      (let ((row (ardentia-dashboard--colorize-row task plain now)))
        (should (equal row plain))
        (should-not (cl-some #'ardentia-test--any-face row))))))

(ert-deftest ardentia-test-colors-header-error ()
  (let* ((ardentia-usage-enabled nil)
         (st (list :host "desktop" :error "boom 100%" :refreshing t)))
    (let* ((ardentia-dashboard-colors t)
           (segs (ardentia-dashboard--info-segments st))
           (err (seq-find (lambda (x) (string-match-p "ERROR" x)) segs))
           (ref (seq-find (lambda (x) (string-match-p "refreshing" x)) segs)))
      (should (ardentia-test--has-face err 'ardentia-flag-trouble))
      (should (ardentia-test--has-face ref 'ardentia-flag-info))
      (should (equal (substring-no-properties err) " | ERROR: boom 100%"))
      (let ((esc (ardentia-dashboard--escape-percent err)))
        (should (equal (substring-no-properties esc) " | ERROR: boom 100%%"))
        (should (memq 'ardentia-flag-trouble
                      (ardentia-test--faces-at esc (1- (length esc)))))))
    (let* ((ardentia-dashboard-colors nil)
           (segs (ardentia-dashboard--info-segments st)))
      (should-not (cl-some #'ardentia-test--any-face segs)))))

(ert-deftest ardentia-test-colors-entries ()
  (let ((now (ardentia-test--t "20261011-1200"))
        (ardentia-dashboard-colors t))
    (let* ((task (ardentia-test--task :queue 'running))
           (entries (ardentia-dashboard--entries (list task) nil now)))
      (should (ardentia-test--has-face (aref (cadar entries) 0)
                                       'ardentia-queue-running))
      (should (equal (cadar entries) (ardentia--task-row task now))))))

;;; ardentia-test.el ends here
