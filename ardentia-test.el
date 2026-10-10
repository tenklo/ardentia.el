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
  (let ((now (ardentia-test--t "20261010-1200")))
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
    (should (equal row ["review" "partial" "default" "1" "12m" "" "a.org" "Hi"]))))

(ert-deftest ardentia-test-dashboard-entries-filter ()
  (let* ((tasks (list (ardentia-test--task :queue 'ready)
                      (ardentia-test--task :queue 'review)))
         (now (current-time)))
    (should (= (length (ardentia-dashboard--entries tasks nil now)) 2))
    (should (= (length (ardentia-dashboard--entries tasks 'review now)) 1))))

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
      (should (string-match-p "\\* Task one :refile:humanreview:" text))
      (should (string-match-p "Original task text" text)))))

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

(ert-deftest ardentia-test-mode-line-tick-never-errors ()
  (let ((ardentia--mode-line-busy nil))
    (cl-letf (((symbol-function 'ardentia--mode-line-refresh)
               (lambda () (error "Boom"))))
      (ardentia--mode-line-tick)
      (should-not ardentia--mode-line-busy))))

;;;; Async fetch with a fake org-tool

(defun ardentia-test--fake-env (dir)
  "Create the fake org directory with tools in DIR."
  (make-directory (expand-file-name "ai/tools" dir) t)
  (dolist (f '("a.org" "b.org" "c.sync-conflict-20261010-ABC.org" "notes.txt"))
    (write-region "" nil (expand-file-name f dir)))
  (let ((tool (expand-file-name "ai/tools/org-tool" dir))
        (log (expand-file-name "args.log" dir)))
    (with-temp-file tool
      (insert "#!/bin/sh\n"
              "echo \"$*\" >> " log "\n"
              "case \"$2\" in\n"
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
          (should (equal (mapcar (lambda (x) (plist-get x :queue)) got-tasks)
                         '(ready review)))
          (let ((log (with-temp-buffer
                       (insert-file-contents (expand-file-name "args.log" dir))
                       (buffer-string))))
            (should (string-match-p "find-tagged humanreview .*a\\.org" log))
            (should (string-match-p "find-tagged aiready" log))
            (should (string-match-p "find-tagged ai_in_progress" log))
            (should-not (string-match-p "sync-conflict" log))
            (should-not (string-match-p "notes\\.txt" log))))
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
                             (viewer "u") (session "u") (shell "u")
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

(provide 'ardentia-test)
;;; ardentia-test.el ends here
