;;; ardentia-answer.el --- Batch answers to review items for ardentia -*- lexical-binding: t -*-

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

;; Answer several review items with one string.  The numbers N are the
;; row numbers shown in the review list.  Items are separated by newline,
;; `;' or `,'; an item is
;;
;;   N L  or  NL          decide option L of the card (case-insensitive)
;;   N L: TEXT / NL TEXT  decide option L, TEXT is added to the note
;;   N ok  /  N done      accept: the task is set DONE (open children first)
;;   N skip               nothing, the item stays
;;   N: TEXT  /  N TEXT   send back with the note TEXT
;;
;; A task with several open Decide children takes `N L' for the first
;; open one; `N.2 L' addresses the second (1-based, file order).  TEXT
;; cannot contain `,' or `;'.
;;
;; The work is split in three pure steps, each tested on its own:
;; `ardentia-answer-parse' (string to actions, grammar errors),
;; `ardentia-answer-resolve' (actions plus the review data of the rows to
;; steps; unknown rows and letters are errors) and
;; `ardentia-answer-summary' (what will happen, one line per item).
;; `ardentia-answer-execute' shows the summary, asks once, then does it
;; in the visiting buffers, one save per file:
;;
;; - decide: a note "Decision on <Decide title>: L -- <option>[; TEXT]"
;;   on the task, the Decide child set DONE, humanreview replaced by
;;   aiready on the task (not while it is :ai_in_progress:; other open
;;   Decide children do not keep the task back)
;; - accept: the open child TODOs, then the task, set DONE
;; - back: the note on the task, humanreview replaced by aiready as above
;; - skip: nothing
;;
;; The commands are `ardentia-dashboard-answer-several' (the task list)
;; and `ardentia-review-decide' (one review buffer).

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ardentia-core)
(require 'ardentia-actions)

(declare-function ardentia-review--collect "ardentia-review" (file id))

;;;; Grammar (pure)

(defun ardentia-answer--split (string)
  "Return the non-blank, trimmed items of STRING."
  (seq-remove #'string-empty-p
              (mapcar #'string-trim (split-string string "[\n;,]" t))))

(defun ardentia-answer-parse-item (item)
  "Parse the answer ITEM into a plist (:n :sub :kind :letter :text).
:kind is `decide', `accept', `skip' or `back'.  :sub is the 1-based
number of the Decide child, or nil.  Signal a `user-error' for an item
that does not start with a row number or says nothing."
  (let ((case-fold-search t))
    (unless (string-match
             "\\`\\([0-9]+\\)\\(?:\\.\\([0-9]+\\)\\)?[ \t]*\\(.*\\)\\'" item)
      (user-error "Cannot read \"%s\": an item starts with the row number" item))
    (let* ((n (string-to-number (match-string 1 item)))
           (sub (and (match-string 2 item) (string-to-number (match-string 2 item))))
           (rest (match-string 3 item))
           kind letter text)
      (cond
       ((string-empty-p rest)
        (user-error "Row %d: say what to do (a letter, ok, skip or a note)" n))
       ((string-match-p "\\`\\(?:ok\\|done\\)\\'" rest) (setq kind 'accept))
       ((string-match-p "\\`skip\\'" rest) (setq kind 'skip))
       ((string-match "\\`:[ \t]*\\(.*\\)\\'" rest)
        (setq kind 'back text (match-string 1 rest)))
       ((string-match "\\`\\([A-Za-z]\\)\\(?:\\'\\|[ \t]*:[ \t]*\\(.*\\)\\|[ \t]+\\(.*\\)\\)\\'"
                      rest)
        (setq kind 'decide
              letter (upcase (match-string 1 rest))
              text (string-trim (or (match-string 2 rest) (match-string 3 rest) ""))))
       (t (setq kind 'back text rest)))
      (when (and (eq kind 'back) (string-blank-p text))
        (user-error "Row %d: a note for the agents is required" n))
      (when (and sub (not (eq kind 'decide)))
        (user-error "Row %d.%d: a Decide child is only addressed with a letter"
                    n sub))
      (list :n n :sub sub :kind kind :letter letter :text text))))

(defun ardentia-answer-parse (string)
  "Parse the batch answer STRING into a list of actions, in order.
See `ardentia-answer-parse-item'.  Signal a `user-error' for an empty
STRING or an item that cannot be read."
  (let ((items (ardentia-answer--split (or string ""))))
    (unless items (user-error "Nothing to answer"))
    (mapcar #'ardentia-answer-parse-item items)))

;;;; Resolving against the review data (pure)

(defun ardentia-answer--range (rows)
  "Return text naming the row numbers of ROWS."
  (let ((ns (sort (mapcar #'car rows) #'<)))
    (if ns (format "rows %d-%d" (car ns) (car (last ns))) "the list has no rows")))

(defun ardentia-answer-decision-note (decide-title option text)
  "Return the note for the choice OPTION of the Decide child DECIDE-TITLE.
TEXT is the human's own text, or empty."
  (format "Decision on %s: %s -- %s%s" decide-title
          (plist-get option :letter) (plist-get option :text)
          (if (string-empty-p (or text "")) "" (concat "; " text))))

(defun ardentia-answer-resolve (actions rows)
  "Check ACTIONS against ROWS and return the steps to carry out.
ROWS is an alist (N . (:task TASK :info INFO)); INFO is the review data
of the task (see `ardentia-review--collect'), nil when unreadable.  Each
step is its action plus :task and, for a decision, :decide (the child
plist), :option and :note.  Signal a `user-error' for an unknown row, a
letter that is not an option of the card, a missing or incomplete card,
or a row answered twice."
  (let (seen steps)
    (dolist (a actions)
      (let* ((n (plist-get a :n))
             (row (cdr (assq n rows)))
             (kind (plist-get a :kind))
             (prior (cdr (assq n seen))))
        (unless row
          (user-error "No row %d in the list (%s)" n (ardentia-answer--range rows)))
        (when (or (and prior (not (eq kind 'decide)))
                  (seq-some (lambda (p) (not (eq (plist-get p :kind) 'decide)))
                            prior)
                  (seq-some (lambda (p)
                              (equal (or (plist-get p :sub) 1)
                                     (or (plist-get a :sub) 1)))
                            prior))
          (user-error "Row %d is answered twice" n))
        (push a (alist-get n seen))
        (let ((step (append a (list :task (plist-get row :task)))))
          (when (eq kind 'decide)
            (let* ((info (plist-get row :info))
                   (decides (plist-get info :decides))
                   (sub (or (plist-get a :sub) 1))
                   (child (nth (1- sub) decides))
                   (options (plist-get (plist-get child :card) :options))
                   (letter (plist-get a :letter))
                   (option (seq-find (lambda (o) (equal (plist-get o :letter) letter))
                                     options)))
              (cond
               ((null info)
                (user-error "Row %d: cannot read the review data of the task" n))
               ((null decides)
                (user-error "Row %d has no open Decide child; use ok, skip or a note" n))
               ((null child)
                (user-error "Row %d has %d open Decide children, no .%d"
                            n (length decides) sub))
               ((null options)
                (user-error "Row %d: card incomplete -- see the Decide child" n))
               ((null option)
                (user-error "Row %d: no option %s (options: %s)" n letter
                            (mapconcat (lambda (o) (plist-get o :letter))
                                       options ", "))))
              (setq step (append step
                                 (list :decide child :option option
                                       :note (ardentia-answer-decision-note
                                              (plist-get child :title) option
                                              (plist-get a :text)))))))
          (push step steps))))
    (nreverse steps)))

(defun ardentia-answer--clip (text width)
  "Return TEXT cut to WIDTH characters."
  (if (> (length text) width) (concat (substring text 0 (- width 3)) "...") text))

(defun ardentia-answer-summary (steps)
  "Return what carrying out STEPS does, one line per item."
  (concat
   (mapconcat
    (lambda (s)
      (let ((title (ardentia-answer--clip
                    (or (plist-get (plist-get s :task) :title) "?") 44))
            (n (if (plist-get s :sub)
                   (format "%d.%d" (plist-get s :n) (plist-get s :sub))
                 (format "%d" (plist-get s :n)))))
        (pcase (plist-get s :kind)
          ('decide (format "%-4s %s\n     decide %s: %s%s" n title
                           (plist-get s :letter)
                           (ardentia-answer--clip
                            (plist-get (plist-get s :option) :text) 60)
                           (if (string-empty-p (plist-get s :text)) ""
                             (format " (note: %s)" (plist-get s :text)))))
          ('accept (format "%-4s %s\n     accept: open children, then the task, DONE"
                           n title))
          ('back (format "%-4s %s\n     send back: %s" n title
                         (ardentia-answer--clip (plist-get s :text) 60)))
          (_ (format "%-4s %s\n     skip: stays" n title)))))
    steps "\n")
   "\n\nDecide children are set DONE; tasks go back to the agents"
   " (not while running).\n"))

;;;; Carrying out

(defun ardentia-answer--find-open-child (id title)
  "Return a marker at the open TODO child of the heading at point, or nil.
The child is the one with ID, else the one titled TITLE."
  (save-excursion
    (let ((more (org-goto-first-child)) found)
      (while (and more (not found))
        (let ((todo (org-get-todo-state)))
          (if (and todo (not (member todo org-done-keywords))
                   (if id
                       (equal (org-entry-get nil "ID") id)
                     (equal (org-get-heading t t t t) title)))
              (setq found (point-marker))
            (setq more (org-get-next-sibling)))))
      found)))

(defun ardentia-answer--open-children ()
  "Return markers at the open TODO children of the heading at point."
  (save-excursion
    (let ((more (org-goto-first-child)) out)
      (while more
        (let ((todo (org-get-todo-state)))
          (when (and todo (not (member todo org-done-keywords)))
            (push (point-marker) out)))
        (setq more (org-get-next-sibling)))
      (nreverse out))))

(defun ardentia-answer--apply (steps)
  "Carry out STEPS, those of one task, at the task heading at point.
The children are looked up before anything is written."
  (let* ((task (point-marker))
         (children (mapcar (lambda (s)
                             (when (eq (plist-get s :kind) 'decide)
                               (let ((d (plist-get s :decide)))
                                 (or (ardentia-answer--find-open-child
                                      (plist-get d :id) (plist-get d :title))
                                     (user-error "%s" ardentia--changed-message)))))
                           steps))
         back)
    (cl-loop for s in steps for child in children do
             (goto-char task)
             (pcase (plist-get s :kind)
               ('decide
                (ardentia--insert-log-note (plist-get s :note))
                (goto-char child)
                (ardentia--set-done)
                (setq back t))
               ('back
                (ardentia--insert-log-note (plist-get s :text))
                (setq back t))
               ('accept
                (let ((open (ardentia-answer--open-children)))
                  (dolist (m open)
                    (goto-char m)
                    (ardentia--set-done)))
                (goto-char task)
                (ardentia--set-done))))
    (when back
      (goto-char task)
      (unless (member "ai_in_progress" (ardentia--direct-tags))
        (ardentia--swap-tag "humanreview" "aiready")))
    (set-marker task nil)))

(defun ardentia-answer--tasks (steps)
  "Return the tasks of STEPS that need work, each with its :answer-steps."
  (let (keys tasks)
    (dolist (s steps)
      (unless (eq (plist-get s :kind) 'skip)
        (let* ((task (plist-get s :task)) (key (ardentia--task-key task)))
          (unless (member key keys) (push key keys) (push task tasks)))))
    (mapcar (lambda (task)
              (let ((key (ardentia--task-key task)))
                (append task
                        (list :answer-steps
                              (seq-filter
                               (lambda (s)
                                 (and (not (eq (plist-get s :kind) 'skip))
                                      (equal (ardentia--task-key (plist-get s :task))
                                             key)))
                               steps)))))
            (nreverse tasks))))

(defun ardentia-answer--confirm (steps)
  "Show the summary of STEPS and ask once; return non-nil to go on."
  (let ((buf (get-buffer-create "*Ardentia answers*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (ardentia-answer-summary steps)))
      (goto-char (point-min))
      (special-mode))
    (save-window-excursion
      (display-buffer buf)
      (unwind-protect
          (y-or-n-p (format "Carry out %d item%s? " (length steps)
                            (if (cdr steps) "s" "")))
        (kill-buffer buf)))))

(defun ardentia-answer--count (steps kind)
  "Return how many of STEPS have KIND."
  (seq-count (lambda (s) (eq (plist-get s :kind) kind)) steps))

(defun ardentia-answer-execute (steps)
  "Show STEPS, ask once and carry them out; return the result plist or nil.
The result has :done, :skipped and :reason (see `ardentia-bulk-edit').
Nil means the human declined.  Echo a one-line result."
  (unless steps (user-error "Nothing to do"))
  (when (ardentia-answer--confirm steps)
    (let* ((tasks (ardentia-answer--tasks steps))
           (result (if tasks
                       (ardentia--without-logging
                         (ardentia-bulk-edit
                          tasks
                          (lambda (task)
                            (ardentia-answer--apply
                             (plist-get task :answer-steps)))))
                     (list :done 0 :skipped 0)))
           (skipped (ardentia-answer--count steps 'skip)))
      (message "Answers: %d task%s done (%d decided, %d accepted, %d sent back); %d left as is%s"
               (plist-get result :done) (if (= (plist-get result :done) 1) "" "s")
               (ardentia-answer--count steps 'decide)
               (ardentia-answer--count steps 'accept)
               (ardentia-answer--count steps 'back)
               skipped
               (if (> (plist-get result :skipped) 0)
                   (format "; %d FAILED (%s)" (plist-get result :skipped)
                           (plist-get result :reason))
                 ""))
      result)))

;;;; From the task list

(defun ardentia-answer-rows (actions numbered)
  "Return the ROWS alist for ACTIONS from NUMBERED, an alist (N . TASK).
Every numbered row is in it; the review data is read from the task's
buffer only for the rows that a decision names."
  (mapcar
   (lambda (cell)
     (let ((n (car cell)) (task (cdr cell)))
       (list n :task task
             :info (and (seq-some (lambda (a)
                                    (and (eq (plist-get a :n) n)
                                         (eq (plist-get a :kind) 'decide)))
                                  actions)
                        (condition-case nil
                            (ardentia-review--collect
                             (plist-get task :file) (plist-get task :id))
                          (error nil))))))
   numbered))

(defun ardentia-answer-run (numbered string)
  "Answer the rows of NUMBERED, an alist (N . TASK), with the batch STRING.
Parse, check, show the summary, ask once, carry out.  Return the result
of `ardentia-answer-execute'."
  (let ((actions (ardentia-answer-parse string)))
    (ardentia-answer-execute
     (ardentia-answer-resolve actions (ardentia-answer-rows actions numbered)))))

(provide 'ardentia-answer)
;;; ardentia-answer.el ends here
