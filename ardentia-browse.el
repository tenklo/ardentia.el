;;; ardentia-browse.el --- Read-only knowledge and agent browsers -*- lexical-binding: t -*-

;; Copyright (C) 2026 Karsten Klöss

;; Author: Karsten Klöss
;; Assisted-by: Claude Code:Claude Opus 5.5, Claude Sonnet 5.5
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

;; Two read-only browsers.  `ardentia-knowledge' lists the knowledge
;; files of the agent team; files above `ardentia-knowledge-large-file-size'
;; are never opened whole: RET greps them instead.  `ardentia-agents'
;; lists the agent definitions with the settings from their YAML
;; frontmatter.  Nothing here writes to any file.  Only the head of each
;; file is read to build the lists.  The functions in the "Pure parts"
;; sections have no side effects besides reading, so they can be tested.

;;; Code:

(require 'tabulated-list)
(require 'seq)
(require 'subr-x)
(require 'grep)
(require 'ardentia-core)

(declare-function ardentia "ardentia" ())

(defcustom ardentia-knowledge-directory
  (expand-file-name "ai/knowledge/" ardentia-org-directory)
  "Directory with the knowledge files shown by `ardentia-knowledge'."
  :type 'directory
  :group 'ardentia)

(defcustom ardentia-knowledge-large-file-size 100000
  "Size in bytes above which a knowledge file is only grepped, not visited."
  :type 'integer
  :group 'ardentia)

(defcustom ardentia-agents-directory
  (expand-file-name ".claude/agents/" ardentia-org-directory)
  "Directory with the agent definition files shown by `ardentia-agents'."
  :type 'directory
  :group 'ardentia)

(defface ardentia-knowledge-large
  '((t :inherit warning))
  "Face for knowledge files that are only grepped, never visited whole."
  :group 'ardentia)

;;;; Pure parts

(defconst ardentia-browse--title-bytes 4096
  "Number of bytes read from a knowledge file to find its title.")

(defconst ardentia-browse--frontmatter-bytes 8192
  "Number of bytes read from an agent file to find its frontmatter.")

(defun ardentia-browse--head (file bytes)
  "Return the first BYTES bytes of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file nil 0 bytes)
    (buffer-string)))

(defun ardentia-browse--regular-files (dir predicate)
  "Return the names of the regular files in DIR that satisfy PREDICATE.
PREDICATE gets the bare file name.  Dotfiles, backup and lock files are
skipped.  Signal a `user-error' when DIR does not exist."
  (unless (file-directory-p dir)
    (user-error "Directory does not exist: %s" dir))
  (seq-filter
   (lambda (name)
     (and (not (string-match-p "\\`[.#]\\|~\\'\\|#\\'" name))
          (funcall predicate name)
          (file-regular-p (expand-file-name name dir))))
   (directory-files dir nil nil t)))

(defun ardentia-browse--frontmatter (text)
  "Return the simple YAML frontmatter of TEXT as an alist of strings.
The text must start with a line \"---\" and the frontmatter ends at the
next \"---\" line.  Only `key: value' lines count; the value is
everything after the first colon.  Return nil without frontmatter."
  (let ((lines (split-string text "\n"))
        (result nil))
    (when (equal (string-trim-right (or (car lines) "")) "---")
      (let ((rest (cdr lines)) (closed nil))
        (while (and rest (not closed))
          (let ((line (string-trim-right (car rest) "\r")))
            (cond
             ((equal (string-trim-right line) "---") (setq closed t))
             ((string-match
               "\\`\\([A-Za-z0-9_-]+\\):[ \t]*\\(.*?\\)[ \t]*\\'" line)
              (push (cons (match-string 1 line) (match-string 2 line))
                    result))))
          (setq rest (cdr rest)))
        (unless closed (setq result nil))))
    (nreverse result)))

(defun ardentia-browse--format-time (time)
  "Return TIME as \"YYYY-MM-DD HH:MM\"."
  (format-time-string "%Y-%m-%d %H:%M" time))

(defun ardentia-browse--dash (value)
  "Return VALUE, or \"-\" when it is nil or empty."
  (if (or (null value) (string-empty-p value)) "-" value))

;;;; Knowledge

(defun ardentia-knowledge--title (file)
  "Return the first markdown heading of FILE, or \"\".
Only the first `ardentia-browse--title-bytes' bytes are read."
  (let ((head (ardentia-browse--head file ardentia-browse--title-bytes)))
    (if (string-match "^# +\\(.*?\\)[ \t]*$" head)
        (match-string 1 head)
      "")))

(defun ardentia-knowledge--large-p (file)
  "Return non-nil when FILE is above `ardentia-knowledge-large-file-size'."
  (> (file-attribute-size (file-attributes file))
     ardentia-knowledge-large-file-size))

(defun ardentia-knowledge--row (file)
  "Return the row vector for the knowledge FILE.
Columns: name, size, modified, note, title."
  (let* ((attrs (file-attributes file))
         (large (ardentia-knowledge--large-p file))
         (name (file-name-nondirectory file)))
    (vector (if large (propertize name 'face 'ardentia-knowledge-large) name)
            (file-size-human-readable (file-attribute-size attrs))
            (ardentia-browse--format-time
             (file-attribute-modification-time attrs))
            (if large (propertize "grep only" 'face 'ardentia-knowledge-large)
              "")
            (ardentia-knowledge--title file))))

(defun ardentia-knowledge--entries ()
  "Return `tabulated-list-entries' for `ardentia-knowledge-directory'."
  (let ((dir ardentia-knowledge-directory))
    (mapcar (lambda (name)
              (let ((file (expand-file-name name dir)))
                (list file (ardentia-knowledge--row file))))
            (sort (ardentia-browse--regular-files dir #'always)
                  #'string<))))

;;;; Knowledge mode

(defun ardentia-knowledge--grep (regexp files)
  "Grep REGEXP in FILES asynchronously, results in a `grep-mode' buffer.
FILES are absolute names; grep runs in `ardentia-knowledge-directory'."
  (let ((default-directory (file-name-as-directory
                            (expand-file-name ardentia-knowledge-directory))))
    (compilation-start
     (concat "grep -nHI -e " (shell-quote-argument regexp) " -- "
             (mapconcat (lambda (f)
                          (shell-quote-argument (file-relative-name f)))
                        files " "))
     'grep-mode)))

(defun ardentia-knowledge--file-at-point ()
  "Return the file name of the row at point or signal a `user-error'."
  (or (tabulated-list-get-id) (user-error "No file here")))

(defun ardentia-knowledge-open ()
  "View the file at point; grep a large file instead of visiting it."
  (interactive)
  (let ((file (ardentia-knowledge--file-at-point)))
    (if (ardentia-knowledge--large-p file)
        (ardentia-knowledge--grep
         (read-regexp (format "Grep %s for" (file-name-nondirectory file)))
         (list file))
      (view-file file))))

(defun ardentia-knowledge-search ()
  "Grep a regexp over all knowledge files."
  (interactive)
  (let ((files (mapcar #'car (ardentia-knowledge--entries))))
    (unless files (user-error "No knowledge files"))
    (ardentia-knowledge--grep (read-regexp "Grep knowledge files for") files)))

(defun ardentia-knowledge-visit ()
  "Visit the file at point; ask first when it is large."
  (interactive)
  (let ((file (ardentia-knowledge--file-at-point)))
    (when (or (not (ardentia-knowledge--large-p file))
              (yes-or-no-p (format "%s is large; visit it anyway? "
                                   (file-name-nondirectory file))))
      (find-file file))))

(defun ardentia-knowledge-refresh ()
  "Re-read the knowledge directory."
  (interactive)
  (setq tabulated-list-entries (ardentia-knowledge--entries))
  (tabulated-list-print t))

(ardentia-define-keys-and-menu
    ardentia-knowledge-mode-map ardentia-knowledge-menu
  :keymap-doc "Keymap for `ardentia-knowledge-mode'."
  :menu-doc "Keys of the ardentia knowledge browser."
  :groups
  (("File at point"
    ("RET" "Open" ardentia-knowledge-open)
    ("o" "Visit" ardentia-knowledge-visit))
   ("Search"
    ("s" "Grep all files" ardentia-knowledge-search))
   ("View"
    ("g" "Refresh" ardentia-knowledge-refresh)
    ("q" "Quit" quit-window))))

(define-derived-mode ardentia-knowledge-mode tabulated-list-mode
  "Ardentia-Knowledge"
  "Major mode listing the knowledge files; large ones are only grepped.

\\{ardentia-knowledge-mode-map}"
  (setq tabulated-list-format
        (vector '("Name" 32 t)
                (list "Size" 8
                      (lambda (a b)
                        (< (file-attribute-size (file-attributes (car a)))
                           (file-attribute-size (file-attributes (car b)))))
                      :right-align t)
                '("Modified" 16 t) '("Note" 9 t) '("Title" 0 t)))
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key '("Name"))
  (setq header-line-format nil)
  (tabulated-list-init-header))

;;;###autoload
(defun ardentia-knowledge ()
  "Browse the knowledge files read-only; large files are grepped, not opened."
  (interactive)
  (unless (file-directory-p ardentia-knowledge-directory)
    (user-error "Directory does not exist: %s" ardentia-knowledge-directory))
  (let ((buf (get-buffer-create "*ardentia knowledge*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'ardentia-knowledge-mode)
        (ardentia-knowledge-mode))
      (ardentia-knowledge-refresh))
    (pop-to-buffer-same-window buf)))

;;;; Agents

(defun ardentia-agents--row (file)
  "Return the row vector for the agent FILE.
Columns: name, model, tools, permission, turns, omitClaudeMd, description.
A file without frontmatter shows its file name and \"-\" elsewhere."
  (let* ((fm (ardentia-browse--frontmatter
              (ardentia-browse--head file ardentia-browse--frontmatter-bytes)))
         (get (lambda (key) (ardentia-browse--dash (cdr (assoc key fm))))))
    (vector (let ((name (cdr (assoc "name" fm))))
              (if (and name (not (string-empty-p name)))
                  name
                (file-name-sans-extension (file-name-nondirectory file))))
            (funcall get "model") (funcall get "tools")
            (funcall get "permissionMode") (funcall get "maxTurns")
            (funcall get "omitClaudeMd") (funcall get "description"))))

(defun ardentia-agents--entries ()
  "Return `tabulated-list-entries' for `ardentia-agents-directory'."
  (let ((dir ardentia-agents-directory))
    (mapcar (lambda (name)
              (let ((file (expand-file-name name dir)))
                (list file (ardentia-agents--row file))))
            (sort (ardentia-browse--regular-files
                   dir (lambda (n) (string-suffix-p ".md" n)))
                  #'string<))))

(defun ardentia-agents-open ()
  "View the agent file at point read-only."
  (interactive)
  (view-file (or (tabulated-list-get-id) (user-error "No agent here"))))

(defun ardentia-agents-refresh ()
  "Re-read the agents directory."
  (interactive)
  (setq tabulated-list-entries (ardentia-agents--entries))
  (tabulated-list-print t))

(ardentia-define-keys-and-menu
    ardentia-agents-mode-map ardentia-agents-menu
  :keymap-doc "Keymap for `ardentia-agents-mode'."
  :menu-doc "Keys of the ardentia agents browser."
  :groups
  (("Agent at point"
    ("RET" "Open" ardentia-agents-open))
   ("View"
    ("g" "Refresh" ardentia-agents-refresh)
    ("q" "Quit" quit-window))))

(define-derived-mode ardentia-agents-mode tabulated-list-mode
  "Ardentia-Agents"
  "Major mode listing the agent definitions from their frontmatter.
The OmitMd column shows the omitClaudeMd setting.

\\{ardentia-agents-mode-map}"
  (setq tabulated-list-format
        (vector '("Name" 16 t) '("Model" 8 t) '("Tools" 24 t)
                '("Permission" 12 t) '("Turns" 5 t) '("OmitMd" 6 t)
                '("Description" 0 t)))
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key '("Name"))
  (tabulated-list-init-header))

;;;###autoload
(defun ardentia-agents ()
  "Browse the agent definitions read-only."
  (interactive)
  (unless (file-directory-p ardentia-agents-directory)
    (user-error "Directory does not exist: %s" ardentia-agents-directory))
  (let ((buf (get-buffer-create "*ardentia agents*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'ardentia-agents-mode)
        (ardentia-agents-mode))
      (ardentia-agents-refresh))
    (pop-to-buffer-same-window buf)))

(provide 'ardentia-browse)
;;; ardentia-browse.el ends here
