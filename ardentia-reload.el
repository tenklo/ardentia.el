;;; ardentia-reload.el --- Reload all ardentia modules in place -*- lexical-binding: t -*-

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

;; `ardentia-reload' loads every ardentia module of a directory into an
;; Emacs that already runs an older ardentia, so the new code takes effect
;; without a restart.  It never prompts, so it can also run unattended
;; after an update of the package.
;;
;; - Every `ardentia*.el' in the directory is a module, except the test
;;   suite and generated autoloads/pkg files; a new module needs no list
;;   edit.  The `.elc' is loaded when it is not older than the `.el'.
;; - The order follows the top-level `require' forms of the files (read,
;;   not evaluated); `ardentia.el' comes last.
;; - Values of variables and options stay (`defvar' and `defcustom' keep a
;;   bound value), so do buffer-local state and running timers, whose
;;   functions are symbols and so run the new definitions.
;; - `defvar-keymap' would keep the old keymap.  The keymap variables of a
;;   module are therefore unbound just before its load, and afterwards the
;;   old keymap object receives the new bindings and is put back.  Buffers
;;   that use the old object see the new keys at once.  When a load fails,
;;   the old keymap is put back unchanged.
;;
;; This file requires no other ardentia module: it can be loaded alone from
;; the new directory and then reload the rest.

;;; Code:

(defconst ardentia-reload--excluded-regexp
  "\\`ardentia-test\\.el\\'\\|-autoloads\\.el\\'\\|-pkg\\.el\\'"
  "File names in the module directory that are no modules.")

(defconst ardentia-reload--wrapper-forms
  '(progn eval-and-compile eval-when-compile)
  "Forms whose body counts as top level when scanning a module.")

(defun ardentia-reload--loaded-directory ()
  "Return the directory the `ardentia' library was loaded from, or nil.
Only a directory that still contains `ardentia.el' counts."
  (let (candidates)
    (dolist (entry load-history)
      (when (and (stringp (car entry))
                 (member '(provide . ardentia) (cdr entry)))
        (push (car entry) candidates)))
    (setq candidates (append (nreverse candidates)
                             (list (ignore-errors (symbol-file 'ardentia 'defun))
                                   (locate-library "ardentia"))))
    (catch 'found
      (dolist (file candidates)
        (when (stringp file)
          (let ((dir (file-name-directory file)))
            (when (and dir (file-exists-p (expand-file-name "ardentia.el" dir)))
              (throw 'found dir)))))
      nil)))

(defun ardentia-reload--module-files (dir)
  "Return the module files of DIR, sorted by name.
These are all `ardentia*.el' files not matched by
`ardentia-reload--excluded-regexp'."
  (let (files)
    (dolist (file (directory-files dir t "\\`ardentia\\(?:-.*\\)?\\.el\\'"))
      (unless (string-match-p ardentia-reload--excluded-regexp
                              (file-name-nondirectory file))
        (push file files)))
    (nreverse files)))

(defun ardentia-reload--top-level-forms (file)
  "Return the top-level forms of FILE, read without evaluation.
The bodies of `ardentia-reload--wrapper-forms' are spliced in."
  (let (forms)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil)))
    (let (flat)
      (while forms
        (let ((form (pop forms)))
          (if (and (consp form) (memq (car form) ardentia-reload--wrapper-forms))
              (setq forms (append (reverse (cdr form)) forms))
            (push form flat))))
      flat)))

(defun ardentia-reload--quoted-symbol (form)
  "Return the symbol of FORM when it is \\='SYMBOL, else nil."
  (and (eq (car-safe form) 'quote) (symbolp (cadr form)) (cadr form)))

(defun ardentia-reload--scan (file)
  "Return a plist describing the module FILE.
:name is the module name, :file the path, :requires the names of the
features required at top level, :vars the variables the file defines
that may hold a keymap."
  (let (requires vars)
    (dolist (form (ardentia-reload--top-level-forms file))
      (when (consp form)
        (let ((head (car form)) (name (car-safe (cdr form))))
          (pcase head
            ('require
             (let ((feature (ardentia-reload--quoted-symbol name)))
               (when feature (push (symbol-name feature) requires))))
            ((or 'defvar 'defvar-keymap 'defvar-local 'ardentia-define-keys-and-menu)
             (when (symbolp name) (push name vars)))
            ((or 'define-derived-mode 'define-minor-mode)
             (when (symbolp name)
               (push (intern (format "%s-map" name)) vars)))))))
    (list :name (file-name-base file) :file file
          :requires (nreverse requires) :vars (nreverse vars))))

(defun ardentia-reload--order (dir)
  "Return the module plists of DIR in load order.
Each module comes after the modules it requires at top level (cycles are
broken where they are met); `ardentia' comes last."
  (let ((table (make-hash-table :test #'equal))
        (names nil) (seen nil) (order nil))
    (dolist (file (ardentia-reload--module-files dir))
      (let ((module (condition-case err (ardentia-reload--scan file)
                      (error (error "ardentia-reload: cannot read %s: %s"
                                    (file-name-base file)
                                    (error-message-string err))))))
        (puthash (plist-get module :name) module table)
        (push (plist-get module :name) names)))
    (letrec ((visit
              (lambda (name)
                (let ((module (gethash name table)))
                  (when (and module (not (member name seen)))
                    (push name seen)
                    (dolist (dep (plist-get module :requires))
                      (unless (equal dep "ardentia") (funcall visit dep)))
                    (push module order))))))
      (dolist (name (sort names #'string<))
        (unless (equal name "ardentia") (funcall visit name)))
      (push "ardentia" seen)
      (when (gethash "ardentia" table) (push (gethash "ardentia" table) order)))
    (nreverse order)))

(defun ardentia-reload--keymap-value-p (sym)
  "Return non-nil when SYM is bound to a keymap object."
  (and (default-boundp sym)
       (let ((value (default-value sym)))
         (and (consp value) (keymapp value)))))

(defun ardentia-reload--assign-keymaps (modules)
  "Return an alist (MODULE-NAME . KEYMAP-VARIABLES) for MODULES.
A variable belongs to the module that defines it in the new files; an
`ardentia' keymap variable no new file defines belongs to the module
that defined it before, as recorded in `load-history'."
  (let ((owner (make-hash-table :test #'eq))
        (names (mapcar (lambda (m) (plist-get m :name)) modules)))
    (dolist (m modules)
      (dolist (var (plist-get m :vars))
        (unless (gethash var owner) (puthash var (plist-get m :name) owner))))
    (mapatoms
     (lambda (sym)
       (when (and (string-prefix-p "ardentia" (symbol-name sym))
                  (not (gethash sym owner))
                  (ardentia-reload--keymap-value-p sym))
         (let* ((file (ignore-errors (symbol-file sym 'defvar)))
                (base (and (stringp file) (file-name-base file))))
           (when (member base names) (puthash sym base owner))))))
    (let (alist)
      (maphash (lambda (var name)
                 (when (ardentia-reload--keymap-value-p var)
                   (push var (alist-get name alist nil nil #'equal))))
               owner)
      alist)))

(defun ardentia-reload--file-to-load (el)
  "Return the file to load for the module source EL.
That is its `.elc' when present and not older than EL, else EL."
  (let ((elc (concat el "c")))
    (if (and (file-exists-p elc) (not (file-newer-than-file-p el elc)))
        elc
      el)))

;;;###autoload
(defun ardentia-reload (&optional dir)
  "Reload all ardentia modules from DIR in place and return a report.
DIR defaults to the directory the `ardentia' library was loaded from.
Every `ardentia*.el' in DIR except the test suite is loaded, its `.elc'
when that is not older, dependencies first and `ardentia.el' last.
Option and variable values, open buffers and timers keep working; the
keymap objects in use get the new bindings.  Never prompts.  On failure
the keymaps of the failing module are restored and an error naming the
module is signaled."
  (interactive)
  (let* ((dir (file-name-as-directory
               (expand-file-name
                (or dir (ardentia-reload--loaded-directory)
                    (user-error "ardentia-reload: cannot find the directory of the loaded ardentia; pass DIR")))))
         (_ (unless (file-exists-p (expand-file-name "ardentia.el" dir))
              (user-error "ardentia-reload: no ardentia.el in %s" dir)))
         (modules (mapcar (lambda (m)
                            (append (list :load (ardentia-reload--file-to-load
                                                 (plist-get m :file)))
                                    m))
                          (ardentia-reload--order dir)))
         (keymaps (ardentia-reload--assign-keymaps modules))
         (load-path (cons (directory-file-name dir) load-path))
         (loaded nil)
         (updated nil)
         ;; The loop calls no function of this file: loading this file
         ;; again in the middle must not change the code that runs.
         (put-back
          (lambda (var old adopt)
            (let ((new (and (default-boundp var) (default-value var))))
              (when (and adopt (consp new) (keymapp new) (not (eq new old)))
                (let ((parent (keymap-parent old)))
                  (setcdr old (cdr (copy-keymap new)))
                  ;; `define-derived-mode' sets the parent when the mode
                  ;; first runs; a fresh map has none yet.
                  (when (and parent (null (keymap-parent old)))
                    (set-keymap-parent old parent)))
                (push var updated))
              (set-default var old)))))
    (dolist (module modules)
      (let* ((name (plist-get module :name))
             (vars (alist-get name keymaps nil nil #'equal))
             (olds (mapcar (lambda (v) (cons v (default-value v))) vars))
             (ok nil)
             (failure nil))
        (unwind-protect
            (condition-case err
                (progn
                  (dolist (old olds) (makunbound (car old)))
                  (load (plist-get module :load) nil t t)
                  (setq ok t))
              (error (setq failure err)))
          (dolist (old olds) (funcall put-back (car old) (cdr old) ok)))
        (when failure
          (error "ardentia-reload: loading %s from %s failed: %s"
                 name dir (error-message-string failure)))
        (push name loaded)))
    (let ((report (format "ardentia-reload: %d modules from %s (%s); keymaps updated: %s"
                          (length loaded) dir
                          (mapconcat #'identity (nreverse loaded) " ")
                          (if updated
                              (mapconcat #'symbol-name (sort updated #'string<) " ")
                            "none"))))
      (message "%s" report)
      report)))

(provide 'ardentia-reload)
;;; ardentia-reload.el ends here
