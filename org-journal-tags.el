;;; org-journal-tags.el --- TODO -*- lexical-binding: t -*-

;; Copyright (C) 2022 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org-journal "2.1.2"))
;; Homepage: https://github.com/SqrtMinusOne/org-journal-tags.el

;; This file is NOT part of GNU Emacs.

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

;; TODO

;;; Code:
(require 'org-journal)
(require 'org-macs)
(require 'cl-lib)

(defgroup org-journal-tags ()
  "Manage tags for org-journal"
  :group 'org-journal)

(defcustom org-journal-tags-db-file
  (if (boundp 'no-littering-var-directory)
      (concat no-littering-var-directory "org-journal-tags/index")
    (concat user-emacs-directory "org-journal-tags/index"))
  "A location of the org-journal-tags database."
  :group 'org-journal-tags
  :type 'file)

(defvar org-journal-tags-db nil
  "The core org-journal-tags database.")

(defface org-journal-tags-tag-face
  '((t (:inherit warning)))
  "A default face for org-journal tags."
  :group 'org-journal-tags)

(defcustom org-journal-tags-face-function #'org-journal-tags--face-default
  "A function to get the face of a tag.

The only argument is the tag string."
  :group 'org-journal-tags
  :type 'function)

(defun org-journal-tags--face-default (tag)
  "A function to return the default tag face for TAG."
  'org-journal-tags-tag-face)

(cl-defstruct (org-journal-tag (:constructor org-journal-tag--create))
  name references)

(defun org-journal-tags-db--empty ()
  "Create an empty org-journal-tags database."
  (make-hash-table :test #'equal))

(defun org-journal-tags-db-load ()
  "Load the org-journal-tags database from the filesystem."
  (if (not (file-exists-p org-journal-tags-db-file))
      (setf org-journal-tags-db (org-journal-tags-db--empty))
    (with-temp-buffer
      (insert-file-contents org-journal-tags-db-file)
      (goto-char (point-min))
      (setf org-journal-tags-db (read (current-buffer))))))

(defun org-journal-tags-db-ensure ()
  "Ensure that the database has been loaded."
  (when (null org-journal-tags-db) (org-journal-tags-db-load)))

(defun org-journal-tags-db-save ()
  (org-journal-tags-db-ensure)
  (mkdir (file-name-directory org-journal-tags-db-file) t)
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file org-journal-tags-db-file
      (let ((standard-output (current-buffer))
            (print-level nil)
            (print-length nil)
            (print-circle nil))
        (princ ";;; Org Journal Tags Database\n\n")
        (prin1 org-journal-tags-db)))))

(defun org-journal-tags-db-unload ()
  "Unload the org-journal-tags database"
  (interactive)
  (org-journal-tags-db-save)
  (setf org-journal-tags-db nil))

(defun org-journal-tags--follow (&rest rest)
  "TODO. Eventually this fill do something."
  (message (prin1-to-string rest)))

(org-link-set-parameters
 "org-journal"
 :follow #'org-journal-tags--follow
 :face (lambda (&rest args) (funcall org-journal-tags-face-function args)))

(defun org-journal-tags--extract-links ()
  "Extract org-journal links from the current org buffer."
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) "org-journal")
        link))))

(provide 'org-journal-tags)
;;; org-journal-tags.el ends here
