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
  name dates)

(cl-defstruct (org-journal-tag-reference (:constructor org-journal-tag-reference--create))
  ref-start ref-end time date)

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

(defun org-journal-tags--ensure-decrypted ()
  "Ensure that the current org-journal is decrypted."
  (when org-journal-enable-encryption
    (goto-char (point-min))
    (while (search-forward ":crypt:" nil t)
      (org-decrypt-entry))))

(defun org-journal-tags--extract-links ()
  "Extract tags from the current org-journal buffer.

Returns an alist of the format (tag-name . reference), where reference is `org-journal-tag-reference'.  Tag names in the alist can repeat."
  (org-journal-tags--ensure-decrypted)
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) "org-journal")
        (let ((tag (org-element-property :path link))
              (parent (org-element-property :parent link))
              (elem (org-element-property :parent link))
              (date-re (org-journal--format->regex
                        org-journal-created-property-timestamp-format))
              time
              date)
          (cl-loop while elem do (setq elem (org-element-property :parent elem))
                   when (and (eq (org-element-type elem) 'headline)
                             (= (org-element-property :level elem) 2))
                   do (setq time (org-element-property :raw-value elem))
                   when (and (eq (org-element-type elem) 'headline)
                             (= (org-element-property :level elem) 1))
                   do (let ((created (org-element-property :CREATED elem)))
                        (string-match date-re created)
                        (setq date
                              (time-convert
                               (encode-time
                                0 0 0
                                (string-to-number (match-string 3 created)) ; day
                                (string-to-number (match-string 2 created)) ; month
                                (string-to-number (match-string 1 created))) ; year
                               'integer))))
          (cons
           tag
           (org-journal-tag-reference--create
            :ref-start (line-number-at-pos (org-element-property :begin parent))
            :ref-end (line-number-at-pos (org-element-property :end parent))
            :time time
            :date date)))))))

(provide 'org-journal-tags)
;;; org-journal-tags.el ends here
