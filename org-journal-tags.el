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
  name
  (dates (make-hash-table)))

(cl-defstruct (org-journal-tag-reference (:constructor org-journal-tag-reference--create))
  ref-start ref-end time date)

(defun org-journal-tags-db--empty ()
  "Create an empty org-journal-tags database."
  `((:tags . ,(make-hash-table :test #'equal))
    (:files . ,(make-hash-table :test #'equal))))

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

(defun org-journal-tags--complete (&optional arg)
  "Create an org-journal-tags link using completion."
  (let ((name (completing-read
               "Tag: "
               (cl-loop for k being the hash-keys of
                        (alist-get :tags org-journal-tags-db)
                        collect k))))
    (format "org-journal:%s" name)))

(org-link-set-parameters
 "org-journal"
 :follow #'org-journal-tags--follow
 :complete #'org-journal-tags--complete
 :face (lambda (&rest args) (funcall org-journal-tags-face-function args)))

(defun org-journal-tags--ensure-decrypted ()
  "Ensure that the current org-journal is decrypted."
  (when org-journal-enable-encryption
    (save-excursion
      (goto-char (point-min))
      (while (search-forward ":crypt:" nil t)
        (org-decrypt-entry)))))

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

(defun org-journal-tags--clear-date (date)
  "Remove all references to DATE from the database."
  (maphash
   (lambda (tag-name tag)
     (remhash date (org-journal-tag-dates tag)))
   (alist-get :tags org-journal-tags-db)))

(defun org-journal-tags--clear-empty-tags ()
  "Remove tags with no references from the database."
  (let ((keys (cl-loop for tag-name being the hash-keys of
                       (alist-get :tags org-journal-tags-db)
                       using (hash-values tag)
                       when (= 0 (hash-table-count (org-journal-tag-dates tag)))
                       collect tag-name)))
    (cl-loop for key in keys do
             (remhash key (alist-get :tags org-journal-tags-db)))))

(defun org-journal-tags--store-links (references)
  "Store tag references in the org-journal-tags database.

REFERENCES is a list, where one element is a cons cell
of (tag-name . `org-journal-tag-reference')"
  (thread-last
    references
    (mapcar (lambda (ref) (org-journal-tag-reference-date (cdr ref))))
    seq-uniq
    (mapcar #'org-journal-tags--clear-date))
  (cl-loop for ref-elem in references
           for tag-name = (car ref-elem)
           for ref = (cdr ref-elem)
           with tags-hash = (alist-get :tags org-journal-tags-db)
           unless (gethash tag-name tags-hash)
           do (puthash tag-name (org-journal-tag--create :name tag-name)
                       tags-hash)
           for tag = (gethash tag-name tags-hash)
           do (let ((dates-hash (org-journal-tag-dates tag))
                    (date (org-journal-tag-reference-date ref)))
                (puthash date
                         (or (if-let ((date-ref-list (gethash date dates-hash)))
                                 (push ref date-ref-list)
                               (list ref)))
                         dates-hash)))
  (org-journal-tags--clear-empty-tags))

(defun org-journal-tags--record-file-processed ()
  "Save the last modification timestamp to the database."
  (puthash
   (buffer-file-name)
   (time-convert
    (nth 5 (file-attributes (buffer-file-name)))
    'integer)
   (alist-get :files org-journal-tags-db)))

(defun org-journal-tags-process-buffer ()
  "Update the org-journal-tags with the current buffer."
  (interactive)
  (org-journal-tags-db-ensure)
  (org-journal-tags--store-links
   (org-journal-tags--extract-links)))

(defun org-journal-tags--cleanup-missing-files ()
  "Remove references to the deleted org journal files."
  ;; First remove missing files
  (let ((files-hash (copy-hash-table (alist-get :files org-journal-tags-db))))
    (cl-loop for file in (org-journal--list-files)
             do (remhash file files-hash))
    (when (< 0 (hash-table-size files-hash))
      (cl-loop for removed-file being the hash-keys of files-hash
               do (remhash removed-file (alist-get :files org-journal-tags-db)))
      ;; If a file is removed, it is also necessary to filter the
      ;; removed dates from the DB
      (let ((dates-hash (make-hash-table)))
        (cl-loop for tag being the hash-values of
                 (alist-get :tags org-journal-tags-db)
                 do (cl-loop for date being the hash-keys of
                             (org-journal-tag-dates tag)
                             do (puthash date nil dates-hash)))
        (cl-loop for date-journal in (org-journal--list-dates)
                 for date = (time-convert
                             (encode-time
                              0 0 0
                              (nth 1 date-journal)
                              (nth 0 date-journal)
                              (nth 2 date-journal))
                             'integer)
                 do (remhash date dates-hash))
        (cl-loop for tag being the hash-values of
                 (alist-get :tags org-journal-tags-db)
                 do (cl-loop for removed-date being the hash-keys of
                             dates-hash
                             do (remhash removed-date (org-journal-tag-dates tag))))))))

(defun org-journal-tags--sync-updated-files ()
  "Update the database with new or updated org-journal files."
  (cl-loop for file in (org-journal--list-files)
           for last-updated = (time-convert
                               (nth 5 (file-attributes file))
                               'integer)
           when (let ((date
                       (gethash file (alist-get :files org-journal-tags-db))))
                  (or (null date) (> last-updated date)))
           do (with-temp-buffer
                (message "Syncronizing org-journal-tags database...")
                (insert-file-contents file)
                (setq-local buffer-file-name file)
                (org-mode)
                (org-journal-tags-process-buffer)
                (org-journal-tags--record-file-processed)
                (set-buffer-modified-p nil))))

(defun org-journal-tags-sync-db ()
  "Update the org-journal-tags database with all journal files."
  (interactive)
  (org-journal-tags-db-ensure)
  (org-journal-tags--cleanup-missing-files)
  (org-journal-tags--clear-empty-tags)
  (org-journal-tags--sync-updated-files))

;;;###autoload
(define-minor-mode org-journal-tags-autosync-mode
  "Automatically update the org-journal-tags database.

Hook this to `org-journal-mode-hook' like this:
(add-hook 'org-journal-mode-hook
          #'org-journal-tags-autosync-mode)"
  :init-value nil
  (if org-journal-tags-autosync-mode
      (progn
        (add-hook 'before-save-hook #'org-journal-tags-process-buffer -100 t)
        (add-hook 'after-save-hook #'org-journal-tags--record-file-processed nil t))
    (remove-hook 'before-save-hook #'org-journal-tags-process-buffer t)
    (remove-hook 'after-save-hook #'org-journal-tags--record-file-processed t)))

(provide 'org-journal-tags)
;;; org-journal-tags.el ends here
