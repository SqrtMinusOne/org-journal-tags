;;; org-journal-tags.el --- TODO -*- lexical-binding: t -*-

;; Copyright (C) 2022 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org-journal "2.1.2") (magit-section "3.3.0"))
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
(require 'cl-lib)
(require 'seq)
(require 'crm)
(require 'magit-section)
(require 'org-journal)
(require 'org-macs)

(defgroup org-journal-tags ()
  "Manage tags for org-journal"
  :group 'org-journal)

(defcustom org-journal-tags-db-file
  (if (boundp 'no-littering-var-directory)
      (concat no-littering-var-directory "org-journal-tags/index")
    (concat user-emacs-directory "org-journal-tags/index"))
  "Location of the org-journal-tags database."
  :group 'org-journal-tags
  :type 'file)

(defvar org-journal-tags-db nil
  "The core org-journal-tags database.

The database is an alist with two keys: :tags and :files.

`:tags' is a hash-map with tag names as keys and instances of
`org-journal-tag' as values.

`:files' is also a hash-map with org-journal files as keys and
timestamps of their last update as values.  This is used to keep
track of updates in the filesystem, for instance when journal
files are created on some other machine.

The database is stored in the file, path to which is set by
`org-journal-tags-db-file', loaded from filesystem with
`org-journal-tags-db-load' and saved with
`org-journal-tags-db-save'.")

(defface org-journal-tags-tag-face
  '((t (:inherit warning)))
  "Default face for org-journal tags."
  :group 'org-journal-tags)

(defface org-journal-tags-info-face
  '((t (:inherit success)))
  "A face to higlight various information."
  :group 'org-journal-tags)

(defcustom org-journal-tags-face-function #'org-journal-tags--face-default
  "A function to get the face of a tag.

The only argument is the tag string.  The default one just returs
`org-journal-tags-tag-face'."
  :group 'org-journal-tags
  :type 'function)

(defcustom org-journal-tags-default-tag-prop "Tags"
  "Default :TAGS: property name for `org-journal-tags-set-prop'.

For now, this can only be variations of the word \"tags\" in
different cases."
  :group 'org-journal-tags
  :type 'string)

(defcustom org-journal-tags-format-new-tag-function
  #'org-journal-tags--format-new-tag-default
  "A function to format a newly inserted org journal tag.

Used by `org-journal-tags-insert-tag' and
`org-journal-tags-set-prop'."
  :type 'function
  :group 'org-journal-tags)

(defun org-journal-tags--format-new-tag-default (tag)
  "Default formatting function for new org journal tags.

TAG is a string with the tag name."
  (format "[[org-journal:%s][#%s]]" tag tag))

(defun org-journal-tags--face-default (tag)
  "A function to return the default tag face.

TAG is a string with the tag name."
  'org-journal-tags-tag-face)


;; Data model and database

(cl-defstruct (org-journal-tag (:constructor org-journal-tag--create))
  "A structure that holds one org journal tag.

The properties are:
- `:name': Tag name.
- `:dates': Hash map with timestamps as keys and lists of
  `org-journal-tag-reference' as values."
  name
  (dates (make-hash-table)))

(cl-defstruct (org-journal-tag-reference (:constructor org-journal-tag-reference--create))
  "A structure that holds one reference to an org journal tag.

The properties are:
- `:ref-start': Start of the referenced region
- `:ref-end': End of the referenced region
- `:time': A string that holds the time of the referneced record.
  Doesn't have to be in any particular format.
- `:date': A timestamp with the date of the referenced record."
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

(defun org-journal-tags-db-reset ()
  "Reset the org-journal-tags database."
  (interactive)
  (setf org-journal-tags-db (org-journal-tags-db--empty)))

(defun org-journal-tags-db-save ()
  "Save the org-journal-tags database to the filesystem."
  (interactive)
  (org-journal-tags-db-ensure)
  (mkdir (file-name-directory org-journal-tags-db-file) t)
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file org-journal-tags-db-file
      (let ((standard-output (current-buffer))
            (print-level nil)
            (print-length nil)
            (print-circle nil))
        (princ ";;; Org Journal Tags Database\n\n")
        (prin1 org-journal-tags-db))))
  :success)

(defun org-journal-tags-db-save-safe ()
  "Save the org-journal-tags database, ignoring errors.

This can be put to `kill-emacs-hook' and not screw up anything
with exceptions."
  (ignore-errors
    (org-journal-tags-db-save)))


(defun org-journal-tags-db-unload ()
  "Unload the org-journal-tags database"
  (interactive)
  (org-journal-tags-db-save)
  (setf org-journal-tags-db nil))


;; Org link

(defun org-journal-tags--follow (tag prefix)
  "TODO. Eventually this fill do something."
  (message (org-journal-tags--links-get-tag tag)))

(defun org-journal-tags--complete (&optional arg)
  "Create an org-journal-tags link using completion."
  (org-journal-tags-db-ensure)
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


;; Tags extraction and persistence

(defun org-journal-tags--ensure-decrypted ()
  "Ensure that the current org-journal is decrypted."
  (when org-journal-enable-encryption
    (save-excursion
      (goto-char (point-min))
      (while (search-forward ":crypt:" nil t)
        (org-decrypt-entry)))))

(defun org-journal-tags--links-get-tag (link)
  "Get the tag name from LINK.

LINK is either Org element or string."
  (replace-regexp-in-string
   (rx "::" (* nonl) eos)
   ""
   (or (org-element-property :path link) link)))

(defun org-journal-tags--get-element-parent (elem type)
  "Get the first parent of ELEM of the type TYPE."
  (cl-loop while elem do (setq elem (org-element-property :parent elem))
           if (eq (org-element-type elem) type)
           return elem))

(defun org-journal-tags--links-inline-get-region (link)
  "Get region boundaries referenced by LINK.

LINK should be an Org element with tree context set, e.g. returned
from `org-element-parse-buffer'."
  (let ((elems (split-string (org-element-property :path link) "::"))
        (paragraph (org-journal-tags--get-element-parent link 'paragraph)))
    (if (= (length elems) 1)
        (list (org-element-property :begin paragraph)
              (org-element-property :end paragraph))
      (let ((next-siblings (string-to-number (nth 1 elems)))
            (container (org-element-property :parent paragraph))
            (begin (org-element-property :begin paragraph))
            i end)
        (cl-loop for elem in (org-element-contents container)
                 if (eq elem paragraph) do (setq i 0)
                 if i do (progn
                           (setq end (org-element-property :end elem))
                           (cl-incf i))
                 if (and i (>= i next-siblings)) return nil)
        (unless end
          (setq end (org-element-property :end paragraph)))
        (list begin end)))))

(defun org-journal-tags--links-extract-inline ()
  "Extract inline links from the current org-journal buffer.

Inline links are ones that are just placed in the section.  Available formats:
- [[org-journal:<link-name>]]
- [[org-journal:<link-name>::<ref-number>]]
In the first case, only the current paragraph is referenced.  In the
second case, it's the current paragraph and ref-number of next
paragraphs."
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) "org-journal")
        (let ((tag (org-journal-tags--links-get-tag link))
              (region (org-journal-tags--links-inline-get-region link))
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
                        (setq date
                              (org-journal-tags--parse-journal-created
                               created date-re))))
          (cons
           tag
           (org-journal-tag-reference--create
            :ref-start (nth 0 region)
            :ref-end (nth 1 region)
            :time time
            :date date)))))))

(defun org-journal-tags--links-parse-link-str (str)
  "Extract the tag name from a text representation of org link.

STR should be a string of one of the following formats:
- [[org-journal:<tag-name>]]
- [[org-journal:<tag-name>][<tag-desc>]]

<tag-name> or nil will be returned."
  (when (string-match
         (rx bos "[[org-journal:" (group (* (not "]"))) "]"
             (? (* nonl)) "]" eos)
         str)
    (match-string 1 str)))

(defun org-journal-tags--parse-journal-created (created &optional date-re)
  "Parse a date from the :CREATED: property of org-journal."
  (unless date-re
    (setq date-re (org-journal--format->regex
                   org-journal-created-property-timestamp-format)))
  (string-match date-re created)
  (time-convert
   (encode-time
    0 0 0
    (string-to-number (match-string 3 created))  ; day
    (string-to-number (match-string 2 created))  ; month
    (string-to-number (match-string 1 created))) ; year
   'integer))

(defun org-journal-tags--links-extract-section ()
  "Extract section-wide links.

These links can be placed in the :TAGS: property of the section
and reference the entire section."
  (let (result
        (date-re (org-journal--format->regex
                  org-journal-created-property-timestamp-format)))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (elem)
        (when-let ((tags-prop (org-element-property :TAGS elem))
                   (_ (= (org-element-property :level elem) 2))
                   (created
                    (when-let (created (org-element-property
                                        :CREATED
                                        (org-element-property :parent elem)))
                      (org-journal-tags--parse-journal-created created date-re))))
          (cl-loop for link in (split-string tags-prop)
                   do (when-let ((tag (org-journal-tags--links-parse-link-str link)))
                        (push (cons
                               tag
                               (org-journal-tag-reference--create
                                :ref-start (org-element-property :contents-begin elem)
                                :ref-end (org-element-property :contents-end elem)
                                :time (org-element-property :raw-value elem)
                                :date created))
                              result))))))
    result))

(defun org-journal-tags--links-extract ()
  "Extract tags from the current org-journal buffer.

Returns an alist of the format (tag-name . reference), where
reference is `org-journal-tag-reference'.  Tag names in the alist
can repeat."
  (org-journal-tags--ensure-decrypted)
  (append
   (org-journal-tags--links-extract-inline)
   (org-journal-tags--links-extract-section)))


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

(defun org-journal-tags--links-store (references)
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

;;;###autoload
(defun org-journal-tags-process-buffer (&optional process-file)
  "Update the org-journal-tags with the current buffer.

By default it only updates the :tags part of
`org-journal-tags-db'.  If PROCESS-FILE is non-nil, it also
updates the :file part.  The latter happens if the function is
called interactively."
  (interactive "p")
  (org-journal-tags-db-ensure)
  (org-journal-tags--links-store
   (org-journal-tags--links-extract))
  (when process-file
    (org-journal-tags--record-file-processed)))

(defun org-journal-tags--parse-journal-date (date-journal)
  "Parse a date from the format used in org-journal.

DATE-JOURNAL is a list of (month day year)."
  (encode-time
   0 0 0
   (nth 1 date-journal)
   (nth 0 date-journal)
   (nth 2 date-journal)))

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
                             (org-journal-tags--parse-journal-date date-journal)
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

;;;###autoload
(defun org-journal-tags-db-sync ()
  "Update the org-journal-tags database with all journal files."
  (interactive)
  (org-journal-tags-db-ensure)
  (org-journal-tags--cleanup-missing-files)
  (org-journal-tags--clear-empty-tags)
  (org-journal-tags--sync-updated-files))


;; Manage tags in the current buffer

(defun org-journal-tags--prop-get-tags (elem)
  "Get all org-journal tags from ELEM.

ELEM should be a headline Org element."
  (thread-last
    (or (org-element-property :TAGS elem)
        "")
    split-string
    (mapcar #'org-journal-tags--links-parse-link-str)
    (seq-filter (lambda (t) t))))

(cl-defun org-journal-tags-prop-apply-delta (&key elem add remove)
  "Apply changes to org-journal tags to the current section.

ELEM should be a level-2 Org headline.  The point is assumed to
be set at the start of the headline.

ADD is a list of tags to add to the current headline, REMOVE is a
list of tags to remove."
  (unless elem
    (setq elem (org-element-at-point)))
  (unless (= 2 (org-element-property :level elem))
    (error "The element at point isn't a level 2 headline!"))
  (save-excursion
    (thread-last
      (org-journal-tags--prop-get-tags elem)
      (seq-filter (lambda (s) (not (seq-contains remove s))))
      (append add)
      seq-uniq
      (seq-sort #'string-lessp)
      (mapcar org-journal-tags-format-new-tag-function)
      ((lambda (tags) (string-join tags " ")))
      (org-set-property org-journal-tags-default-tag-prop))))

;;;###autoload
(defun org-journal-tags-prop-set ()
  "Set up the \"tags\" property of the current org-journal section."
  (interactive)
  (org-journal-tags-db-ensure)
  (save-excursion
    (outline-back-to-heading)
    (let ((elem (org-element-at-point)))
      (unless (= 2 (org-element-property :level elem))
        (user-error "Can't find a level 2 heading!"))
      (let* ((all-tags (cl-loop for tag being the hash-keys of
                                (alist-get :tags org-journal-tags-db)
                                collect tag))
             (tags (org-journal-tags--prop-get-tags elem))
             (add-tags (seq-difference all-tags tags))
             (options (append
                       (mapcar (lambda (tag) (format "+%s" tag)) add-tags)
                       (mapcar (lambda (tag) (format "-%s" tag)) tags)))
             (crm-separator " ")
             ;; By default, space is bound to "complete word" function.
             ;; Re-bind it to insert a space instead.  Note that <tab>
             ;; still does the completion.
             (crm-local-completion-map
              (let ((map (make-sparse-keymap)))
                (set-keymap-parent map crm-local-completion-map)
                (define-key map " " 'self-insert-command)
                map))
             (changes (completing-read-multiple "Tags: " options))
             (add-tags-res (thread-last
                             changes
                             (seq-filter (lambda (s)
                                           (string-match-p (rx bos "+") s)))
                             (mapcar (lambda (s) (substring s 1)))))
             (remove-tags-res (thread-last
                                changes
                                (seq-filter (lambda (s)
                                              (string-match-p (rx bos "-") s)))
                                (mapcar (lambda (s) (substring s 1))))))
        (org-journal-tags-prop-apply-delta
         :elem elem
         :add add-tags-res
         :remove remove-tags-res)))))

;;;###autoload
(defun org-journal-tags-insert-tag ()
  "Insert org-journal tag at point."
  (interactive)
  (org-journal-tags-db-ensure)
  (thread-last
    (cl-loop for tag being the hash-keys of
             (alist-get :tags org-journal-tags-db)
             collect tag)
    (completing-read "Tag: " )
    (funcall org-journal-tags-format-new-tag-function)
    insert))

;; View and global setup

(defun org-journal-tags--setup-buffer ()
  "Setup the update of `org-journal-tags-db' after buffer save."
  (add-hook 'before-save-hook #'org-journal-tags-process-buffer -100 t)
  (add-hook 'after-save-hook #'org-journal-tags--record-file-processed nil t))

;;;###autoload
(define-minor-mode org-journal-tags-autosync-mode
  "Automatically update the org-journal-tags database.

This does two things:
- sets up individual org journal buffers to update to database after
  save.
- sets up saving the database on exit from Emacs.

If you don't want to turn this on, you can manually call:
- `org-journal-tags-process-buffer' to process the current org-journal
  buffer
- `org-journal-tags-db-sync' to sync the filesystem
- `org-journal-tags-db-save' to save the database"
  :global t
  (if org-journal-tags-autosync-mode
      (progn
        (add-hook 'org-journal-mode-hook #'org-journal-tags--setup-buffer)
        (add-hook 'kill-emacs-hook #'org-journal-tags-db-save-safe))
    (remove-hook 'org-journal-mode-hook #'org-journal-tags--setup-buffer)
    (remove-hook 'kill-emacs-hook #'org-journal-tags-db-save-safe)))

(defvar org-journal-tags-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (when (fboundp #'evil-define-key*)
      (evil-define-key* 'normal map
        (kbd "<tab>") #'magit-section-toggle
        "q" #'kill-buffer-and-window))
    map)
  "A keymap for `org-journal-tags-status-mode'.")

(define-derived-mode org-journal-tags-status-mode magit-section "Org Journal Tags"
  "TODO")

(defun org-journal-tags--buffer-render-info ()
  (let ((dates (org-journal--list-dates)))
    (insert (format "Date:          %s\n"
                    (propertize (format-time-string "%Y-%m-%d")
                                'face 'org-journal-tags-info-face)))
    (insert (format "Last record:   %s\n"
                    (propertize (thread-last
                                  (last dates)
                                  car
                                  org-journal-tags--parse-journal-date
                                  (format-time-string "%Y-%m-%d"))
                                'face 'org-journal-tags-info-face)))
    (insert (format "Total tags:    %s\n"
                    (propertize (thread-first
                                  (alist-get :tags org-journal-tags-db)
                                  hash-table-count
                                  number-to-string)
                                'face 'org-journal-tags-info-face)))
    (insert (format "Total dates:   %s\n"
                    (propertize (number-to-string (length dates))
                                'face 'org-journal-tags-info-face)))))

(defun org-journal-tags--buffer-render-contents ()
  "Render the contents of the org-journal-tags status buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (org-journal-tags-status-mode)
    (magit-insert-section (org-journal-tags)
      (magit-insert-heading)
      (org-journal-tags--buffer-render-info))))

;;;###autoload
(defun org-journal-tags-status ()
  "TODO"
  (interactive)
  (org-journal-tags-db-ensure)
  (when-let ((buffer (get-buffer "*org-journal-tags*")))
    (kill-buffer buffer))
  (let ((buffer (get-buffer-create "*org-journal-tags*")))
    (with-current-buffer buffer
      (org-journal-tags--buffer-render-contents))
    (switch-to-buffer-other-window buffer)))

(provide 'org-journal-tags)
;;; org-journal-tags.el ends here
