;;; org-journal-tags.el --- Tagging and querying system of org-journal -*- lexical-binding: t -*-

;; Copyright (C) 2022 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org-journal "2.1.2") (magit-section "3.3.0") (transient "0.3.7"))
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
(require 'crm)
(require 'magit-section)
(require 'org-element)
(require 'org-journal)
(require 'org-macs)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'widget)


;; XXX No idea why the byte compiler doesn't see the function and
;; doesn't let me (require 'cl-macs).  It is required in cl-lib though
(declare-function cl--alist-to-plist "cl-macs")

;; XXX I want to have the compatibility with evil-mode without
;; requiring it, so I check whether this function is bound later in
;; the code.
(declare-function evil-define-key* "evil-core")

;; XXX Also no idea why the byte compiler doesn't see this
;; function. It is obviously in subr-x
(declare-function string-pad "subr-x")

(defgroup org-journal-tags ()
  "Tagging and querying system for org-journal."
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

The database is an alist with two keys:
- `:tags' is a hash-map with tag names as keys and instances of
  `org-journal-tag' as values.
- `:files' is also a hash-map with org-journal files as keys and
  timestamps of their last update as values.  This is used to keep
  track of updates in the filesystem, for instance when journal files
  are created on some other machine.

Everywhere in this packages dates are used in the form of UNIX
timestamp, e.g. such as retunrned by (time-convert nil 'integer).

The database is stored in the file, path to which is set by
`org-journal-tags-db-file'.

Also take a look at `org-journal-tags-db-load' and
`org-journal-tags-db-save'.")

(defface org-journal-tags-tag-face
  '((t (:inherit warning)))
  "Default face for org-journal tags in the org buffer."
  :group 'org-journal-tags)

(defface org-journal-tags-info-face
  '((t (:inherit success)))
  "A face to highlight various information."
  :group 'org-journal-tags)

(defface org-journal-tags-date-header
  '((t (:inherit magit-section-heading)))
  "A face for date headings in the query result buffer."
  :group 'org-journal-tags)

(defface org-journal-tags-time-header
  '((t (:inherit magit-section-secondary-heading)))
  "A face for time headings in the query result buffer."
  :group 'org-journal-tags)

(defcustom org-journal-tags-face-function #'org-journal-tags--face-default
  "A function to get a face of a tag.

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
  "A function to format the newly inserted org journal tag.

Used by `org-journal-tags-insert-tag' and
`org-journal-tags-set-prop'."
  :type 'function
  :group 'org-journal-tags)

(defcustom org-journal-tags-query-ascending-sort nil
  "If t, sort the query results in ascending order."
  :type 'boolean
  :group 'org-journal-tags)

(defcustom org-journal-tags-barchart-symbols
  '(" " "▁" "▂" "▃" "▄" "▅" "▆" "▇")
  "Symbols to draw a horizontal barchart."
  :type 'list
  :group 'org-journal-tags)

(defface org-journal-tags-barchart-face
  ;; XXX I don't think `:inherit' and override would harm anyone
  ;; here. `fixed-pitch' generally has only the `:family' attribute
  ;; set.
  `((t (:foreground ,(face-foreground 'warning) :inherit fixed-pitch)))
  "A face to plot a horizontal barchar."
  :group 'org-journal-tags)

(defcustom org-journal-tags-date-group-func #'org-journal-tags--group-date-default
  "A function to group dates in barcharts.

Take a look at `org-journal-tags--group-refs-by-date' for more
detail."
  :type 'function
  :group 'org-journal-tags)

(defconst org-journal-tags-query-buffer-name "*org-journal-query*"
  "Default buffer name for org-journal-tags quieries.")

(defconst org-journal-tags-status-buffer-name "*org-journal-tags*"
  "Default buffer name for org-journal-tags status.")

(defun org-journal-tags--format-new-tag-default (tag)
  "Default formatting function for new org journal tags.

TAG is a string with the tag name."
  (format "[[org-journal:%s][#%s]]" tag tag))

(defun org-journal-tags--face-default (&rest _)
  "Return the default tag face."
  'org-journal-tags-tag-face)

(defun org-journal-tags--group-date-default (date)
  "Return year and week of DATE.

DATE is a UNIX timestamp."
  (format-time-string "%Y-%W" (seconds-to-time date)))


(defvar-local org-journal-tags--query-refs nil
  "The current query results in the org-journal-tags query buffer.")

(defvar-local org-journal-tags--query-params nil
  "Query params in the org-journal-tags query buffer or elsewhere.

Overriding this variable can be used to change the starting value
of infixes in `org-journal-tags-transient-query'.")

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
  "A structure that holds one reference to some piece of org-journal.

The properties are:
- `:ref-start': Start of the referenced region.
- `:ref-end': End of the referenced region.
- `:time': A string that holds the time of the reference record.
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

(defun org-journal-tags--valid-tag-p (tag-name)
  "Check if TAG-NAME is a valid tag name for org-journal-tags.

Empty string is reserved as a \"tag\" that references every
record in the journal."
  (not (string-empty-p tag-name)))

(defun org-journal-tags--list-tags ()
  "Return all saved org-journal tag names."
  (cl-loop for tag-name being the hash-keys of
           (alist-get :tags org-journal-tags-db)
           if (org-journal-tags--valid-tag-p tag-name)
           collect tag-name))

(defun org-journal-tags-db-unload ()
  "Unload the org-journal-tags database."
  (interactive)
  (org-journal-tags-db-save)
  (setf org-journal-tags-db nil))


;; Org link

(defun org-journal-tags--follow (tag _prefix)
  "Open org-journal-tags query transient for TAG."
  (let ((org-journal-tags--query-params
         `((:tag-names . (,(replace-regexp-in-string
                            (rx "::" (* nonl) eos)
                            ""
                            tag))))))
    ;; XXX `org-journal-tags--query-params' is used in the init-value
    ;; method of infixes of the `org-journal-tags-transient-query'.  I
    ;; have no idea how else to silence the "Unused lexical variable"
    ;; warning.
    (ignore org-journal-tags--query-params)
    (org-journal-tags-transient-query)))

(defun org-journal-tags--complete (&optional _)
  "Create an org-journal-tags link using completion."
  (org-journal-tags-db-ensure)
  (let ((name (completing-read
               "Tag: "
               (org-journal-tags--list-tags))))
    (unless (org-journal-tags--valid-tag-p name)
      (user-error "Invalid tag name: %s" name))
    (format "org-journal:%s" name)))

(org-link-set-parameters
 "org-journal"
 :follow #'org-journal-tags--follow
 :complete #'org-journal-tags--complete
 :face (lambda (&rest args) (funcall org-journal-tags-face-function args)))


;; Tags extraction and persistence

(defun org-journal-tags--ensure-decrypted ()
  "Ensure that the current org-journal buffer is decrypted."
  (when org-journal-enable-encryption
    (save-excursion
      (goto-char (point-min))
      (while (search-forward ":crypt:" nil t)
        (org-decrypt-entry)))))

(defun org-journal-tags--links-get-tag (link)
  "Get tag name from LINK.

LINK is either Org element or string."
  (replace-regexp-in-string
   (rx "::" (* nonl) eos)
   ""
   (or (org-element-property :path link) link)))

(defun org-journal-tags--get-element-parent (elem type)
  "Get the first parent of ELEM of the type TYPE.

ELEM is an org element."
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
        (list begin (1- end))))))

(defun org-journal-tags-get-link-region-at-point ()
  "Select region referenced by the org-journal-tags link.

The point should be exactly at the beginning of the link."
  (interactive)
  (let ((link (org-element-link-parser)))
    (unless link
      (user-error "No link found at point"))
    (unless (string-equal (org-element-property :type link) "org-journal")
      (user-error "Link is not of the \"org-jounral\" type"))
    (let ((region (org-journal-tags--links-inline-get-region
                   (org-element-map (org-element-parse-buffer) 'link
                     (lambda (elem)
                       (when (= (org-element-property :begin elem)
                                (org-element-property :begin link))
                         elem))
                     nil t))))
      (set-mark (nth 0 region))
      (goto-char (nth 1 region))
      (activate-mark))))

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
          (when (org-journal-tags--valid-tag-p tag)
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
              :date date))))))))

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
  "Parse a date from the :CREATED: property of org-journal.

DATE-RE is as regex to parse the date, such as one formed by
`org-journal--format->regex'."
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

(defun org-journal-tags--links-extract-section (&optional add-empty)
  "Extract section-wide links.

These links can be placed in the :TAGS: property of the section
and reference the entire section.

If ADD-EMPTY is non-nil, also add an empty tag that references
every section."
  (let (result
        (date-re (org-journal--format->regex
                  org-journal-created-property-timestamp-format)))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (elem)
        (when (= (org-element-property :level elem) 2)
          (when-let ((created
                      (when-let (created (org-element-property
                                          :CREATED
                                          (org-element-property :parent elem)))
                        (org-journal-tags--parse-journal-created created date-re)))
                     (ref (org-journal-tag-reference--create
                           :ref-start (org-element-property :contents-begin elem)
                           :ref-end (org-element-property :contents-end elem)
                           :time (org-element-property :raw-value elem)
                           :date created)))
            (when add-empty
              (push (cons "" (copy-org-journal-tag-reference ref)) result))
            (when-let ((tags (org-element-property :TAGS elem)))
              (cl-loop for link in (split-string tags)
                       do (when-let ((tag (org-journal-tags--links-parse-link-str link)))
                            (push (cons tag (copy-org-journal-tag-reference ref))
                                  result))))))))
    result))

(defun org-journal-tags--links-extract ()
  "Extract tags from the current org-journal buffer.

Returns an alist of the format (tag-name . reference), where
reference is `org-journal-tag-reference'.  Tag names in the alist
can repeat."
  (org-journal-tags--ensure-decrypted)
  (append
   (org-journal-tags--links-extract-inline)
   (org-journal-tags--links-extract-section t)))


(defun org-journal-tags--clear-date (date)
  "Remove all references to DATE from the database."
  (maphash
   (lambda (_tag-name tag)
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
    (mapc #'org-journal-tags--clear-date))
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
   (alist-get :files org-journal-tags-db))
  (org-journal-tags--cache-invalidate (buffer-file-name)))

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
    (seq-filter (lambda (s) s))))

(cl-defun org-journal-tags-prop-apply-delta (&key elem add remove)
  "Apply changes to the tags in the property drawer.

ELEM should be a level-2 org-journal headline.  The point is
assumed to be set at the start of the headline.

ADD is a list of tags to add to the current headline, REMOVE is a
list of tags to remove."
  (unless elem
    (setq elem (org-element-at-point)))
  (unless (= 2 (org-element-property :level elem))
    (error "The element at point isn't a level 2 headline!"))
  (save-excursion
    (thread-last
      (org-journal-tags--prop-get-tags elem)
      (seq-filter (lambda (s) (not (seq-contains-p remove s))))
      (append add)
      seq-uniq
      (seq-sort #'string-lessp)
      (mapcar org-journal-tags-format-new-tag-function)
      (funcall (lambda (tags) (string-join tags " ")))
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
                                           (string-match-p (rx bos "+" (+ nonl)) s)))
                             (mapcar (lambda (s) (substring s 1)))))
             (remove-tags-res (thread-last
                                changes
                                (seq-filter (lambda (s)
                                              (string-match-p (rx bos "-" (+ nonl)) s)))
                                (mapcar (lambda (s) (substring s 1))))))
        (org-journal-tags-prop-apply-delta
         :elem elem
         :add add-tags-res
         :remove remove-tags-res)))))

;;;###autoload
(defun org-journal-tags-insert-tag ()
  "Insert org-journal tag at point."
  (interactive)
  (insert
   (let ((name (completing-read
                "Tag: "
                (org-journal-tags--list-tags))))
     (unless (org-journal-tags--valid-tag-p name)
       (user-error "Invalid tag name: %s" name))
     (funcall org-journal-tags-format-new-tag-function
              name))))
;; Global setup

(defun org-journal-tags--setup-buffer ()
  "Setup the update of `org-journal-tags-db' after buffer save."
  (add-hook 'before-save-hook #'org-journal-tags-process-buffer -100 t)
  (add-hook 'after-save-hook #'org-journal-tags--record-file-processed nil t))

;;;###autoload
(define-minor-mode org-journal-tags-autosync-mode
  "Automatically update the org-journal-tags database.

This does two things:
- sets up individual org journal buffers to update to the database
  after save.
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


;; Query the DB

(defun org-journal-tags--query-construct-dates-hash (refs &optional push-func check-func)
  "Put REFS in a nested hash table by date and time.

REFS is a list of `org-journal-tag-reference'.

PUSH-FUNC is a function that receives two arguments: a list of
references within the same date and time and a new reference to
be added to the list.

CHECK-FUNC is a function that receives two arguments - date and
time - and determines if they should be put in the hash."
  (unless push-func
    (setq push-func
          (lambda (time-refs ref)
            (push ref time-refs))))
  (let ((dates-hash (make-hash-table)))
    (cl-loop
     for ref in refs
     for date = (org-journal-tag-reference-date ref)
     for time = (org-journal-tag-reference-time ref)
     if (or (not check-func) (funcall check-func date time))
     do (progn
          (unless (gethash date dates-hash)
            (puthash date (make-hash-table :test #'equal) dates-hash))
          (let ((times-hash (gethash date dates-hash)))
            (puthash time
                     (funcall push-func
                              (gethash time times-hash)
                              ref)
                     times-hash))))
    dates-hash))

(defun org-journal-tags--query-deconstruct-dates-hash (dates-hash)
  "Deconstruct DATES-HASH to the list of tag references.

DATES-HASH should be in the same format as returned by
`org-journal-tags--query-construct-dates-hash'.

The returned value is a list of `org-journal-tag-reference'."
  (cl-loop for times-hash being the hash-values of dates-hash
           append (cl-loop for refs being the hash-values of times-hash
                           append refs)))

(defun org-journal-tags--nested-segment-p (a1 a2 b1 b2)
  "Check if segment [B1, B2] is nested in [A1, A2]."
  (and (<= a1 b1) (>= a2 b2)))

(defun org-journal-tags--intersecting-segment-p (a1 a2 b1 b2)
  "Check if [A1, A2] intersects with (not nested in!) [B1, B2]."
  (or (and (<= a1 b1) (<= b1 a2))
      (and (<= b1 a1) (<= a1 b2))))

(defun org-journal-tags--query-merge-refs-push (time-refs ref)
  "Add REF to the list of org-journal references.

REF is an instance of `org-journal-tag-reference', TIME-REFS is a
list of such instances.  All references are assumed to be of
equal time and date.

If REF is nested in one or many of the references of TIME-REFS or
vice versa, a larger reference will be kept.

If REF intersects with some reference in TIME-REFS, an
intersection of the two references will be saved.

Thus, after this operation, there will be no intersection between
references."
  (or (cl-loop
       with ref-start = (org-journal-tag-reference-ref-start ref)
       with ref-end = (org-journal-tag-reference-ref-end ref)
       for old-ref in time-refs
       for old-ref-start = (org-journal-tag-reference-ref-start old-ref)
       for old-ref-end = (org-journal-tag-reference-ref-end old-ref)
       ;; If the new reference is nested in the old one, do nothing
       if (org-journal-tags--nested-segment-p
           old-ref-start old-ref-end
           ref-start ref-end)
       return time-refs
       ;; If some old reference is nested in the new one, replace old one(s)
       if (org-journal-tags--nested-segment-p
           ref-start ref-end
           old-ref-start old-ref-end)
       return (append
               (seq-remove (lambda (r)
                             (org-journal-tags--nested-segment-p
                              ref-start ref-end
                              (org-journal-tag-reference-ref-start r)
                              (org-journal-tag-reference-ref-end r)))
                           time-refs)
               (list ref))
       ;; If the new reference intersects with some old one, put
       ;; the intersection of all
       if (org-journal-tags--intersecting-segment-p
           old-ref-start old-ref-end
           ref-start ref-end)
       return (let ((int (seq-filter
                          (lambda (r)
                            (org-journal-tags--intersecting-segment-p
                             ref-start ref-end
                             (org-journal-tag-reference-ref-start r)
                             (org-journal-tag-reference-ref-end r)))
                          time-refs)))
                (append
                 (seq-difference time-refs int)
                 (list (org-journal-tag-reference--create
                        :ref-start (seq-min
                                    (append
                                     (mapcar #'org-journal-tag-reference-ref-start
                                             int)
                                     (list ref-start)))
                        :ref-end (seq-max
                                  (append
                                   (mapcar #'org-journal-tag-reference-ref-end
                                           int)
                                   (list ref-end)))
                        :time (org-journal-tag-reference-time ref)
                        :date (org-journal-tag-reference-date ref))))))
      (append time-refs (list ref))))

(defun org-journal-tags--query-merge-refs (refs)
  "Merge intersecting org-journal-tags references.

REFS is a list of instances of `org-journal-tag-reference'.
After this function, no two references will be intersecting or
nested in one another."
  (org-journal-tags--query-deconstruct-dates-hash
   (org-journal-tags--query-construct-dates-hash
    refs
    #'org-journal-tags--query-merge-refs-push)))

(defun org-journal-tags--query-union-refs (refs-1 refs-2)
  "Return union of REFS-1 and REFS-2.

REFS-1 and REFS-2 are lists of instances of
`org-journal-tag-reference'."
  (org-journal-tags--query-merge-refs
   (append
    refs-1
    refs-2)))

(defun org-journal-tags--query-diff-to-one-ref (refs target-ref)
  "Exclude all intersections of TARGET-REF with REFS from TARGET-REF.

REFS is a list of `org-journal-tag-reference', TARGET-REF is one
instance of `org-journal-tag-reference'.  All references are
assumed to have one date and time.

The return value is a list of `org-journal-tag-reference'.  The
list may be empty (if TARGET-REF is nested in one of REFS, for
instance), it may be multiple references (if some reference in
REFS splits TARGET-REF in two) or it may be one reference."
  (let ((result (list target-ref))
        (date (org-journal-tag-reference-date target-ref))
        (time (org-journal-tag-reference-time target-ref)))
    (dolist (ref-2 refs)
      ;; A shallow copy because we're modifying RESULT
      (dolist (ref-1 (seq-copy result))
        ;; [start-1, end-1] is what we're trying to insert
        ;; [start-2, end-2] is a segment from REFS that shouldn't
        ;; overlap with the former
        (let ((start-1 (org-journal-tag-reference-ref-start ref-1))
              (end-1 (org-journal-tag-reference-ref-end ref-1))
              (start-2 (org-journal-tag-reference-ref-start ref-2))
              (end-2 (org-journal-tag-reference-ref-end ref-2)))
          (cond
           ;; If [start-1, end-1] is nested in [start-2, end-2], remove
           ;; the first segment altogether
           ((org-journal-tags--nested-segment-p
             start-2 end-2
             start-1 end-1)
            (setq result (seq-filter (lambda (r) (not (eq r ref-1))) result)))
           ;; If [start-2, end-2] is nested in [start-1, end-1], split
           ;; the first segment in two.  This excludes equality of the
           ;; segments because of the previous condition.
           ((org-journal-tags--nested-segment-p
             start-1 end-1
             start-2 end-2)
            (setq result
                  (append
                   (seq-filter (lambda (r) (not (eq r ref-1))) result)
                   (list
                    (org-journal-tag-reference--create
                     :ref-start start-1 :ref-end start-2
                     :date date :time time)
                    (org-journal-tag-reference--create
                     :ref-start end-2 :ref-end end-1
                     :date date :time time)))))
           ;; start-1 <= start-2 <= end-1
           ;; The segment [start-2, end-1] is overlapping
           ((and (<= start-1 start-2) (<= start-2 end-1))
            (setq result
                  (append
                   (seq-filter (lambda (r) (not (eq r ref-1))) result)
                   (list
                    (org-journal-tag-reference--create
                     :ref-start start-1 :ref-end start-2
                     :date date :time time)))))
           ;; start-2 <= start-1 <= end-2
           ;; The segment [start-1, end-2] is overlapping
           ((and (<= start-2 start-1) (<= start-1 end-2))
            (setq result
                  (append
                   (seq-filter (lambda (r) (not (eq r ref-1))) result)
                   (list
                    (org-journal-tag-reference--create
                     :ref-start end-2 :ref-end end-1
                     :date date :time time)))))
           ;; Do nothing if there are no overlaps
           ))))
    result))

(defun org-journal-tags--query-diff-refs (refs-1 refs-2)
  "Remove all intersections between REFS-1 and REFS-2 from REFS-1.

REFS-1 and REFS-2 are lists of instances of
`org-journal-tag-reference'."
  (let ((dates-hash-2 (org-journal-tags--query-construct-dates-hash refs-2)))
    (org-journal-tags--query-deconstruct-dates-hash
     (org-journal-tags--query-construct-dates-hash
      refs-1
      (lambda (time-refs-1 ref-1)
        (let ((date (org-journal-tag-reference-date ref-1))
              (time (org-journal-tag-reference-time ref-1)))
          (if-let* ((times-hash-2 (gethash date dates-hash-2))
                    (time-refs-2 (gethash time times-hash-2)))
              (append
               time-refs-1
               (org-journal-tags--query-diff-to-one-ref time-refs-2 ref-1))
            (push ref-1 time-refs-1))))))))

(defun org-journal-tags--query-intersect-to-one-ref (refs ref-1)
  "Return parts of REFS that intersect with REF-1.

REFS is a list org `org-journal-tag-reference', REF-1 is one
`org-journal-tag-reference'.

The return value is a list of `org-journal-tag-reference'."
  (let ((date (org-journal-tag-reference-date ref-1))
        (time (org-journal-tag-reference-time ref-1))
        result)
    (dolist (ref-2 refs)
      (let ((start-1 (org-journal-tag-reference-ref-start ref-1))
            (end-1 (org-journal-tag-reference-ref-end ref-1))
            (start-2 (org-journal-tag-reference-ref-start ref-2))
            (end-2 (org-journal-tag-reference-ref-end ref-2)))
        (cond
         ;; If one segment is nested in another, save the intersection
         ((or (org-journal-tags--nested-segment-p
               start-1 end-1 start-2 end-2)
              (org-journal-tags--nested-segment-p
               start-2 end-2 start-1 end-1))
          (setq result
                ;; Because there will be intersections otherwise
                (org-journal-tags--query-merge-refs-push
                 result
                 (org-journal-tag-reference--create
                  :time time :date date
                  :ref-start (max start-1 start-2)
                  :ref-end (min end-1 end-2)))))
         ;; start-1 <= start-2 <= end-1
         ;; The segment [start-2, end-1] is overlapping
         ((and (<= start-1 start-2) (<= start-2 end-1))
          (setq result
                (org-journal-tags--query-merge-refs-push
                 result
                 (org-journal-tag-reference--create
                  :ref-start start-2 :ref-end end-1
                  :date date :time time))))
         ;; start-2 <= start-1 <= end-2
         ;; The segment [start-1, end-2] is overlapping
         ((and (<= start-2 start-1) (<= start-1 end-2))
          (setq result
                (org-journal-tags--query-merge-refs-push
                 result
                 (org-journal-tag-reference--create
                  :ref-start start-1 :ref-end end-2
                  :date date :time time))))
         ;; Do nothing if there are no overlaps
         )))
    result))

(defun org-journal-tags--query-intersect-refs (refs-1 refs-2)
  "Return intersections between REFS-1 and REFS-2.

REFS-1 and REFS-2 are lists of `org-journal-tag-reference'."
  (let ((dates-hash-2 (org-journal-tags--query-construct-dates-hash refs-2)))
    (org-journal-tags--query-deconstruct-dates-hash
     (org-journal-tags--query-construct-dates-hash
      refs-1
      (lambda (time-refs-1 ref-1)
        (let* ((date (org-journal-tag-reference-date ref-1))
               (time (org-journal-tag-reference-time ref-1))
               (time-refs-2 (gethash time (gethash date dates-hash-2))))
          (append
           time-refs-1
           (org-journal-tags--query-intersect-to-one-ref
            time-refs-2
            ref-1))))
      (lambda (date-1 time-1)
        (when-let ((times-hash-2 (gethash date-1 dates-hash-2)))
          (gethash time-1 times-hash-2)))))))

(defun org-journal-tags--query-sort-refs (refs &optional ascending)
  "Sort REFS by date and time.

REFS is a list of `org-journal-tag-reference'.

If ASCENDING is non-nil, do sort dates in ascending
order (i.e. the earliest date comes first.).  Times are always
sorted ascending."
  (seq-sort
   (lambda (ref-1 ref-2)
     (let ((date-1 (org-journal-tag-reference-date ref-1))
           (date-2 (org-journal-tag-reference-date ref-2)))
       (if (= date-1 date-2)
           (string-lessp (org-journal-tag-reference-time ref-1)
                         (org-journal-tag-reference-time ref-2))
         (funcall (if ascending #'<= #'>=)
                  date-1 date-2))))
   refs))

(defun org-journal-tags--query-get-date-list (start-date end-date)
  "List all the dates for records.

As everywhere in org-journal-tags, dates are returned in the UNIX
timestamp format.

START-DATE and END-DATE are used to trim the range of the
returned dates from both ends."
  (thread-last
    (org-journal--list-dates)
    (mapcar (lambda (date)
              (time-convert
               (org-journal-tags--parse-journal-date date)
               'integer)))
    (seq-filter
     (lambda (date)
       (and (or (null start-date) (>= date start-date))
            (or (null end-date) (<= date end-date)))))))

(defun org-journal-tags--query-get-tags-references (tag-names dates)
  "Return all references to required tags from the db.

TAG-NAMES is a list of strings, DATES is a list of timestamps."
  (cl-loop for date in dates append
           (cl-loop for tag-name in tag-names
                    for tag = (gethash tag-name
                                       (alist-get :tags org-journal-tags-db))
                    append (gethash date (org-journal-tag-dates tag)))))

(defun org-journal-tags--query-get-child-tags (parent-tag)
  "Get child org-journal tags for PARENT-TAG.

A tag is considered to be a child of PARENT-TAG if it starts with
\"<parent-tag-name>.\".  PARENT-TAG itself is also returned."
  (cl-loop for tag being the hash-keys of (alist-get :tags org-journal-tags-db)
           if (string-match-p
               (rx bos (literal parent-tag) (or eos (: "." (* nonl))))
               tag)
           collect tag))

(defun org-journal-tags--query-get-tag-names (tag-names &optional children)
  "Filter and extend TAG-NAMES for the query.

TAG-NAMES is a list of strings or nil.  If it's nil, a list with
an empty string is returned, which is a root tag for every other
tag.

If CHIDLREN is non-nil, names of tags in TAG-NAMES and all their
CHILDREN are returned.  Otherwise, only tags in TAG-NAMES are
returned."
  (if tag-names
      (seq-uniq
       (cl-loop for tag-name in
                (cl-loop for tag-name in tag-names
                         unless children collect tag-name
                         if children append
                         (org-journal-tags--query-get-child-tags
                          tag-name))
                if (gethash tag-name
                            (alist-get :tags org-journal-tags-db))
                collect tag-name))
    '("")))

(defvar org-journal-tags--files-cache (make-hash-table :test #'equal)
  "A cache for org-journal files used to speed up queries.

Keys are filenames, values are the correspoinding buffer strings.")

(defun org-journal-tags--cache-invalidate (file-name)
  "Invalid file contents cache for FILE-NAME."
  (remhash file-name org-journal-tags--files-cache))

(defun org-journal-tags-cache-reset ()
  "Clear the org-journal-tags file contents cache."
  (interactive)
  (clrhash org-journal-tags--files-cache))

(defun org-journal-tags--extract-ref (ref)
  "Get the string referenced by the REF.

REF should be an instance of `org-journal-tag-reference'."
  (let ((file-name (org-journal--get-entry-path
                    (org-journal-tag-reference-date ref))))
    (unless (gethash file-name org-journal-tags--files-cache)
      (with-temp-buffer
        (message "Parsing: %s" file-name)
        (insert-file-contents file-name)
        (setq org-startup-indented nil)
        (let ((org-mode-hook nil))
          (org-mode))
        (org-journal-tags--ensure-decrypted)
        (org-font-lock-ensure)
        (puthash file-name (buffer-string)
                 org-journal-tags--files-cache)))
    (string-trim
     (substring
      (gethash file-name org-journal-tags--files-cache)
      (1- (org-journal-tag-reference-ref-start ref))
      (1- (org-journal-tag-reference-ref-end ref))))))

(defun org-journal-tags--string-match-indices (regex string)
  "Get indices of REGEX matches in STRING."
  (cl-loop for match = (string-match regex string
                                     (if match (1+ match) nil))
           if match collect match into result
           unless match return result))

(defun org-journal-tags--string-extract-refs (regex ref string)
  "Extract references from those paragraphs of REF that match REGEX.

STRING is a string, corresponding to REF.  It is split to
paragraphs by two newline symbols in a row."
  (let ((paragraphs
         `(0 ,@(org-journal-tags--string-match-indices "\n\n" string)
             ,(length string)))
        (matches (org-journal-tags--string-match-indices regex string)))
    (cl-loop for i from 0 to (1- (length paragraphs))
             for start in paragraphs
             for end = (nth (1+ i) paragraphs)
             if (cl-loop for match in matches
                         if (and (<= start match) (<= match end))
                         return t)
             collect (org-journal-tag-reference--create
                      :date (org-journal-tag-reference-date ref)
                      :time (org-journal-tag-reference-time ref)
                      :ref-start (+ (org-journal-tag-reference-ref-start ref) start)
                      :ref-end (+ (org-journal-tag-reference-ref-start ref) end)))))

(defun org-journal-tags--query-filter-refs-by-regex (refs regex &optional narrow)
  "Filter REFS by REGEX.

REFS is a list of `org-journal-tag-reference', REGEX is regular
expression.

If NARROW is non-nil, only one paragraph for every match will be
returned."
  (cl-loop for ref in refs
           for text = (org-journal-tags--extract-ref ref)
           if (string-match-p regex text)
           append (if (not narrow)
                      (list ref)
                    (org-journal-tags--string-extract-refs regex ref text))))

(cl-defun org-journal-tags-query (&key tag-names exclude-tag-names start-date
                                       end-date regex regex-narrow children order)
  "Query the org-journal-tags database.

All the keys are optional.

TAG-NAMES is a list of strings with tag names to include.
EXCLUDE-TAG-NAMES is a list of strings with tag names to exclude.

START-DATE and END-DATE are UNIX timestamps that set the search
boundaries.

REGEX is a regex by which the references will be filtered.  If
REGEX-NARROW is non-nil, each found reference will be narrowed
only to a particular paragraph where a match occurred.

If CHILDREN is non-nil, also search within all the children of TAG-NAMES.

If ORDER is 'ascending, the references list will be sorted in
ascending order.  If ORDER is anything else except nil, the order
will be descending.

The returned value is a list of `org-journal-tag-reference'."
  (org-journal-tags-db-ensure)
  (let ((dates (org-journal-tags--query-get-date-list start-date end-date))
        results)
    (setq results (org-journal-tags--query-get-tags-references
                   (org-journal-tags--query-get-tag-names tag-names children)
                   dates))
    (when exclude-tag-names
      (setq results
            (org-journal-tags--query-diff-refs
             results
             (org-journal-tags--query-get-tags-references
              (org-journal-tags--query-get-tag-names exclude-tag-names children)
              dates))))
    (when regex
      (setq results (org-journal-tags--query-filter-refs-by-regex
                     results regex regex-narrow)))
    (setq results (org-journal-tags--query-merge-refs results))
    (when order
      (setq results
            (org-journal-tags--query-sort-refs
             results (eq order 'ascending))))
    results))

(defun org-journal-tags--get-dates-list (refs)
  "Get the date list to group REFS."
  (let ((start-date (org-journal-tag-reference-date (nth 0 refs)))
        (end-date (org-journal-tag-reference-date (car (last refs)))))
    (when (> start-date end-date)
      (setq start-date end-date)
      (setq end-date (org-journal-tag-reference-date (nth 0 refs))))
    (seq-group-by
     org-journal-tags-date-group-func
     (cl-loop for date from start-date to end-date by (* 60 60 24)
              collect date))))

(defun org-journal-tags--group-refs-by-date (refs &optional max-length dates-list)
  "Group REFS by date.

REFS is a list of `org-journal-tag-reference'.

Grouping is done with `org-journal-tags-date-group-func'.  The
function should receive a date (in the form of timestamp) and
return a string name of the group.

MAX-LENGTH is the maximum number of groups.  The function merges
all odd groups with even until the total number of groups is less
than MAX-LENGTH.

DATES-LIST determines the used dates to group.  It is a list of
lists, in the nested list the first element is the group name and
the rest are date numbers.  Such a list is constructed by
`org-journal-tags--get-dates-list'."
  (let* ((dates-list (or dates-list (org-journal-tags--get-dates-list refs)))
         (dates-hash (make-hash-table :test #'equal)))
    (cl-loop for ref in refs
             for date-group = (funcall org-journal-tags-date-group-func
                                       (org-journal-tag-reference-date ref))
             do (puthash date-group
                         (push ref (gethash date-group dates-hash))
                         dates-hash))
    (let ((result (cl-loop for group in dates-list
                           for date-group = (car group)
                           for dates = (cdr group)
                           for refs = (gethash date-group dates-hash)
                           collect `(,date-group . ((:dates . ,dates) (:refs . ,refs))))))
      (while (and max-length (> (length result) max-length))
        (setq result
              (cl-loop for (a b) on result by #'cddr
                       collect
                       `(,(car a)
                         . ((:dates . ,(append (alist-get :dates (cdr a))
                                               (alist-get :dates (cdr b))))
                            (:refs . ,(append (alist-get :refs (cdr a))
                                              (alist-get :refs (cdr b)))))))))
      result)))

;; Status buffer

(defmacro org-journal-tags--with-close-status (&rest body)
  "Create an interactive lambda that closes the status buffer.

BODY is put in that lambda."
  `(lambda ()
     (interactive)
     (when (eq major-mode 'org-journal-tags-status-mode)
       (quit-window t))
     ,@body))

(defvar org-journal-tags-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "s") #'org-journal-tags-transient-query)
    (define-key map (kbd "n") (org-journal-tags--with-close-status
                               (call-interactively
                                #'org-journal-new-entry)))
    (define-key map (kbd "o") (org-journal-tags--with-close-status
                               (org-journal-open-current-journal-file)))
    (define-key map (kbd "?") #'org-journal-tags--status-transient-help)
    (when (fboundp #'evil-define-key*)
      (evil-define-key* 'normal map
        (kbd "<tab>") #'magit-section-toggle
        "s" #'org-journal-tags-transient-query
        "n" (org-journal-tags--with-close-status
             (call-interactively
              #'org-journal-new-entry))
        "o" (org-journal-tags--with-close-status
             (org-journal-open-current-journal-file))
        "?" #'org-journal-tags--status-transient-help
        "q" (lambda ()
              (interactive)
              (quit-window t))))
    map)
  "A keymap for `org-journal-tags-status-mode'.")

(transient-define-prefix org-journal-tags--status-transient-help ()
  "Commands in the status buffer."
  ["Section commands"
   ("<tab>" "Toggle section" magit-section-toggle)
   ("M-1" "Show level 1" magit-section-show-level-1-all)
   ("M-2" "Show level 2" magit-section-show-level-2-all)]
  ["Org Journal"
   ("s" "New query" org-journal-tags-transient-query)
   ("n" "New journal entry" (lambda nil
                              (interactive)
                              (when (eq major-mode 'org-journal-tags-status-mode)
                                (quit-window t))
                              (call-interactively #'org-journal-new-entry)))
   ("o" "Current journal entry" (lambda ()
                                  (interactive)
                                  (when (eq major-mode 'org-journal-tags-status-mode)
                                    (quit-window t))
                                  (org-journal-open-current-journal-file)))
   ("q" "Quit" transient-quit-one)])

(define-derived-mode org-journal-tags-status-mode magit-section "Org Journal Tags"
  "A major mode to display the org-journal-tags status buffer.")

(defun org-journal-tags--buffer-render-info ()
  "Render the miscellaneous information for the status buffer."
  (let ((dates (org-journal--list-dates)))
    (insert (format "Last record:   %s\n"
                    (propertize (thread-last
                                  (last dates)
                                  car
                                  org-journal-tags--parse-journal-date
                                  (format-time-string org-journal-date-format))
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

(defun org-journal-tags--get-all-tag-references (tag-name)
  "Extract all references to TAG-NAME from the database."
  (when (gethash tag-name (alist-get :tags org-journal-tags-db))
    (cl-loop for refs being the hash-values of
             (org-journal-tag-dates
              (gethash tag-name (alist-get :tags org-journal-tags-db)))
             append refs)))

(defun org-journal-tags--buffer-render-tag-buttons ()
  "Render tag buttons for the org-journal-tags status buffer.

This function creates a button and a horizontal barchart for each
tag."
  (when-let* ((tag-names (seq-sort #'string-lessp (org-journal-tags--list-tags)))
              (dates-list (org-journal-tags--get-dates-list
                           (org-journal-tags--query-sort-refs
                            (org-journal-tags--get-all-tag-references ""))))
              (max-tag-name (seq-max (mapcar #'length tag-names)))
              (widget-push-button-prefix "")
              (widget-push-button-suffix ""))
    ;; XXX Silencing the byte-compliation warnings.  These two
    ;; wariables change the behavior of `widget-create'.
    (ignore widget-push-button-prefix)
    (ignore widget-push-button-suffix)
    (dolist (tag-name tag-names)
      (widget-create 'push-button
                     :notify (lambda (widget &rest _)
                               (let ((org-journal-tags--query-params
                                      `((:tag-names
                                         . (,(widget-get widget :tag-name))))))
                                 (ignore org-journal-tags--query-params)
                                 (org-journal-tags-transient-query)))
                     :tag-name tag-name
                     (string-pad tag-name max-tag-name))
      (widget-insert " ")
      (org-journal-tags--buffer-render-horizontal-barchart
       (mapcar
        (lambda (group) (length (alist-get :refs (cdr group))))
        (org-journal-tags--buffer-get-barchart-data
         (org-journal-tags--get-all-tag-references tag-name)
         (- (window-body-width) max-tag-name 2)
         dates-list)))
      (widget-insert "\n"))))

(defun org-journal-tags--buffer-render-contents ()
  "Render the contents of the org-journal-tags status buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (org-journal-tags-status-mode)
    (magit-insert-section (org-journal-tags-info)
      (magit-insert-section (org-journal-tags)
        (insert (format "Date:          %s\n"
                        (propertize (format-time-string org-journal-date-format)
                                    'face 'org-journal-tags-info-face)))
        (magit-insert-heading)
        (org-journal-tags--buffer-render-info))
      (insert "\n")
      (magit-insert-section (org-journal-tags)
        (insert (propertize "All tags" 'face 'magit-section-heading))
        (magit-insert-heading)
        (org-journal-tags--buffer-render-tag-buttons))))
  (goto-char (point-min)))

;;;###autoload
(defun org-journal-tags-status ()
  "Open org-journal-tags status buffer."
  (interactive)
  (org-journal-tags-db-ensure)
  (when org-journal-tags-autosync-mode
    (org-journal-tags-db-sync))
  (when-let ((buffer (get-buffer org-journal-tags-status-buffer-name)))
    (kill-buffer buffer))
  (let ((buffer (get-buffer-create org-journal-tags-status-buffer-name)))
    (with-current-buffer buffer
      (org-journal-tags--buffer-render-contents))
    (switch-to-buffer-other-window buffer)))


;; Barcharts

(defun org-journal-tags--buffer-get-barchart-data (refs &optional max-length dates-list)
  "Group REFS to data series for a barchart.

REFS is a list of `org-journal-tag-reference'.  MAX-LENGTH is the
maximum length of the barchart.  It nil, (1- (windows-body-width))
is taken.

DATES-LIST is a list of grouped dates as described in
`org-journal-tags--group-refs-by-date'."
  (when refs
    (org-journal-tags--group-refs-by-date
     refs (or max-length (1- (window-body-width))) dates-list)))

(defun org-journal-tags--buffer-render-horizontal-barchart (data &optional max-height)
  "Render a horizontal barchart for DATA at the point.

DATA is a list of numbers.  0 will be rendered as the first
symbol in `org-journal-tags-barchart-symbols', the maximum number
will be rendered as the last symbol.

The maximum number can be overridden with MAX-HEIGHT if it's
necessary to synchronize the height of multiple barcharts."
  (let* ((max-datum (or max-height (max 1 (seq-max data))))
         (max-symbol-index (1- (length org-journal-tags-barchart-symbols))))
    (insert
     (propertize
      (cl-loop for datum in data
               for symbol-index = (ceiling (* datum
                                              (/ (float max-symbol-index) max-datum)))
               concat (nth symbol-index org-journal-tags-barchart-symbols))
      'face 'org-journal-tags-barchart-face))))

(defun org-journal-tags--buffer-render-vertical-barchart (groups &optional max-width)
  "Render a vertical barchart for GROUPS at the point.

GROUPS is an output of `org-journal-tags--group-refs-by-date'.
This function plots a bar for the count of references in each
group.

MAX-WIDTH overrides the maximum number of references per group.
That can be used to scale multiple barcharts the same way."
  (insert
   (cl-loop with max-name-width = (seq-max (mapcar (lambda (group)
                                                     (length (car group)))
                                                   groups))
            with max-group-width = (or max-width
                                       (thread-last
                                         groups
                                         (mapcar
                                          (lambda (group)
                                            (length (alist-get :refs (cdr group)))))
                                         seq-max))
            with max-space-for-group = (- (window-body-width) 6
                                          max-name-width)
            with width-coef = (if (< max-space-for-group max-group-width)
                                  (/ max-group-width max-space-for-group)
                                1)
            for group in groups
            for number = (length (alist-get :refs (cdr group)))
            for ticks-number = (floor (* number width-coef))
            concat (concat
                    (propertize (string-pad (car group) max-name-width)
                                'face 'org-journal-tags-info-face)
                    " "
                    (string-pad
                     (format "[%d]" number)
                     4)
                    ": "
                    (propertize (string-pad "" ticks-number ?+)
                                'face 'org-journal-tags-barchart-face)
                    "\n"))))

;; Query buffer

(defvar org-journal-tags-query-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "<RET>") #'org-journal-tags--buffer-visit-thing-at-point)
    (define-key map (kbd "s") #'org-journal-tags-transient-query)
    (define-key map (kbd "r") #'org-journal-tags--query-refresh)
    (when (fboundp #'evil-define-key*)
      (evil-define-key* '(normal motion) map
        (kbd "<tab>") #'magit-section-toggle
        (kbd "<RET>") #'org-journal-tags--buffer-visit-thing-at-point
        "r" #'org-journal-tags--query-refresh
        "s" #'org-journal-tags-transient-query
        "?" #'org-journal-tags--query-transient-help
        "q" (lambda ()
              (interactive)
              (quit-window t))))
    map)
  "A keymap for `org-journal-tags-query-mode'.")

(transient-define-prefix org-journal-tags--query-transient-help ()
  "Commands in the query results buffer."
  ["Section commands"
   ("<tab>" "Toggle section" magit-section-toggle)
   ("M-1" "Show level 1" magit-section-show-level-1-all)
   ("M-2" "Show level 2" magit-section-show-level-2-all)]
  ["General commands"
   ("s" "Update the query" org-journal-tags-transient-query)
   ("r" "Refresh buffer" org-journal-tags--query-refresh)
   ("<RET>" "Visit thing at point" org-journal-tags--buffer-visit-thing-at-point)
   ("q" "Quit" transient-quit-one)])

(define-derived-mode org-journal-tags-query-mode magit-section "Org Journal Tags Query"
  "A major mode to display results of org-journal-tags quieries."
  :group 'org-journal-tags
  (setq-local buffer-read-only t))

(defclass org-journal-tags-date-section (magit-section)
  ((date :initform nil)))

(defclass org-journal-tags-time-section (magit-section)
  ((ref :initform nil)))

(defun org-journal-tags--goto-date (date)
  "Open the org-journal file corresponding to DATE.

DATE is a UNIX timestamp."
  (let ((org-journal-file (thread-last
                            date
                            seconds-to-time
                            org-journal--get-entry-path)))
    (unless (file-exists-p org-journal-file)
      (user-error "Journal file %s not found" org-journal-file))
    (progn
      (funcall org-journal-find-file org-journal-file)
      (unless (org-journal--daily-p)
        (thread-last date
                     seconds-to-time
                     decode-time
                     ;; XXX It is possible to pass a lambda as a
                     ;; single form in the threading macro, but
                     ;; somehow it's deprecated by the byte compiler
                     (funcall (lambda (time) (list (nth 4 time)
                                                   (nth 3 time)
                                                   (nth 5 time))))
                     org-journal--goto-entry)))))

(defun org-journal-tags--goto-ref (ref)
  "Open the org-jounral file corresponding to REF.

REF is an instance `org-journal-tag-reference'."
  (let ((org-journal-file (thread-last
                            ref
                            org-journal-tag-reference-date
                            seconds-to-time
                            org-journal--get-entry-path)))
    (unless (file-exists-p org-journal-file)
      (user-error "Journal file %s not found" org-journal-file))
    (progn
      (funcall org-journal-find-file org-journal-file)
      (org-journal-tags--ensure-decrypted)
      (goto-char (org-journal-tag-reference-ref-start ref)))))

(defun org-journal-tags--buffer-visit-thing-at-point ()
  "Open thing at point in the org-journal-tags query buffer."
  (interactive)
  (let ((section (magit-current-section)))
    (cond
     ((and (slot-exists-p section 'ref)
           (slot-boundp section 'ref))
      (org-journal-tags--goto-ref (oref section ref)))
     ((and (slot-exists-p section 'date)
           (slot-boundp section 'date))
      (org-journal-tags--goto-date (oref section date)))
     (t (user-error "Nothing to visit at point")))))

(defun org-journal-tags--buffer-render-query (refs)
  "Render the contents of the org-journal-tags query buffer.

REFS is a list org `org-journal-tag-reference'."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq-local org-journal-tags--query-refs refs)
    (unless (eq major-mode 'org-journal-tags-query-mode)
      (org-journal-tags-query-mode))
    (magit-insert-section (org-journal-tags-query)
      (insert "Found results: "
              (propertize (number-to-string (length refs))
                          'face 'org-journal-tags-info-face)
              "\n")
      (when-let ((groups (org-journal-tags--buffer-get-barchart-data refs)))
        (magit-insert-section (org-journal-tags-query-barchart nil t)
          (org-journal-tags--buffer-render-horizontal-barchart
           (mapcar
            (lambda (group) (length (alist-get :refs (cdr group))))
            groups))
          (insert "\n")
          (magit-insert-heading)
          (org-journal-tags--buffer-render-vertical-barchart groups)
          (insert "\n")))
      (dolist (date-refs
               (seq-group-by
                #'org-journal-tag-reference-date
                refs))
        (magit-insert-section section (org-journal-tags-date-section)
          (thread-last date-refs
                       car
                       seconds-to-time
                       (format-time-string org-journal-date-format)
                       (format "%s\n")
                       (funcall
                        (lambda (s)
                          (propertize s 'face 'org-journal-tags-date-header)))
                       insert)
          (oset section date (car date-refs))
          (magit-insert-heading)
          (dolist (ref (cdr date-refs))
            (magit-insert-section section (org-journal-tags-time-section nil t)
              (thread-last
                ref
                org-journal-tag-reference-time
                (format "%s\n")
                (funcall
                 (lambda (s)
                   (propertize s 'face 'org-journal-tags-time-header)))
                insert)
              (oset section ref ref)
              (magit-insert-heading)
              (insert (org-journal-tags--extract-ref ref))
              (insert "\n"))))))
    (goto-char (point-min))))

(defun org-journal-tags--query-refresh ()
  "Refresh the current org-journal-tags query buffer."
  (interactive)
  (org-journal-tags--buffer-render-query
   org-journal-tags--query-refs))


;; Query transient

(defclass org-journal-tags--transient-variable (transient-variable)
  ((transient :initform 'transient--do-call))
  "A base class for settings in the query buffer.

The name of the variable corresponds to the key in
`org-journal-tags-query'.  Values from the transient suffixes are
extracted with `org-journal-tags--transient-extract-values',
converted to a proper plist with
`org-journal-tags--transient-values-to-params' and fed to
`org-journal-tags-query'.

The starting values can be get from the
`org-journal-tags--query-params' variable, which can be made
buffer-local in certain org-journal-tags buffer or overridden with
lexical binding.")

(defclass org-journal-tags--transient-switch-with-variable (transient-switch)
  ((variable :initarg :variable)))

(cl-defmethod transient-init-value ((obj org-journal-tags--transient-variable))
  "Initialize the starting value for the infix.

OBJ is an instance of the `org-journal-tags--transient-variable' class."
  (if (bound-and-true-p org-journal-tags--query-params)
      (oset obj value
            (alist-get (oref obj variable) org-journal-tags--query-params))
    (oset obj value nil)))

(cl-defmethod transient-init-value ((obj org-journal-tags--transient-switch-with-variable))
  "Initialize the starting value for the infix.

OBJ is an instance of the `org-journal-tags--transient-variable' class."
  (if (bound-and-true-p org-journal-tags--query-params)
      (oset obj value
            (alist-get (oref obj variable) org-journal-tags--query-params))
    (oset obj value nil)))

(cl-defmethod transient-infix-value ((obj org-journal-tags--transient-variable))
  "Return the value for the infix.

OBJ is an instance of the `org-journal-tags--transient-variable' class."
  (slot-value obj 'value))

(cl-defmethod transient-format-value ((obj org-journal-tags--transient-variable))
  "A value formatter with `prin1-to-string'.

OBJ is an instance of the `org-journal-tags--transient-variable' class."
  (let ((value (if (slot-boundp obj 'value) (slot-value obj 'value) nil)))
    (if value
        (propertize
         (prin1-to-string value)
         'face 'transient-value)
      (propertize "unset" 'face 'transient-inactive-value))))

(defclass org-journal-tags--transient-tags (org-journal-tags--transient-variable)
  ((reader :initform #'org-journal-tags--transient-tags-reader)))

(defun org-journal-tags--transient-tags-reader (prompt initial-input _history)
  "Read tags for the `org-journal-tags--transient-tags' class.

PROMPT is a string to prompt with.  INITIAL-INPUT is the initial
state of the minibuffer."
  (let ((crm-separator " ")
        (crm-local-completion-map
         (let ((map (make-sparse-keymap)))
           (set-keymap-parent map crm-local-completion-map)
           (define-key map " " 'self-insert-command)
           map)))
    (completing-read-multiple
     prompt (org-journal-tags--list-tags) nil nil initial-input)))

(cl-defmethod transient-format-value ((obj org-journal-tags--transient-tags))
  "Format value for the `org-journal-tags--transient-tags' class.

OBJ in an instance of that class."
  (let ((value (if (slot-boundp obj 'value) (slot-value obj 'value) nil)))
    (if value
        (propertize
         (string-join value " ")
         'face 'transient-value)
      (propertize "unset" 'face 'transient-inactive-value))))

(defclass org-journal-tags--transient-date (org-journal-tags--transient-variable)
  ((reader :initform #'org-journal-tags--transient-date-reader)))

(defun org-journal-tags--transient-date-reader (prompt _initial-input _history)
  "Read the date with `org-read-date'.

PROMPT is a string to prompt with.

Returns a UNIX timestamp."
  (time-convert
   (org-read-date nil t nil prompt)
   'integer))

(cl-defmethod transient-format-value ((obj org-journal-tags--transient-date))
  "Format value for the `org-journal-tags--transient-date' class.

OBJ in an instance of that class."
  (let ((value (if (slot-boundp obj 'value) (slot-value obj 'value) nil)))
    (if value
        (propertize
         (format-time-string
          org-journal-date-format
          (seconds-to-time
           value))
         'face 'transient-value)
      (propertize "unset" 'face 'transient-inactive-value))))

(defclass org-journal-tags--transient-regex (org-journal-tags--transient-variable)
  ((reader :initform #'org-journal-tags--transient-regex-reader)))

(defun org-journal-tags--transient-regex-reader (prompt initial-input _history)
  "Read a regular expression from minibuffer.

Used in the `org-journal-tags--transient-regex' class.  PROMPT is
a string to prompt with, INITIAL-INPUT is the default state of
the minibuffer."
  (read-from-minibuffer prompt initial-input))

(cl-defmethod transient-format-value ((obj org-journal-tags--transient-regex))
  "Format value of `org-journal-tags--transient-regex'.

OBJ is an instance of that class."
  (let ((value (if (slot-boundp obj 'value) (slot-value obj 'value) nil)))
    (if (stringp value)
        (propertize
         value
         'face 'transient-value)
      (propertize "unset" 'face 'transient-inactive-value))))

(transient-define-infix org-journal-tags--transient-include-tags ()
  :class 'org-journal-tags--transient-tags
  :variable :tag-names
  :description "Include tags"
  :prompt "Include tag names: ")

(transient-define-infix org-journal-tags--transient-exclude-tags ()
  :class 'org-journal-tags--transient-tags
  :variable :exclude-tag-names
  :description "Exclude tags"
  :prompt "Exclude tag names: ")

(transient-define-infix org-journal-tags--transient-children ()
  :class 'org-journal-tags--transient-switch-with-variable
  :description "Include child tags"
  :argument "--children"
  :variable :children)

(transient-define-infix org-journal-tags--transient-start-date ()
  :class 'org-journal-tags--transient-date
  :variable :start-date
  :description "Start date"
  :prompt "Start date: ")

(transient-define-infix org-journal-tags--transient-end-date ()
  :class 'org-journal-tags--transient-date
  :variable :end-date
  :description "End date"
  :prompt "End date: ")

(transient-define-infix org-journal-tags--transient-regex-search ()
  :class 'org-journal-tags--transient-regex
  :variable :regex
  :description "Regex"
  :prompt "Search by regular expression: ")

(transient-define-infix org-journal-tags--transient-regex-narrow ()
  :class 'org-journal-tags--transient-switch-with-variable
  :description "Narrow to regex"
  :argument "--regex-narrow"
  :variable :regex-narrow)

(transient-define-infix org-journal-tags--transient-order ()
  :class 'org-journal-tags--transient-switch-with-variable
  :description "Sort"
  :argument "--ascending"
  :variable :order)

(defun org-journal-tags--transient-extract-values ()
  "Return (variable . value) alist for the current transient."
  (cl-loop for suffix in (transient-suffixes transient-current-command)
           if (and (slot-exists-p suffix 'variable) (slot-exists-p suffix 'value))
           collect (cons (slot-value suffix 'variable) (slot-value suffix 'value))))

(defun org-journal-tags--transient-values-to-params (values)
  "Make a plist acceptable to `org-journal-tags-query'.

VALUES should be an alist of transient values."
  (let ((params (cl--alist-to-plist values)))
    (setq params
          (if (plist-get params :order)
              (plist-put params :order 'ascending)
            (plist-put params :order 'descending)))))

(defmacro org-journal-tags--render-query-refs (&rest body)
  "Process the query results before rendering them in the buffer.

The macro runs a query for the parameters extracted from the
current transient, put results into the RES variable, and makes in
available to the BODY, which can process the variable however necessary."
  `(let* ((params (org-journal-tags--transient-extract-values))
          (refs (apply #'org-journal-tags-query
                       (org-journal-tags--transient-values-to-params params))))
     (with-current-buffer (get-buffer-create org-journal-tags-query-buffer-name)
       (org-journal-tags--buffer-render-query
        (progn
          ,@body))
       (setq-local org-journal-tags--query-params params))
     (unless (string-equal (buffer-name (current-buffer))
                           org-journal-tags-query-buffer-name)
       (switch-to-buffer-other-window org-journal-tags-query-buffer-name))))

(transient-define-suffix org-journal-tags--transient-exec-new-query ()
  :description "Run query"
  (interactive)
  (when (eq major-mode 'org-journal-tags-status-mode)
    (quit-window t))
  (org-journal-tags--render-query-refs refs))

(transient-define-suffix org-journal-tags--transient-query-intersection ()
  :description "Intersection"
  (interactive)
  (org-journal-tags--render-query-refs
   (org-journal-tags--query-intersect-refs
    org-journal-tags--query-refs
    refs)))

(transient-define-suffix org-journal-tags--transient-query-union ()
  :description "Union"
  (interactive)
  (org-journal-tags--render-query-refs
   (org-journal-tags--query-union-refs
    org-journal-tags--query-refs
    refs)))

(transient-define-suffix org-journal-tags--transient-query-diff-from ()
  :description "Difference from current"
  (interactive)
  (org-journal-tags--render-query-refs
   (org-journal-tags--query-diff-refs
    org-journal-tags--query-refs
    refs)))

(transient-define-suffix org-journal-tags--transient-query-diff-to ()
  :description "Difference to current"
  (interactive)
  (org-journal-tags--render-query-refs
   (org-journal-tags--query-diff-refs
    refs
    org-journal-tags--query-refs)))

(transient-define-suffix org-journal-tags--transient-reset ()
  :description "Reset query"
  :transient t
  (interactive)
  (cl-loop for suffix in (transient-suffixes transient-current-command)
           if (slot-exists-p suffix 'value)
           collect (oset suffix value nil)))

(transient-define-prefix org-journal-tags-transient-query ()
  "Query Org Journal."
  ["Tags"
   ("ti" org-journal-tags--transient-include-tags)
   ("te" org-journal-tags--transient-exclude-tags)
   ("tc" org-journal-tags--transient-children)]
  ["Date"
   ("ds" org-journal-tags--transient-start-date)
   ("de" org-journal-tags--transient-end-date)]
  ["Regex"
   ("rr" org-journal-tags--transient-regex-search)
   ("rn" org-journal-tags--transient-regex-narrow)]
  ["Order"
   ("o" org-journal-tags--transient-order)]
  ["Modify the current results"
   :class transient-row
   :if-mode org-journal-tags-query-mode
   ("mu" org-journal-tags--transient-query-union)
   ("mi" org-journal-tags--transient-query-intersection)
   ("md" org-journal-tags--transient-query-diff-from)
   ("mt" org-journal-tags--transient-query-diff-to)]
  ["Actions"
   :class transient-row
   ("e" org-journal-tags--transient-exec-new-query)
   ("<RET>" org-journal-tags--transient-exec-new-query)
   ("Q" org-journal-tags--transient-reset)
   ("q" "Quit" transient-quit-one)])

(provide 'org-journal-tags)
;;; org-journal-tags.el ends here
