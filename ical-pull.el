;;; ical-pull.el --- pull ical feeds into org-agenda                 -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: calendar
;; Version: 0.0.1
;; Package-requires: ((shadchen "1.2")(dash "2.9.0")(s "1.9.0")(noflet "0.0.14"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Sync ical calendars with Org Agenda.

;;; Code:

(require 'web)
(require 'shadchen)
(require 'dash)
(require 'icalendar)
(require 'noflet)
(require 's)

(defgroup ical-pull nil
  "Pull ical feeds into org-agenda.")

(defcustom ical-pull-list nil
  "A list of urls to pull into org-agenda."
  :type '(repeat string)
  :group 'ical-pull)

(defun ical-pull/parse-ical-buffer  (buffer)
  (save-excursion
    (let (ical-contents ical-errors)
      (goto-char (point-min))
      (when (re-search-forward "^BEGIN:VCALENDAR\\s-*$" nil t)
        (beginning-of-line)
        (icalendar--read-element nil nil)))))

(defun ical-pull/ical-buffer->list (ical-buffer)
  (match
      (ical-pull/parse-ical-buffer ical-buffer)
    ((list
      (list
       'VCALENDAR _
       ical-top-alist
       (funcall (lambda (l) (--filter (eq (car it) 'VEVENT) l))
                events)))
     events)))

(defun ical-pull/parse-ical (ical-buffer)
  "Return nil or the list of VEVENTS in ICAL-BUFFER.

Each VEVENT is returned as a list of: 

  uid, event start time, summary, url

The event start time is a string of YYYYmmDDHHMMSS.  The uid may
be any unique identifier.  The summary and url are both
un-escaped."
  (save-excursion
    ;; Return the list of items
    (--map
     (match it
       ((list
         'VEVENT _
         (alist 'DTSTART (list (list 'TZID _) tstart)
                'SUMMARY (list _ title)
                'URL (list _ url)
                'UID (list _ uid)) _)
        (list uid tstart
              (icalendar--convert-string-for-import title)
              url))
       ((list
         'VEVENT _
         (alist 'DTSTART (list (list 'TZID _) tstart)
                'SUMMARY (list _ title)
                'UID (list _ uid)) _)
        (list uid tstart
              (icalendar--convert-string-for-import title)))
       ((list
         'VEVENT _
         (alist 'DTSTART (list _ tstart) ; sometimes there's no TZ
                'SUMMARY (list _ title)
                'UID (list _ uid)) _)
        (list uid tstart
              (icalendar--convert-string-for-import title))))
     (ical-pull/ical-buffer->list ical-buffer))))

(defun ical-pull/yyymmdd->org-date (yyyymmdd)
  ;; currying function
  (noflet ((@ (cmp num) (lambda (x) (funcall cmp (string-to-int x) num))))
    (save-match-data
      (string-match
       (rx (group-n 1 (= 4 (any "0-9")))
           (group-n 2 (= 2 (any "0-9")))
           (group-n 3 (= 2 (any "0-9")))
           (* (and "T"
                   (group-n 4 (= 2 (any "0-9")))
                   (group-n 5 (= 2 (any "0-9")))
                   (* (group-n 6 (= 2 (any "0-9")))))))
       yyyymmdd)
      (match
       (--map (match-string it yyyymmdd) (number-sequence 1 6))
       ((list year (? (@ '<= 12) month) (? (@ '< 32) day)
              (? (@ '<= 23) hour) (? (@ '<= 59) minute) (? (@ '<= 59) second))
        (format "<%s-%s-%s> " year month day))
       ((list year (? (@ '<= 12) month) (? (@ '< 32) day)
              (? (@ '<= 23) hour) (? (@ '<= 59) minute))
        (format "<%s-%s-%s> " year month day))
       ((list year (? (@ '<= 12) month) (? (@ '< 32) day))
        (format "<%s-%s-%s> " year month day))))))

;; Nearly as good as tail-calling, quick-exiting.
(defun ical-pull/org-elements-find-pos (uid elements)
  "Find the start and end of the org tree with UID.

As long as UID is a headline with a tag \"uid\" in an org tree
we'll find it and be able to edit it."
  (catch :value
    (cl-labels
        ((node (tree)
               (match tree
                 ((list
                   (list*
                    'headline
                    (plist :raw-value (? (apply-partially 'equal uid) _)
                           :tags (? (apply-partially 'member "uid") _)
                           :parent (list* 'headline
                                          (plist 
                                           :begin begin
                                           :end end) _)) _))
                  (throw :value (list begin end)))
                 ((list* (list* 'headline attrs children) after)
                  (node children)
                  (node after))
                 ((list (list* 'headline attrs children))
                  (node children))
                 (_ nil))))
      (node elements))))

(defun ical-pull/ical-insert-linked (start uid summary url)
  (insert
   (s-format
    "* ${start} [[${url}][${summary}]]\n** ${uid} :uid:\n"
    'aget
    `(("start" . ,(ical-pull/yyymmdd->org-date start))
      ("uid" . ,uid)
      ("url" . ,url)
      ("summary" . ,summary)))))

(defun ical-pull/ical-insert (start uid summary)
  (insert
   (s-format
    "* ${start} ${summary}\n** ${uid} :uid:\n"
    'aget
    `(("start" . ,(ical-pull/yyymmdd->org-date start))
      ("uid" . ,uid)
      ("summary" . ,summary)))))

(defun ical-pull/ical-position-for-uid (uid elements)
  (match (ical-pull/org-elements-find-pos uid elements)
    ((list begin end) (delete-region begin end)(goto-char begin))
    (nil (goto-char (point-max))))) 

(defun ical-pull/ical-to-org (ical-list)
  "Take ICAL-LIST and make org agenda entries.

ICAL-LIST is as generated by `ical-pull/ical-parse'."
  (with-current-buffer
      (find-file-noselect
       (expand-file-name "~/work/org/ical-pull-diary.org"))
    (let ((elements (cddr (org-element-parse-buffer))))
      ;;(erase-buffer)
      (goto-char (point-min))
      (--map
       (match it
         ((list uid start summary url)
          (ical-pull/ical-position-for-uid uid elements)
          (ical-pull/ical-insert-linked start uid summary url))
         ((list uid start summary)
          (ical-pull/ical-position-for-uid uid elements)
          (ical-pull/ical-insert start uid summary)))
       ical-list))
    (save-buffer)
    (org-agenda-list)))

(defmacro comment (&rest stuff))

(defun ical-pull/get-icalendar (url)
  "Pull the calendar from the web for `org-agenda'.

The URL is retrieved and converted from icalendar into native
org-agenda format and the file is saved."
  (web-http-get
   (lambda (con hdr data)
     (if (equal "200" (gethash 'status-code hdr))
         (with-temp-buffer
           (insert data)
           (delete-trailing-whitespace (point-min) (point-max))
           (ical-pull/ical-to-org (ical-pull/parse-ical (current-buffer)))
           ;; Handy error handling for debugging
           (comment
            (condition-case err
                (ical-pull/ical-to-org (ical-pull/parse-ical (current-buffer)))
              (error
               (progn
                 (message "%S" err)
                 (let ((buffer-contents (buffer-string)))
                   (with-current-buffer (get-buffer-create "*ical-pull-problem*")
                     (erase-buffer)
                     (insert buffer-contents)
                     (pop-to-buffer (current-buffer)))))))))
         ;; Else it's not a 200
         (message "ical-pull found %s has status-code: %s" url (gethash 'status-code hdr))))
   :url url))

;;;###autoload
(defun icall-pull-calendar ()
  (interactive)
  (--each
      ical-pull-list
      (ical-pull/get-icalendar it)))

(provide 'meetup)

;;; meetup.el ends here
