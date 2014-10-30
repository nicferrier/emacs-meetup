;;; meetup.el --- meetup integration                 -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: calendar

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

;; Sync meetup.com with org-mode.

;;; Code:

(require 'web)
(require 'shadchen)
(require 'dash)
(require 'icalendar)
(require 'noflet)


(defconst meetup-icalendar-url ; probably this whole thing can get more generic
  "http://www.meetup.com/events/ical/28515762/bb5fded36fe53b5e92bde2fa5c91a94e379060df/going"
  "The URL of the ICalendar Meetup meetings.

This is quite difficult to find in Meetup.com. 

First login.
Then go to calendar view.
The underneath the calendar there is an export to.

That is where you find the icalendar url.")

(defconst meetup-org-file ; to custom
  "~/work/org/meetup-diary.org"
  "The org-file where we save the meetup events.")


;; genericizing this - probably urls and files to save them in can be tied together



(defun meetup/parse-ical (ical-buffer)
  "Return nil or the list of VEVENTS in ICAL-BUFFER.

Each VEVENT is returned as a list of: 

  uid, event start time, summary, url

The event start time is a string of YYYYmmDDHHMMSS.  The uid may
be any unique identifier.  The summary and url are both
un-escaped."
  (when (re-search-forward "^BEGIN:VCALENDAR\\s-*$" nil t)
    (let (ical-contents ical-errors)
      ;; read ical
      (beginning-of-line)
      ;; Return the list of items
      (--map
       (match
        it
        ((list
          'VEVENT _
          (alist 'DTSTART (list (list 'TZID _) tstart)
                 'SUMMARY (list _ title)
                 'URL (list _ url)
                 'UID (list _ uid)) _)
         (list uid tstart
               (icalendar--convert-string-for-import title)
               url)))
       (match
        (icalendar--read-element nil nil)
        ((list
          (list
           'VCALENDAR _
           ical-top-alist
           (funcall (lambda (l) (--filter (eq (car it) 'VEVENT) l))
                    events))) events))))))

(defun meetup/yyymmdd->org-date (yyyymmdd)
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

(defun meetup/ical-to-org (ical-list)
  "Take ICAL-LIST and make org agenda entries.

ICAL-LIST is as generated by `meetup/ical-parse'."
  (with-current-buffer
      (find-file-noselect
       (expand-file-name "~/work/org/meetup-diary.org"))
    (erase-buffer)
    (goto-char (point-min))
    (--map
     (match
      it
      ((list uid start summary url)
       (insert
        (s-format
         "* ${start} [[${url}][${summary}]]\n** ${uid}\n"
         'aget
         `(("start" . ,(meetup/yyymmdd->org-date start))
           ("uid" . ,uid)
           ("url" . ,url)
           ("summary" . ,summary)))))) ical-list)
    (save-buffer)
    (org-agenda-list)))

(defun meetup/get-icalendar ()
  "Pull the meetup calendar from the web for `org-agenda'.

The URL is retrieved and converted from icalendar into native
org-agenda format and the file is saved."
  (web-http-get
   (lambda (con hdr data)
     (with-temp-buffer
       (insert data)
       (delete-trailing-whitespace (point-min) (point-max))
       (goto-char (point-min))
       (meetup/ical-to-org
        (meetup/parse-ical (current-buffer)))))
   :url meetup-icalendar-url))

(defun meetup-sync-calendar ()
  (interactive)
  (meetup/get-icalendar))



(provide 'meetup)

;;; meetup.el ends here
