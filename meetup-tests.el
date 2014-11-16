
;;(meetup/get-icalendar 
;; "https://www.google.com/calendar/ical/ferriernic%40gmail.com/public/basic.ics")

(defmacro comment (&rest stuff))

;; here's a matcher that finds the unique ids and then pulls the start and end markers
;;
;; we could use this to delete old ones that had gone

(defun meetup/org-elements ()
  (with-current-buffer "meetup-diary.org"
    (cddr (org-element-parse-buffer))))

(defun meetup/org-elements-dummy-data ()
  "Can replace `meetup/org-elements' with fake data."
  (let* ((child (list 'headline '(:raw-value "event_216111812@meetup.com" :tags ("uid"))))
         (parent (list 'headline '(:raw-value "blah" :begin 1 :end 10) child))
         (full-child (append (cadr child) (list :parent parent))))
    (setf (cadr child) full-child)
    parent))


;;(meetup/org-elements-find-pos "event_214526092@meetup.com")
