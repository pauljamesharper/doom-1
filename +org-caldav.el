;;; ~/.doom.d/+org-caldav.el -*- lexical-binding: t; -*-

(use-package! org-caldav
  :after org
  :init
  (setq org-caldav-url "https://dav.mailbox.org/caldav"
        org-caldav-calendar-id "Y2FsOi8vMC80NQ"
        org-caldav-inbox "~/org/.calendar-inbox.org"
        org-caldav-files '("~/org/calendar.org"
                         "~/org/actions.org"
                         "~/org/someday.org"))
  :config
  (setq org-icalendar-timezone "Europe/Berlin"
        org-icalendar-alarm-time 15
        org-icalendar-include-todo t
        org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due)
        org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo)
        org-icalendar-exclude-tags '("weekly" "daily" "monthly")
        org-caldav-exclude-tags '("weekly" "daily" "monthly")))
