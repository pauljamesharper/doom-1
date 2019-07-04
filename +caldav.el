;;; ~/.doom.d/+caldav.el -*- lexical-binding: t; -*-

(def-package! org-caldav
  :commands (
             org-caldav-sync
             org-caldav-delete-everything
             )
  :config
  (setq org-caldav-url "https://dav.mailbox.org/caldav"
        org-caldav-calendar-id "Y2FsOi8vMC80NA"
        org-caldav-inbox "~/Exocortex/Executive/calendar-inbox.org"
        org-caldav-files '("~/Exocortex/Executive/calendar.org"
                         "~/Exocortex/Executive/actions.org"
                         "~/Exocortex/Executive/someday.org")
        org-icalendar-timezone "Europe/Berlin"
        org-icalendar-alarm-time 1
        ;; This makes sure to-do items as a category can show up on the calendar
        org-icalendar-include-todo t
        ;; This ensures all org "deadlines" show up, and show up as due dates
        org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due)
        ;; This ensures "scheduled" org items show up, and show up as start times
        org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo)
        )
)
