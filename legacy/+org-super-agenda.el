;;; ~/.doom.d/+org-super-agenda.el -*- lexical-binding: t; -*-

(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-start-day "+0d")
  (setq org-agenda-span 'day)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-dim-blocked-tasks nil) ;; makes main tasks visible in agenda-view
  (setq org-agenda-files '("~/org/actions.org"
                           "~/org/cs.org"
                          "~/org/strategy.org"
                          "~/org/reading.org"
                          "~/org/watching.org"
                          "~/org/calendar.org"
                          "~/org/outreach.org"
                          "~/org/calendar-inbox.org"))
  (setq org-super-agenda-groups '((:name "Today"
                                         :time-grid t)
                                  (:name "Overdue"
                                         :deadline past)
                                  (:name "Due today"
                                          :deadline today)
                                  (:name "Due soon"
                                          :deadline future)
                                  (:name "Habits"
                                         :habit t)
                                  (:name "Reschedule or start"
                                         :scheduled past)
                                  (:name "Start today"
                                          :scheduled today)
                                  (:name "Start soon"
                                          :scheduled future)
                                  ))
    :config
    (org-super-agenda-mode))
