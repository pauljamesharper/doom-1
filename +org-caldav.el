;;; ~/.doom.d/+caldav.el -*- lexical-binding: t; -*-

(use-package! org-caldav
  :defer t
  :init
  ;; This is the sync on close function; it also prompts for save after syncing so
  ;; no late changes get lost
  (defun org-caldav-sync-at-close ()
    (org-caldav-sync)
    (save-some-buffers))

  ;; This is the delayed sync function; it waits until emacs has been idle for
  ;; "secs" seconds before syncing.  The delay is important because the caldav-sync
  ;; can take five or ten seconds, which would be painful if it did that right at save.
  ;; This way it just waits until you've been idle for a while to avoid disturbing
  ;; the user.
  (defvar org-caldav-sync-timer nil
     "Timer that `org-caldav-push-timer' used to reschedule itself, or nil.")
  (defun org-caldav-sync-with-delay (secs)
    (when org-caldav-sync-timer
      (cancel-timer org-caldav-sync-timer))
    (setq org-caldav-sync-timer
	  (run-with-idle-timer
	   (* 1 secs) nil 'org-caldav-sync)))

  (setq org-caldav-url "https://dav.mailbox.org/caldav"
        org-caldav-calendar-id "Y2FsOi8vMC80NQ"
        org-caldav-inbox "~/org/.calendar-inbox.org"
        org-caldav-files '("~/org/calendar.org"
                         "~/org/actions.org"
                         "~/org/someday.org"))

  :config
  (setq org-icalendar-timezone "Europe/Berlin"
        org-icalendar-alarm-time 15
        ;; This makes sure to-do items as a category can show up on the calendar
        org-icalendar-include-todo t
        ;; This ensures all org "deadlines" show up, and show up as due dates
        org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due)
        ;; This ensures "scheduled" org items show up, and show up as start times
        org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo)
        ;; exclude-tags for habits
        org-icalendar-exclude-tags '("morning" "shutdown" "intellect")
        org-caldav-exclude-tags '("morning" "shutdown" "intellect")))
  ;; Add the delayed save hook with a five minute idle timer
  ;; (add-hook 'after-save-hook
  ;;     (lambda ()
  ;;       (when (eq major-mode 'org-mode)
  ;;   (org-caldav-sync-with-delay 300)))))
  ;; Add the close emacs hook
  ;; (add-hook 'kill-emacs-hook 'org-caldav-sync-at-close))
