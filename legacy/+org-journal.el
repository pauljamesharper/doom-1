;;; ~/.doom.d/+journal.el -*- lexical-binding: t; -*-


(after! org-journal
  (setq org-journal-file-type 'yearly
        org-journal-file-format "%Y.org.gpg"
        org-journal-date-format "<%Y-%M-%d>"
        org-journal-search-result-date-format "<%Y-%M-%d>"))
