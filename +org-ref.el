;;; ~/.doom.d/+org-ref.el -*- lexical-binding: t; -*-

(use-package! org-ref
  :after org
  :config
  (setq reftex-default-bibliography
        '("~/Library/.bib/library.bib"
          "~/Library/.bib/platform_state_surveillance.bib")
        org-ref-default-bibliography
        '("~/Library/.bib/library.bib"
          "~/Library/.bib/platform_state_surveillance.bib")
        org-ref-pdf-directory '("~/Library")
        org-ref-bibliography-notes "~/org/bibnotes.org"
        org-ref-notes-function #'org-ref-notes-function-many-files
        org-ref-default-citation-link "autocite"
        org-ref-cite-types '("autocite" "textcite" "autocites" "textcites" "fullcite"))

  (defun my/org-ref-open-pdf-at-point ()
    "Open the pdf for bibtex key under point if it exists."
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
          (key (car results))
    (pdf-file (car (bibtex-completion-find-pdf key))))
      (if (file-exists-p pdf-file)
    (org-open-file pdf-file)
        (message "No PDF found for %s" key))))

  (setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)

  (setq bibtex-completion-bibliography
        '("~/Library/.bib/library.bib"
          "~/Library/.bib/platform_state_surveillance.bib")
        bibtex-completion-library-path "~/Library/"
        bibtex-completion-pdf-field "file"
        bibtex-completion-notes-path "~/org/bibnotes"
        bibtex-completion-notes-template-multiple-files "\n\n* ${title}")

  (defun org-ref-noter-at-point ()
    "Open the pdf for bibtex key under point if it exists."
    (interactive)
      (let* ((results (org-ref-get-bibtex-key-and-file))
            (key (car results))
        (pdf-file (car (bibtex-completion-find-pdf key))))
          (if (file-exists-p pdf-file)
              (progn
                (find-file-other-window pdf-file)
                (org-noter))
          (message "No PDF found for %s" key)))))

        ;; (interactive)
        ;; (let* ((results (org-ref-get-bibtex-key-and-file))
        ;;       (key (car results))
        ;;       (pdf-file (funcall org-ref-get-pdf-filename-function key)))
        ;;   (if (file-exists-p pdf-file)
        ;;       (progn
        ;;         (find-file-other-window pdf-file)
        ;;         (org-noter))
        ;;     (message "no pdf found for %s" key))))

  ;; (add-to-list 'org-ref-helm-user-candidates
  ;;             '("Org-Noter notes" . org-ref-noter-at-point)))
