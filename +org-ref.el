;;; ~/.doom.d/+org-ref.el -*- lexical-binding: t; -*-

(use-package! org-ref
  :config
  (setq reftex-default-bibliography
        '("~/Library/.bib/library.bib"
          "~/Library/.bib/platform_state_surveillance.bib")
        org-ref-default-bibliography
        '("~/Library/.bib/library.bib"
          "~/Library/.bib/platform_state_surveillance.bib")
        org-ref-pdf-directory '("~/Library")
        org-ref-bibliography-notes "~/Projects/personal-website/content/post/summaries.org"
        org-ref-default-citation-link "autocite"
        org-ref-cite-types '("autocite" "textcite" "autocites" "textcites" "fullcite"))

  (setq org-ref-notes-function
        (lambda (thekey)
          (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
            (bibtex-completion-edit-notes
            (list (car (org-ref-get-bibtex-key-and-file thekey)))))))

  (setq bibtex-completion-bibliography
        '("~/Library/.bib/library.bib"
          "~/Library/.bib/platform_state_surveillance.bib")
        bibtex-completion-library-path "~/Library/"
        bibtex-completion-pdf-field "file"
        bibtex-completion-notes-path "~/Projects/personal-website/content/post/"
        bibtex-completion-notes-extension ".md"
        bibtex-completion-notes-template-multiple-files (format "---\ntitle: \"${title} (${author-or-editor} ${year})\"\n"))


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


  ;; PDF Opening Behaviour
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
