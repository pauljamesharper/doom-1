;;; ~/.doom.d/+refman.el -*- lexical-binding: t; -*-

(def-package! org-ref
  :commands (org-ref-bibtex-next-entry
             org-ref-bibtex-previous-entry
             doi-utils-get-bibtex-entry-pdf
             org-ref-helm-insert-cite-link
             org-ref-find-bibliography
             org-ref-open-in-browser
             org-ref-open-bibtex-notes
             org-ref-open-bibtex-pdf
             org-ref-bibtex-hydra/body
             org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
             org-ref-sort-bibtex-entry
             arxiv-add-bibtex-entry
             arxiv-get-pdf-add-bibtex-entry
             doi-utils-add-bibtex-entry-from-doi
             isbn-to-bibtex
             pubmed-insert-bibtex-from-pmid)
  :init
  (setq org-ref-completion-library 'org-ref-helm-bibtex))

(def-package! bibtex
  :defer t
  :config
  (setq bibtex-dialect 'biblatex
        bibtex-align-at-equal-sign t
        bibtex-text-indentation 20)
  (map! :map bibtex-mode-map
        [fill-paragraph] #'bibtex-fill-entry))

(def-package! bibtex-completion
  :defer t)

(def-package! helm-bibtex
  :commands helm-bibtex)

(setq org-ref-default-citation-link "autocite")
(setq reftex-default-bibliography '("~/Exocortex/library.bib" "~/Projects/kolleg/references.bib"))

(setq org-ref-default-bibliography '("~/Exocortex/library.bib" "~/Projects/kolleg/references.bib")
      org-ref-pdf-directory "~/Zotero/storage/")

(setq bibtex-completion-library-path '("~/Zotero/storage/"))
(setq bibtex-completion-pdf-field "file")
(setq bibtex-completion-bibliography
      '("~/Exocortex/library.bib"
        "~/Projects/kolleg/references.bib"))
(setq bibtex-completion-notes-path "~/Exocortex/Memory/Academic/bibnotes/")

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

(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.x?html?\\'" . "firefox %s")
        ))

(add-hook 'org-mode-hook (lambda () (require 'org-ref)))
