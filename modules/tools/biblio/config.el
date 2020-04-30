;;; tools/biblio/config.el -*- lexical-binding: t; -*-

;; Internal function to set the various paths used in the
;; reference packages.
(defun +biblio-set-paths-fn (&optional symbol value)
  (when symbol
    (set-default symbol value))
  (when value
    (cond ((eq symbol '+biblio-pdf-library-dir)
           (when (featurep! :lang org)
             (setq org-ref-pdf-directory value))
           (setq bibtex-completion-library-path value))
          ((eq symbol '+biblio-default-bibliography-files)
           (when (featurep! :lang org)
             (setq reftex-default-bibliography value
                   org-ref-default-bibliography value))
           (setq bibtex-completion-bibliography value))
          ((eq symbol '+biblio-notes-path)
           (when (featurep! :lang org)
             (if (directory-name-p value)
                 (setq org-ref-notes-directory value)
               (setq org-ref-bibliography-notes value)))
           (setq bibtex-completion-notes-path value)))))


(defcustom +biblio-pdf-library-dir nil
  "Directory where pdf files are stored. Must end with a slash."
  :type 'string
  :set #'+biblio-set-paths-fn)

(defcustom +biblio-default-bibliography-files nil
  "A list of default bibtex files to use."
  :type '(repeat :tag "List of bibtex files" file)
  :set #'+biblio-set-paths-fn)

(defcustom +biblio-notes-path nil
  "The place where you will store your notes for bibliography files.

This can be either a single file or directory of files.
In case of directory the path must end with a slash."
  :type 'string
  :set #'+biblio-set-paths-fn)


(use-package! bibtex-completion
  :defer t
  :preface
  ;; Allow the user to set a template of their own via (setq). if the user does
  ;; not set one fall back to the +biblio variants which have a reasonable
  ;; fallback.
  (defvar bibtex-completion-notes-template-multiple-files nil)
   :config

  (when (featurep! :completion ivy)
    (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus)))

  (setq bibtex-completion-additional-search-fields '(keywords)
        ;; This tell bibtex-completion to look at the File field of the bibtex
        ;; to figure out which pdf to open
        bibtex-completion-pdf-field "file")
  (unless bibtex-completion-notes-template-multiple-files
    (setq bibtex-completion-notes-template-multiple-files
          "${title} : (${=key=})

- tags ::
- keywords :: ${keywords}

\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: /${file}\n  :NOTER_PAGE: \n  :END:\n\n")))


;; TODO which set of keys that should be bound for commonly used functions
;; see https://github.com/jkitchin/org-ref/blob/master/org-ref-core.el#L3998

(use-package! org-roam-bibtex
  :when (featurep! :lang org +roam)
  :preface
  ;; if the user has not set a template mechanism set a reasonable one of them
  ;; The package already tests for nil itself so we define a dummy tester
  (defvar org-roam-bibtex-preformat-keywords nil)
  (defvar org-roam-bibtex-templates nil)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (unless org-roam-bibtex-preformat-keywords
    (setq org-roam-bibtex-preformat-keywords
          '("=key=" "title" "url" "file" "author-or-editor" "keywords")))
  (unless org-roam-bibtex-templates
    (setq org-roam-bibtex-templates
          '(("r" "ref" plain (function org-roam-capture--get-point)
             ""
             :file-name "${slug}"
             :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}

\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(org-roam-bibtex-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"
             :unnarrowed t)))))
