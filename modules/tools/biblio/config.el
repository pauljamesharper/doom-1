;;; tools/biblio/config.el -*- lexical-binding: t; -*-

(use-package! org-roam
  :hook (org-load . org-roam-mode)
  :hook (org-roam-backlinks-mode . turn-on-visual-line-mode)
  :commands (org-roam-buffer-toggle-display
             org-roam-dailies-find-date
             org-roam-dailies-find-today
             org-roam-dailies-find-tomorrow
             org-roam-dailies-find-yesterday)
  :init
  (map! :after org
        :map org-mode-map
        :localleader
        :prefix ("m" . "org-roam")
        "b" #'org-roam-switch-to-buffer
        "f" #'org-roam-find-file
        "g" #'org-roam-graph
        "i" #'org-roam-insert
        "I" #'org-roam-insert-immediate
        "m" #'org-roam
        (:prefix ("d" . "by date")
         :desc "Find previous note" "b" #'org-roam-dailies-find-previous-note
         :desc "Find date"          "d" #'org-roam-dailies-find-date
         :desc "Find next note"     "f" #'org-roam-dailies-find-next-note
         :desc "Find tomorrow"      "m" #'org-roam-dailies-find-tomorrow
         :desc "Capture today"      "n" #'org-roam-dailies-capture-today
         :desc "Find today"         "t" #'org-roam-dailies-find-today
         :desc "Capture Date"       "v" #'org-roam-dailies-capture-date
         :desc "Find yesterday"     "y" #'org-roam-dailies-find-yesterday
         :desc "Find directory"     "." #'org-roam-dailies-find-directory))
  :config
  (setq org-roam-directory "~/Exocortex"
        org-roam-db-location "~/Exocortex/.exocortex.db"
        +org-roam-open-buffer-on-find-file nil
        org-roam-verbose nil            ; https://youtu.be/fn4jIlFwuLU
        ;; Make org-roam buffer sticky; i.e. don't replace it when opening a
        ;; file with an *-other-window command.
        org-roam-buffer-window-parameters '((no-delete-other-windows . t))
        org-roam-completion-everywhere t
        org-roam-completion-system
        (cond ((featurep! :completion helm) 'helm)
              ((featurep! :completion ivy) 'ivy)
              ((featurep! :completion ido) 'ido)
              ('default)))

  ;; Hide the mode line in the org-roam buffer, since it serves no purpose. This
  ;; makes it easier to distinguish from other org buffers.
  (add-hook 'org-roam-buffer-prepare-hook #'hide-mode-line-mode))

;; Since the org module lazy loads org-protocol (waits until an org URL is
;; detected), we can safely chain `org-roam-protocol' to it.
(use-package! org-roam-protocol
  :after org-protocol)

(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode))

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
  (when (featurep! :completion ivy)
    (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus)))
  :config
  (setq bibtex-completion-additional-search-fields '(keywords)
        ;; This tell bibtex-completion to look at the File field of the bibtex
        ;; to figure out which pdf to open
        bibtex-completion-pdf-field "file"))

(use-package! org-ref
  :when (featurep! :lang org)
  :after org
  :preface
  ;; This need to be set before the package is loaded, because org-ref will
  ;; automatically `require' an associated package during its loading.
  (setq org-ref-completion-library #'org-ref-ivy-cite)
  :config
  (defun my/org-ref-open-pdf-at-point ()
    "Open the pdf for bibtex key under point if it exists."
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results)))
      (funcall bibtex-completion-pdf-open-function (car (bibtex-completion-find-pdf key)))))
  ;; actually use it
  (setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)
  ;; I need this interactively
  (defun my/org-ref-update-pre-post-text ()
    "Prompt for pre/post text and update link accordingly. A blank string deletes pre/post text."
  (interactive)
  (save-excursion
    (let* ((cite (org-element-context))
           (type (org-element-property :type cite))
           (key (org-element-property :path cite))
           (text (read-from-minibuffer "Pre/post text: ")))
      ;; First we delete the citation
      (when (-contains? org-ref-cite-types type)
        (cl--set-buffer-substring
         (org-element-property :begin cite)
         (org-element-property :end cite)
         ""))
      ;; Then we reformat the citation
      (if (string= text "")
          (progn
            (insert (format "%s:%s " type key))
            ;; Avoid space before punctuation
            (when (looking-at "[[:punct:]]")
              (delete-char 1)))
        (insert (format "[[%s:%s][%s]] " type key text))
        (when (looking-at "[[:punct:]]")
          (delete-char 1)))))))

