;;; tools/biblio/config.el -*- lexical-binding: t; -*-

(use-package! citar
  :after org
  :when (featurep! :completion vertico)
  :defer t)

(use-package! citeproc
  :after org
  :defer t)

;;; Org-Cite configuration

(use-package! oc
  :after org
  :config
  (require 'ox)
  (map! :map org-mode-map
        :localleader
        :desc "Insert citation" "@" #'org-cite-insert)
  (setq org-cite-global-bibliography
        (let ((paths
               (cond
                ((boundp 'citar-bibliography) citar-bibliography)
                ((boundp 'bibtex-completion-bibliography) bibtex-completion-bibliography))))
          ;; Always return bibliography paths as list for org-cite.
          (if (stringp paths) (list paths) paths)))
  ;; setup export processor; default csl/citeproc-el, with biblatex for latex
  (setq org-cite-export-processors
        '((latex biblatex)
          (t csl))))

  ;;; Org-cite processors
(use-package! oc-biblatex
  :after oc)

(use-package! oc-csl
  :after oc)

(use-package! oc-natbib
  :after oc)

;;;; Third-party

(use-package! citar-org
  :after org
  :no-require
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (org-support-shift-select t)
  (when (featurep! :lang org +roam2)
    ;; Include property drawer metadata for 'org-roam' v2.
    (citar-org-note-include '(org-id org-roam-ref))))

(use-package org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode))
