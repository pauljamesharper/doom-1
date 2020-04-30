;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "631dc607e8")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "631dc607e8"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "631dc607e8"))
(when (featurep! :lang org)
  (package! org-ref :pin "1eb4ddda00"))
(when (featurep! :lang org +roam)
  (package! org-roam-bibtex :pin "6bbdebb39d"))
