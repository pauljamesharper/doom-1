;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "6a884fe8ae")
(package! helm-bibtex :pin "6a884fe8ae")
(when (featurep! :lang org)
  (package! org-ref :pin "1eb4ddda00"))
(when (featurep! :lang org +roam)
  (package! org-roam-bibtex :pin "a97ec6d954"))
