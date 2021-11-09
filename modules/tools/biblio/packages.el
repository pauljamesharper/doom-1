;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el
;;;

(when (featurep! :completion vertico)
  (package! citar :pin "fd33f5c4f7981036a969b5ca8aaf42380848ab32"))

(package! citeproc :pin "c8ff95862823cdff067e8cc9bb7f5ef537e8f1d9")

;; for legacy docuements
(package! citeproc-org :pin "0fb4c96f48b3055a59a397af24d3f1a82cf77b66")
;; for org-roam connection
(package! org-roam-bibtex :pin "d65b70e9d19efc5001e01a61c36cc3e57e198131")
