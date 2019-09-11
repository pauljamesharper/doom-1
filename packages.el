;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! org-re-reveal)
(package! helm-bibtex)
(package! org-ref :recipe (:host github :repo "jkitchin/org-ref" :files ("*")))
(package! org-caldav)
(package! mu4e-alert)
(package! fancy-battery)
(package! org-journal)
(package! org-noter)
(package! ox-hugo)
