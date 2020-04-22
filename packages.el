;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! doom-snippets :ignore t)
(package! yasnippet-snippets)
(package! org-caldav)
(package! org-super-agenda)
(package! org-ref)
(package! mu4e-alert)
(package! anki-editor)
(package! org-clock-budget :recipe (:host github :repo "Fuco1/org-clock-budget"))
(package! mathpix :recipe (:host github :repo "jethrokuan/mathpix.el"))
