;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; (package! doom-snippets :ignore t)
;; (package! yasnippet-snippets)
(package! org-caldav)
(package! company-bibtex)
(package! org-super-agenda)
(package! mu4e-alert)
(package! anki-editor)
(package! org-download)
(package! org-clock-budget
  :recipe (:host github :repo "linozen/org-clock-budget"))
(package! mathpix
  :recipe (:host github :repo "linozen/mathpix.el" :branch "master"))
(package! org-ref-ox-hugo
  :recipe (:host github :repo "linozen/org-ref-ox-hugo" :branch "custom/overrides"))
