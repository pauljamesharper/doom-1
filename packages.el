;;; ~/.doom.d/packages.el

;; General
(package! org-caldav)
(package! org-super-agenda)
(package! anki-editor)

;; Disabled stuff
(package! solaire-mode :disable t)

;; Forked
(package! org-clock-budget
  :recipe (:host github :repo "linozen/org-clock-budget"))
(package! mathpix
  :recipe (:host github :repo "linozen/mathpix.el" :branch "master"))
