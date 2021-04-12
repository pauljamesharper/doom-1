;;; ~/.doom.d/packages.el

;; General
(package! org-caldav)
(package! org-super-agenda)
(package! anki-editor)

;; Disabled stuff for terminal
(package! solaire-mode :disable t)

;; Forked
(package! org-clock-budget
  :recipe (:host github :repo "linozen/org-clock-budget"))
(package! mathpix
  :recipe (:host github :repo "linozen/mathpix.el" :branch "master"))

;; (package! bibtex-actions
;;   :recipe (:host github :repo "bdarcus/bibtex-actions")
;;   :pin "b088175b04958460c82d06254321c53839400948")
