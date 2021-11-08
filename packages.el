;;; ~/.doom.d/packages.el

;; General
(package! org-caldav)
(package! org-super-agenda)
(package! anki-editor)
(package! org-analyzer)
(package! org-clock-csv)

;; Disabled stuff for terminal
(package! solaire-mode :disable t)

(package! org-clock-budget
  :recipe (:host github :repo "Fuco1/org-clock-budget"))
(package! mathpix
  :recipe (:host github :repo "jethrokuan/mathpix.el" :branch "master"))

;; (package! bibtex-actions
;;   :recipe (:host github :repo "bdarcus/bibtex-actions")
;;   :pin "b088175b04958460c82d06254321c53839400948")

(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))
