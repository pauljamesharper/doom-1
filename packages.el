;;; ~/.doom.d/packages.el

;; org-mode related
(package! org-caldav :pin "8569941a0a5a9393ba51afc8923fd7b77b73fa7a")
(package! org-super-agenda :pin "fb5e2ef277bc811a3b061106c99e4c47b6b86f80")
(package! anki-editor :pin "546774a453ef4617b1bcb0d1626e415c67cc88df")
(package! org-analyzer :pin "19da62aa4dcf1090be8f574f6f2d4c7e116163a8")
(package! websocket)
(package! org-roam-ui
  :pin "fb8a3eae55a7d7859455b23962448e276b8204bb"
  :recipe (:host github
           :repo "org-roam/org-roam-ui"
           :branch "main"
           :files ("*.el" "out")))

;; other
(package! string-inflection
  :pin "e9f548606e3d56b58874b4d664cfd71d0b06a42c"
  :recipe (:host github
           :repo "akicho8/string-inflection"
           :branch "master"))

(package! mathpix
  :pin "1ce2d4aa7708271cf60ec929688c1ce420c3fc86"
  :recipe (:host github
           :repo "jethrokuan/mathpix.el"
           :branch "master"))

(package! lsp-tailwindcss
  :pin "8b45d5ab6ad41f881ef52983d6906193736e6f41"
  :recipe (:host github
           :repo "merrickluo/lsp-tailwindcss"
           :branch "master"))

;; I don't like solaire-mode
(package! solaire-mode :disable t)
