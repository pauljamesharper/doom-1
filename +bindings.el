;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

;; everywhere
(map! :leader
      :desc "Add to dictionary"       "a"      #'my/flyspell-save-word
      :desc "Change dictionary"       "d"      #'ispell-change-dictionary

      (:prefix-map ("i" . "insert")
        :desc "Insert math symbol"    "m"      #'helm-insert-latex-math)

      (:prefix-map ("r" . "org-ref")
        :desc "citation"               "c"     #'org-ref-insert-cite-with-completion
        :desc "pdf"                    "p"     #'org-ref-open-pdf-at-point
        :desc "notes"                  "n"     #'org-ref-open-notes-at-point))

;; org-mode specific
(map! :map org-mode-map
      :desc "Hide property drawers"   "M-p"    #'my/org-cycle-hide-properties-everywhere
      :desc "org-ref"                 "C-c c"  #'org-ref-insert-cite-with-completion

      (:localleader
        :desc "Hide property drawers"  "p"     #'my/org-cycle-hide-properties-everywhere))

;; pdf-tools specific
(map! :map pdf-view-mode-map
      (:prefix-map ("M-a" . "annotate")
        :desc "Add highlight"          "h"    #'pdf-annot-add-highlight-markup-annotation
        :desc "Add text annotation"    "t"    #'pdf-annot-add-text-annotation))
