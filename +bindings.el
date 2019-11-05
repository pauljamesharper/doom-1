;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

;; everywhere
(map! :leader
      :desc "Add to dictionary"       "a"    #'my/flyspell-save-word
      :desc "Change dictionary"       "d"    #'ispell-change-dictionary

      (:prefix-map ("i" . "insert")
        :desc "Insert math symbol"    "m"    #'helm-insert-latex-math))

;; org-mode
(map! :map org-mode-map
      :desc "Hide property drawers"   "M-p"  #'my/org-cycle-hide-properties-everywhere

      (:localleader
        :desc "Hide property drawers"  "p"   #'my/org-cycle-hide-properties-everywhere))

;; pdf-tools
(map! :map pdf-view-mode-map
      (:prefix-map ("M-a" . "annotate")
        :desc "Add highlight"          "h"  #'pdf-annot-add-highlight-markup-annotation
        :desc "Add text annotation"    "t"  #'pdf-annot-add-text-annotation))
