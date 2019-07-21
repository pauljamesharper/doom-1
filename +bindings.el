;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-



(map! :leader
      :desc "Add to dictionary"       "a"    #'my/flyspell-save-word)

(after! org
  (map! :map org-mode-map
        :desc "Hide property drawers"   "M-p"  #'my/org-cycle-hide-properties-everywhere

        (:localleader
          :desc "Hide property drawers" "p"    #'my/org-cycle-hide-properties-everywhere
          :desc "Archive subtree"       "a"    #'org-archive-subtree

        (:prefix ("e" . "export")
          :desc "org-export"            "e"    #'org-export-dispatch)))
)
