;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :desc "Add to dictionary"         "a"    #'my/flyspell-save-word
      :desc "Change dictionary"         "d"    #'ispell-change-dictionary

      (:prefix-map ("i" . "insert")
         :desc "Insert math symbol"     "m"    #'helm-insert-latex-math))

