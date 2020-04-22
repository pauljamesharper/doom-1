;;; ~/.doom.d/+mathpix.el -*- lexical-binding: t; -*-

(use-package! mathpix.el
  :custom ((mathpix-app-id "mathpix_sehn_tech_b5ad38")
           (mathpix-app-key "f965173bcdbfec889c20"))
  :bind
  ("C-x m" . mathpix-screenshot))
