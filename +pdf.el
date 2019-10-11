;;; ~/.doom.d/+pdf-annot.el -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
(add-hook 'pdf-view-mode-hook #'pdf-tools-enable-minor-modes)

(setq pdf-annot-color-history '("LightGoldenrod1" "salmon" "pale green" "cornflower blue")
      pdf-annot-default-annotation-properties
      '((t (label . "Lino on Emacs"))
        (text (icon . "Note")
              (color . "#ff0000"))
        (highlight (color . "LightGoldenrod1"))
        (squiggly (color . "orange"))
        (strike-out (color . "red"))
        (underline (color . "blue"))))
