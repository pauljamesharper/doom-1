;;; ~/.doom.d/+pdf-tools.el -*- lexical-binding: t; -*-

(after! pdf-tools
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
  (add-hook 'pdf-view-mode-hook #'pdf-tools-enable-minor-modes)
  ;; (setq +latex-viewers '(pdf-tools))
  (setq pdf-annot-color-history '("yellow" "red" "green" "blue")
        pdf-annot-default-annotation-properties '((t (label . "Lino on Emacs"))
                                                  (text (icon . "Note")
                                                        (color . "#ff0000"))
                                                  (highlight (color . "yellow")))))
