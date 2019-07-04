;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

;;ORG-MODE
(after! org

  ;; Org-Agenda
  (setq org-agenda-files '("~/Exocortex/Executive"))

  ;;Capture Templates
  (setq org-capture-templates
        '(("t" "Todo" entry
         (file+headline "~/Exocortex/Executive/actions.org" "Other")
          "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
         ("a" "appointment" entry
         (file+headline "~/Exocortex/Executive/calendar.org" "Appointments")
          "* %?\nSCHEDULED: %^T\n%a\n"))
  )

  ;; Latex-Export
  (require 'ox-latex)
  (require 'ox-bibtex)
  (require 'org)
  (require 'ox)

  (setq org-latex-bib-compiler "biber"
        org-latex-pdf-process ; -shell-escape needed for minted
        '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "biber %b"
          "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; Bibliography
  (add-to-list 'org-latex-packages-alist
               "\\usepackage[backend=biber, eprint=false, url=false,
               isbn=false, style=authoryear-icomp,
               date=year]{biblatex}" t)
  (add-to-list 'org-latex-packages-alist
               "\\addbibresource{~/Exocortex/library.bib}" t)
)
