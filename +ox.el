;;; ~/.doom.d/+ox-word.el -*- lexical-binding: t; -*-

(use-package! ox-word
  :load-path "~/.doom.d/load/ox-word/"
  :after ox)

(use-package! ox-hugo
  :after ox
  :config
  (setq org-hugo-section "post"))

(after! ox-latex
  (setq org-latex-bib-compiler "biber"
        org-latex-pdf-process
  '("latexmk -shell-escape -bibtex -pdf %f"))
  ;; #+latex_header: \usepackage[citestyle=authoryear-icomp,bibstyle=authoryear, hyperref=true,backref=true,maxcitenames=3,url=true,backend=biber,natbib=true]{biblatex}
  ;; #+latex_header: \addbibresource{~/Library/.bib/library.bib}
  ;; (add-to-list 'org-latex-packages-alist
  ;;             "\\usepackage[backend=biber, eprint=false, url=true,
  ;;             isbn=false, style=verbose-inote,
  ;;             date=year]{biblatex}" t)
  ;; (add-to-list 'org-latex-packages-alist
  ;;             "\\addbibresource{~/library.bib}" t)

  (add-to-list 'org-export-smart-quotes-alist
              '("en_cs"
                  (primary-opening   :utf-8 "“" :html "&ldquo;" :latex "\\enquote{"  :texinfo "``")
                  (primary-closing   :utf-8 "”" :html "&rdquo;" :latex "}"           :texinfo "''")
                  (secondary-opening :utf-8 "‘" :html "&lsquo;" :latex "\\enquote*{" :texinfo "`")
                  (secondary-closing :utf-8 "’" :html "&rsquo;" :latex "}"           :texinfo "'")
                  (apostrophe        :utf-8 "’" :html "&rsquo;"))))
