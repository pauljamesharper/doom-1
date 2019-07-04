;;; ~/.doom.d/+flyspell.el -*- lexical-binding: t; -*-


(setq ispell-program-name (executable-find "hunspell")
      ispell-dictionary "en_GB")

(bind-key "C-c d"
          (lambda ()
            (interactive)
            (ispell-change-dictionary "de_DE")
            (flyspell-buffer)))

(bind-key "C-c e"
          (lambda ()
            (interactive)
            (ispell-change-dictionary "en_GB")
            (flyspell-buffer)))
