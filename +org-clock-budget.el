;;; ~/.doom.d/+org-clock-budget.el -*- lexical-binding: t; -*-

(use-package! org-clock-budget
  :config
  (setq org-clock-budget-intervals '(("BUDGET_WEEK" org-clock-budget-interval-this-week)
                                     ("BUDGET_YEAR" org-clock-budget-interval-this-year))
        org-clock-budget-ratio-faces '((1.0 hydra-face-red)
                                       (0.95 font-lock-type-face)
                                       (0.5 ivy-confirm-face)
                                       (0.0 font-lock-keyword-face))))
