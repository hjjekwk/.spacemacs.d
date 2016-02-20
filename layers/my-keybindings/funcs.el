(defun mc/mark-all-dwim-or-mark-sexp (arg)
  "C-u C-M-SPC: mc/mark-all-dwim
C-u C-u C-M-SPC: C-u M-x mc/mark-all-dwim"
  (interactive "p")
  (cl-case arg
    (16 (mc/mark-all-dwim t))
    (4 (mc/mark-all-dwim nil))
    (1 (mark-sexp 1))))
