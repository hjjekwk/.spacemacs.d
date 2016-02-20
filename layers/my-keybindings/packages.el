(defconst my-keybindings-packages
  '(hydra multiple-cursors))

(defun my-keybindings/init-multiple-cursors ()
  (use-package multiple-cursors
    :bind (("H-n" . mc/mark-next-like-this)
           ("H-p" . mc/mark-previous-like-this)
           ("C-M-SPC" . mc/mark-all-dwim-or-mark-sexp)
           ("H-'" . mc/mark-more-like-this-extended)
           ("H-:" . multiple-cursors-hydra/mc/mark-next-like-this))
    ))

(defun my-hydra/init-hydra ()
  (use-package hydra
    :bind (("s-f" . hydra-projectile/body)
           ("C-x t" . hydra-toggle/body)
           ("C-M-o" . hydra-window/body))
    :config
    (hydra-add-font-lock)

    (require 'windmove)

    (defun hydra-move-splitter-left (arg)
      "Move window splitter left."
      (interactive "p")
      (if (let ((windmove-wrap-around))
            (windmove-find-other-window 'right))
          (shrink-window-horizontally arg)
        (enlarge-window-horizontally arg)))

    (defun hydra-move-splitter-right (arg)
      "Move window splitter right."
      (interactive "p")
      (if (let ((windmove-wrap-around))
            (windmove-find-other-window 'right))
          (enlarge-window-horizontally arg)
        (shrink-window-horizontally arg)))

    (defun hydra-move-splitter-up (arg)
      "Move window splitter up."
      (interactive "p")
      (if (let ((windmove-wrap-around))
            (windmove-find-other-window 'up))
          (enlarge-window arg)
        (shrink-window arg)))

    (defun hydra-move-splitter-down (arg)
      "Move window splitter down."
      (interactive "p")
      (if (let ((windmove-wrap-around))
            (windmove-find-other-window 'up))
          (shrink-window arg)
        (enlarge-window arg)))

    (defhydra hydra-toggle (:color teal)
      "
_a_ abbrev-mode:      %`abbrev-mode
_d_ debug-on-error    %`debug-on-error
_f_ auto-fill-mode    %`auto-fill-function
_t_ truncate-lines    %`truncate-lines

"
      ("a" abbrev-mode nil)
      ("d" toggle-debug-on-error nil)
      ("f" auto-fill-mode nil)
      ("t" toggle-truncate-lines nil)
      ("q" nil "cancel"))

    (key-chord-define-global
     "ds"
     (defhydra hydra-zoom ()
       "zoom"
       ("j" text-scale-increase "in")
       ("k" text-scale-decrease "out")
       ("0" (text-scale-set 0) "reset")
       ("1" (text-scale-set 0) :bind nil)
       ("2" (text-scale-set 0) :bind nil :color blue)))

    (defhydra hydra-error (global-map "M-g")
      "goto-error"
      ("h" flycheck-list-errors "first")
      ("j" flycheck-next-error "next")
      ("k" flycheck-previous-error "prev")
      ("v" recenter-top-bottom "recenter")
      ("q" nil "quit"))

    (defhydra hydra-window (:color amaranth)
      "
Move Point^^^^   Move Splitter   ^Ace^                       ^Split^
--------------------------------------------------------------------------------
_w_, _<up>_      Shift + Move    _C-a_: ace-window           _2_: split-window-below
_a_, _<left>_                    _C-s_: ace-window-swap      _3_: split-window-right
_s_, _<down>_                    _C-d_: ace-window-delete    ^ ^
_d_, _<right>_                   ^   ^                       ^ ^
You can use arrow-keys or WASD.
"
      ("2" split-window-below nil)
      ("3" split-window-right nil)
      ("a" windmove-left nil)
      ("s" windmove-down nil)
      ("w" windmove-up nil)
      ("d" windmove-right nil)
      ("A" hydra-move-splitter-left nil)
      ("S" hydra-move-splitter-down nil)
      ("W" hydra-move-splitter-up nil)
      ("D" hydra-move-splitter-right nil)
      ("<left>" windmove-left nil)
      ("<down>" windmove-down nil)
      ("<up>" windmove-up nil)
      ("<right>" windmove-right nil)
      ("<S-left>" hydra-move-splitter-left nil)
      ("<S-down>" hydra-move-splitter-down nil)
      ("<S-up>" hydra-move-splitter-up nil)
      ("<S-right>" hydra-move-splitter-right nil)
      ("C-a" ace-window nil)
      ("u" hydra--universal-argument nil)
      ("C-s" (lambda () (interactive) (ace-window 4)) nil)
      ("C-d" (lambda () (interactive) (ace-window 16)) nil)
      ("q" nil "quit"))

    (defhydra hydra-org-template (:color blue :hint nil)
      "
_c_enter  _q_uote     _e_macs-lisp    _L_aTeX:
_l_atex   _E_xample   _p_erl          _i_ndex:
_a_scii   _v_erse     _P_erl tangled  _I_NCLUDE:
_s_rc     ^ ^         plant_u_ml      _H_TML:
_h_tml    ^ ^         ^ ^             _A_SCII:
"
      ("s" (hot-expand "<s"))
      ("E" (hot-expand "<e"))
      ("q" (hot-expand "<q"))
      ("v" (hot-expand "<v"))
      ("c" (hot-expand "<c"))
      ("l" (hot-expand "<l"))
      ("h" (hot-expand "<h"))
      ("a" (hot-expand "<a"))
      ("L" (hot-expand "<L"))
      ("i" (hot-expand "<i"))
      ("e" (progn
             (hot-expand "<s")
             (insert "emacs-lisp")
             (forward-line)))
      ("p" (progn
             (hot-expand "<s")
             (insert "perl")
             (forward-line)))
      ("u" (progn
             (hot-expand "<s")
             (insert "plantuml :file CHANGE.png")
             (forward-line)))
      ("P" (progn
             (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
             (hot-expand "<s")
             (insert "perl")
             (forward-line)))
      ("I" (hot-expand "<I"))
      ("H" (hot-expand "<H"))
      ("A" (hot-expand "<A"))
      ("<" self-insert-command "ins")
      ("o" nil "quit"))

    (defun hot-expand (str)
      "Expand org template."
      (insert str)
      (org-try-structure-completion))

    (with-eval-after-load "org"
      (define-key org-mode-map "<"
        (lambda () (interactive)
          (if (looking-back "^")
              (hydra-org-template/body)
            (self-insert-command 1))))))

  (defhydra hydra-projectile (:color blue :columns 4)
    "Projectile"
    ("a" counsel-git-grep "ag")
    ("b" projectile-switch-to-buffer "switch to buffer")
    ("c" projectile-compile-project "compile project")
    ("d" projectile-find-dir "dir")
    ("f" projectile-find-file "file")
    ;; ("ff" projectile-find-file-dwim "file dwim")
    ;; ("fd" projectile-find-file-in-directory "file curr dir")
    ("g" ggtags-update-tags "update gtags")
    ("i" projectile-ibuffer "Ibuffer")
    ("K" projectile-kill-buffers "Kill all buffers")
    ;; ("o" projectile-multi-occur "multi-occur")
    ("p" projectile-switch-project "switch")
    ("r" projectile-run-async-shell-command-in-root "run shell command")
    ("x" projectile-remove-known-project "remove known")
    ("X" projectile-cleanup-known-projects "cleanup non-existing")
    ("z" projectile-cache-current-file "cache current")
    ("q" nil "cancel"))

  (defhydra multiple-cursors-hydra (:hint nil)
    "
       ^Up^            ^Down^        ^Other^
  ----------------------------------------------
  [_p_]   Next    [_n_]   Next    [_l_] Edit lines
  [_P_]   Skip    [_N_]   Skip    [_a_] Mark all
  [_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
  ^ ^             ^ ^             [_q_] Quit
  "
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("r" mc/mark-all-in-region-regexp :exit t)
    ("q" nil))
  )
