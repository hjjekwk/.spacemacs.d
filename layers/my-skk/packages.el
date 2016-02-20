(defconst my-skk-packages
  '(
    (ddskk :location
           (recipe
            :fetcher github
            :repo "skk-dev/ddskk"
            :old-name (skk)
            :files ("context-skk.el" "ddskk*.el" "skk*.el"
                    "tar-util.el" "doc/skk.texi" "etc/skk.xpm"
                    (:exclude "skk-xemacs.el" "skk-lookup.el"))))
    )
  )

(defun my-skk/init-ddskk ()
  (use-package ddskk
    :init
    (setq skk-user-directory "~/.skk")
    :ensure t
    :defer t
    ))

(defun turn-on-skk-mode ()
  (interactive)
  (skk-mode 1))
