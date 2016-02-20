(defconst my-org-reveal-packages
  '(
    (ox-reveal
     :location
     (recipe
      :fetcher github
      :repo "yjwen/org-reveal"
      :files ("ox-reveal.el" "Readme.org" "local.css" "images/*.png"))
     )
    )
  )

(defun my-org-reveal/init-ox-reveal ()
  (use-package ox-reveal
    :config
    (setq org-reveal-root
          (expand-file-name "~/.spacemacs.d/layers/my-org-reveal/local/reveal.js"))))
