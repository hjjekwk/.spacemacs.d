(when (spacemacs/system-is-mac)
  (spacemacs/set-leader-keys "bf" 'reveal-in-osx-finder)

  (when (display-graphic-p)
    (setq mac-command-key-is-meta t)
    (setq mac-option-key-is-meta t)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)
    (setq mac-function-modifier 'hyper)))
