(defconst my-eww-packages
  '())

(require 'eww)

;; disable colorieze
(defvar eww-disable-colorize t)
;; use google for search
(setq eww-search-prefix "http://www.google.co.jp/search?q=")

(defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
  (unless eww-disable-colorize
    (funcall orig start end fg)))
(advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
(advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)

(defun eww-disable-color ()
  "disable color for eww"
  (interactive)
  (setq-local eww-disable-colorize t)
  (eww-reload))

(defun eww-enable-color ()
  "enable color for eww"
  (interactive)
  (setq-local eww-disable-colorize nil)
  (eww-reload))

;; open new website in new buffer
(defun eww-mode-hook--rename-buffer ()
  "Rename eww browser's buffer so sites open in new page."
  (rename-buffer "eww" t))
; (add-hook 'eww-mode-hook 'eww-mode-hook--rename-buffer)

(defun eww-disable-images ()
  "do not show images"
  (interactive)
  (setq-local shr-put-image-function 'shr-put-image-alt)
  (eww-reload))

(defun eww-enable-images ()
  "show images"
  (interactive)
  (setq-local shr-put-image-function 'shr-put-image)
  (eww-reload))

(defun shr-put-image-alt (spec alt &optional flags)
  (insert alt))

;; default do not show image
(defun eww-mode-hook--disable-image ()
  (setq-local shr-put-image-function 'shr-put-image-alt))
(add-hook 'eww-mode-hook 'eww-mode-hook--disable-image)

;;; packages.el ends here
