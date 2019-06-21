;;;; Custom init.el
;;;; Ronald Leppink

;;; Packages
(load-file (expand-file-name "packages.el" user-emacs-directory))

;;; Color theme
(load-file (expand-file-name "acme-theme.el" user-emacs-directory))
(load-theme 'acme t)

;;; Customize settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;; Load org-mode settings
(load-file (expand-file-name "org.el" user-emacs-directory))


;; Can't be set by Customize?
(set-frame-font "DejaVu Sans Mono-8")


;;; Always remove good-for-nothing trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;; Love 2D
(defun run-love ()
  "Run the Love 2D game framework"
  (interactive)
  (async-shell-command "love ."))


;;; Elixir
(defun execute-elixir ()
  "Execute the `elixir` command in the current directory"
  (interactive)
  (async-shell-command (concat "elixir" " " (buffer-file-name))))


;;; Set directory for backup files
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))


;;; Allow indenting of the whole buffer
(defun reindent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))
