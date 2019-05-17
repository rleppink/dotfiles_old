;; Initialize MELPA
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


;; Package list
(use-package which-key :ensure t)

(use-package lua-mode
	     :ensure t
	     :bind (:map lua-mode-map
			 ("C-c C-r" . 'run-love)))
(use-package helm
  :ensure t
  :config (require 'helm-config))

(use-package evil
  :ensure t
  :init (setq evil-want-C-u-scroll t)
  :config (evil-mode))


;; F#
(use-package fsharp-mode :ensure t)
