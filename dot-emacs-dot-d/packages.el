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
(use-package ivy
  :ensure t
  :diminish (ivy-mode . "") ; Don't dispay ivy-mode in the modeline
  :init (ivy-mode 1)
  :demand
  :config
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d "))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
	 ("C-c C-f" . counsel-find-file)
	 ("C-c C-s" . counsel-git-grep)))


(use-package markdown-mode :ensure t)
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

(use-package elixir-mode
  :ensure t
  :bind (:map elixir-mode-map
	      ("C-c C-x" . 'execute-elixir)))
