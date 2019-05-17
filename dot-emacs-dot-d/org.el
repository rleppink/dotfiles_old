;;;; org.el
;;;; Settings for org-mode

;;; Set up nice usage of PlantUML in org-mode
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

(setq org-plantuml-jar-path (expand-file-name "C:/users/ronaldl/plantuml.jar"))

;; Don't require confirmation every time PlantUML code is evaluated
(setq org-confirm-babel-evaluate
      (lambda (lang body)
	(not (string= lang "plantuml"))))


(defun org-babel-execute-display ()
  "Execute org babel source block, and display inline images immediately"
  (interactive)
  (org-babel-execute-src-block-maybe)
  (org-display-inline-images))

(define-key org-mode-map (kbd "C-c e") 'org-babel-execute-display)


;;; org-capture setup
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      '(("w" "TODO [w]ork" entry
         (file+headline "~/Stack/sync/org/work.org" "Work")
         "* TODO %i%?")
        ("p" "TODO [p]ersonal" entry
         (file+headline "~/Stack/sync/org/personal.org" "Personal")
         "* TODO %i%?")
	("i" "Project [i]dea" entry
	 (file+headline "~/stack/sync/org/personal.org" "Personal")
	 "* Project idea: %i%?")))
