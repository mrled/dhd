; andraia's host-specific emacs init file

; misc stuff
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
(autoload 'psvn "psvn" "SVN for Emacs")
(autoload 'org-install "org-install" "Organization mode for Emacs")
(autoload 'vm "vm" "A more MUA-like MUA for Emacs")

; emacs-muse
(if (file-exists-p 
  (setq custom-file (concat "~/doc/remote/dhd/host/" host-name "/emacs.muse.el")))
    (load-file custom-file))


;; ceded (emacs ide-like features) stuff
;(if (file-exists-p (setq cedet-file (concat home-load-path "cedet/common/cedet.elc")))
;  (load-file cedet-file)
;  (semantic-load-enable-code-helpers))

; slime
; (if (file-exists-p (setq slime-path (concat home-load-path "slime/")))
;   (add-to-list 'load-path slime-path)
;   (setq inferior-lisp-program "/usr/local/bin/openmcl")
;   (require 'slime)
;   (slime-setup))
(add-to-list 'load-path "~/opt/emacs/slime-2.0/")
(setq inferior-lisp-program "/usr/local/bin/openmcl")
(require 'slime)
(slime-setup)

(autoload 'slime "slime"
          "Start an inferior^_superior Lisp and connect to its Swank server."
          t)
(autoload 'slime-mode "slime"
          "SLIME: The Superior Lisp Interaction Mode for Emacs (minor-mode)."
          t)

;; TeX stuff
;(load "auctex.el" nil t t)
;(load "preview-latex.el" nil t t)
;(setq TeX-auto-save t)
;(setq TeX-parse-self t)


;; Load Enhanced Carbon Emacs plugin
(unless (or (boundp 'enhanced-carbon-emacs)
	    (boundp 'aquamacs-version))
  (defun load-local-site-start (site-lisp-directory)
    "Load site-start.el from a given site-lisp directory"
    (let ((current-default-directory default-directory))
      (setq default-directory site-lisp-directory)
      (normal-top-level-add-subdirs-to-load-path)
      (setq default-directory current-default-directory)
      (setq load-path (cons site-lisp-directory load-path))
      (load (concat site-lisp-directory "/site-start.el"))
      ))
  (load-local-site-start 
   "/Library/Application Support/emacs/ec-emacs/site-lisp"))


(setq       TeX-command-list '(("BibTeX" "bibtex %s" TeX-run-BibTeX nil nil)
                               ("BibTeX all" "~/konfigurationsdateien/bin/bibtexall %s" TeX-run-BibTeX nil nil)
                               ("LaTeX" "latex '\\nonstopmode\\input{%t}'" TeX-run-LaTeX nil t)
                               ("LaTeX Interactive" "latex %t" TeX-run-interactive nil t)
                               ("TeX" "tex %t" TeX-run-TeX nil "nil")
                               ("pdfTeX" "pdftex %t" TeX-run-TeX nil "nil")
                               ("pdfLaTeX" "pdflatex '\\nonstopmode\\input{%t}'" TeX-run-LaTeX nil t)
                               ("pdfLaTeX write18" "pdflatex -shell-escape '\\nonstopmode\\input{%t}'" TeX-run-LaTeX nil t)
                               ("pdfLaTeX Interactive" "pdflatex %t" TeX-run-interactive nil nil)
                               ("dvips" "dvips -Ppdf -G0 %d -o %f" TeX-run-command nil nil)
                               ("makeindex" "makeindex %s" TeX-run-command nil t)
                               ("makeinfo" "makeinfo %t" TeX-run-compile nil t)
                               ("ps2pdf" "ps2pdf14 -sPAPERSIZE=letter %f" TeX-run-command nil nil)
                               ("View" "%V" TeX-run-background nil nil)
                               ("View PDF with gs" "kghostview %s.pdf" TeX-run-background nil nil)
                               ("View PostScript" "kghostview %f" TeX-run-background nil nil)
                               ("Print" "%p " TeX-run-command t nil)
                               ("Spell check" "ispell %s" TeX-run-ispell nil nil)
                               ("Other" "" TeX-run-command t t)))
