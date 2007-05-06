(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")

(autoload 'psvn "psvn" "SVN for Emacs")
(autoload 'org-install "org-install" "Organization mode for Emacs")
(autoload 'vm "vm" "A more MUA-like MUA for Emacs")

;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/muse/")
(autoload 'muse "muse" "Wiki mode for Emacs")
(require 'muse)
(require 'muse-mode)
(require 'muse-project)
(require 'muse-html)
(require 'muse-latex)
(require 'muse-texinfo)
(require 'muse-docbook)
(require 'muse-book)
(require 'muse-blosxom)

;; ceded (emacs ide-like features) stuff
;(if (file-exists-p (setq cedet-file (concat home-load-path "cedet/common/cedet.elc")))
;  (load-file cedet-file)
;  (semantic-load-enable-code-helpers))

;; lisp programming stuff
;; (if (file-exists-p (setq slime-path (concat home-load-path "slime/")))
;;   (add-to-list 'load-path slime-path)
;;   (setq inferior-lisp-program "/usr/local/bin/openmcl")
;;   (require 'slime)
;;   (slime-setup))

z(add-to-list 'load-path "~/opt/emacs/slime-2.0/")
(setq inferior-lisp-program "/usr/local/bin/openmcl")
(require 'slime)
(slime-setup)


(autoload 'slime "slime"
          "Start an inferior^_superior Lisp and connect to its Swank server."
          t)
(autoload 'slime-mode "slime"
          "SLIME: The Superior Lisp Interaction Mode for Emacs (minor-mode)."
          t)

