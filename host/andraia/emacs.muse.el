;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/muse/")
(autoload 'muse "muse" "Wiki mode for Emacs")
(require 'muse)
(require 'muse-mode)
(require 'muse-project)
(require 'muse-wiki)
(require 'muse-html)
(require 'muse-latex)
(require 'muse-texinfo)
(require 'muse-docbook)
(require 'muse-book)
(require 'muse-blosxom)

(setq muse-project-alist
      '(("web"
         ("~/doc/remote/muse/web" :default "index")
         (:base "html" :path "~/Sites/"))))
