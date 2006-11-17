(setq make-backup-files nil) 

(require 'w32-symlinks)

;; for cygwin + NTEmacs

;;(require 'cygwin-mount)
;;(cygwin-mount-activate)
;;
;;(let* ((cygwin-root "c:/cygwin")
;;       (cygwin-bin (concat cygwin-root "/bin")))
;;  (setenv "HOME" (concat cygwin-root "/home/mledbetter"))
;;  (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))
;;  (setq exec-path (cons cygwin-bin exec-path)))
;;
;;(setq shell-file-name "bash")
;;(setq explicit-shell-file-name "bash")



;; This assumes that Cygwin is installed in C:\cygwin (the
;; default) and that C:\cygwin\bin is not already in your
;; Windows Path (it generally should not be).
;;
(setq exec-path (cons "C:/cygwin/bin" exec-path))
(setenv "PATH" (concat "C:\\cygwin\\bin;" (getenv "PATH")))
;;
;; NT-emacs assumes a Windows command shell, which you change
;; here.
;;
(setq process-coding-system-alist '(("bash" . undecided-unix)))
(setq shell-file-name "bash")
(setenv "SHELL" shell-file-name) 
(setq explicit-shell-file-name shell-file-name) 
;;
;; This removes unsightly ^M characters that would otherwise
;; appear in the output of java applications.
;;
;;(add-hook 'comint-output-filter-functions
;;          'comint-strip-ctrl-m)
(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(w32-symlinks-handle-shortcuts t))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )
