;; eshell stuff
; make C-a go to the beginning of the command line, unless it is already there, in which case 
;  it goes to the real beginning of the line
(defun eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))
(add-hook 'eshell-mode-hook '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-maybe-bol)))
; path and stuff (not sure what it all does... http://www.khngai.com/emacs/eshell.php)
(add-hook 'eshell-mode-hook
   '(lambda nil
;   (let ((path))
;      (setq path ".;c:/program files/microsoft visual studio/vb98/")
;      (setq path (concat path ";d:/program files/microsoft visual studio/vc98/bin"))
;    (setenv "PATH" path))
   (local-set-key "\C-u" 'eshell-kill-input))
 )
(defcustom eshell-ask-to-save-history t
  "*Determine if history should be automatically saved.
History is always preserved after sanely exiting an Eshell buffer.
However, when Emacs is being shut down, this variable determines
whether to prompt the user.
If set to nil, it means never ask whether history should be saved.
If set to t, always ask if any Eshell buffers are open at exit time.
If set to `always', history will always be saved, silently."
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Ask" f)
		 (const :tag "Always save" always))
  :group 'eshell-hist)

;eshell aliases are permanent - they get recorded to this file automatically
(setq eshell-aliases-file "~/doc/remote/dhd/hbase/emacs/eshell-aliases.el")
