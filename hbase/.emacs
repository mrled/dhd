; mrl's emacs file
; see:
;    http://bc.tech.coop/emacs.html
;    http://homepages.inf.ed.ac.uk/s0243221/emacs/
;    http://www.student.northpark.edu/pemente/emacs_tabs.htm

; "If you are an idiot, then you should use Emacs." 

; my vars:
(setq host-name (nth 0 (split-string system-name  "\\."))) ; emacs doesnt set by default. CHANGE if it does. 

;; other packages: load these before host-specific emacs file
(add-to-list 'load-path "~/opt/emacs/site-lisp" "/usr/local/share/emacs/site-lisp")

; if I have a host-specific emacs file, load it. 
(if (file-exists-p 
  (setq host-specific-init-file (concat "~/doc/remote/dhd/host/" host-name "/emacs.el")))
    (load-file host-specific-init-file))

; settings (not custom variables)
(setq visible-bell t              ; Is this vi? Should there be beeping? 
      inhibit-startup-message t   ; inhibit startup
      initial-scratch-message nil ; inhibit splash
      make-backup-files t         ; Enable backup files
      version-control t           ; Enable backup versioning 
      backup-directory-alist (quote ((".*" . "~/Backup/emacs/"))) ; Save bckups
      delete-old-versions t       ; don't ask me to delete old backups, just do it
      mouse-autoselect-window t   ; focus-follows-mouse in WINDOWS, NOT frames
      display-time-24hr-format t
      display-time-day-and-date t)
(fset 'yes-or-no-p 'y-or-n-p) ; "yes or no" = "y or n"
(line-number-mode 1) ;; Show line-number in the mode line
(column-number-mode 1) ;; Show column-number in the mode line
(show-paren-mode t) ;; show matching paren when your curser is on a paren
(tool-bar-mode 0)
(global-font-lock-mode t) ;; syntax highlighting
(menu-bar-mode nil) ;; menu bars suck (i wonder how this works under os x?)
(global-hl-line-mode t) ;; Highlight the current line. 
(set-face-background 'hl-line "#335")     ;; Emacs 22 Only
;(set-face-background 'highlight "#330")  ;; Emacs 21 Only

;; trying to make Info behave
(require 'info)
(add-to-list 'Info-directory-list "/usr/share/emacs/info")
;; "/usr/local/share/emacs/info" "/opt/csw/share/info")
(setq Info-directory-list
 (cons (expand-file-name "/usr/share/emacs/info")
       Info-directory-list))

;; key shortcuts
; http://steve.yegge.googlepages.com/effective-emacs
(global-set-key "\C-w" 'backward-kill-word) ; replaces the kill-region default
(global-set-key "\C-c\C-w" 'kill-region)    ; rebinds kill-region to my space
; you should use the "\C-c" "name"space for defining your own keys
;(global-set-key "\C-cr"    'revert-buffer)
(global-set-key "\C-c\C-r" 'revert-buffer)

; for the love of mercy, indent the same way every time!
(setq-default indent-tabs-mode nil) ; only ever use regular spaces, never tab
(setq default-tab-width 4) ; when tab char on disk, display as 4 chars wide
(define-key text-mode-map (kbd "TAB") 'tab-to-tab-stop) ; [TAB]key = tab2tab-stop
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)) ;; tab = 4 spaces, not 8




(when (eq system-type 'windows-nt)
  (setq pr-gs-command "c:\\Program Files\\gs\\gs8.54\\bin\\gswin32c.exe")
  (setq pr-gv-command "C:\\Program Files\\Ghostgum\\gsview\\gsview32.exe")
  (defvar myfont "-*-ProFontWindows-normal-r-*-*-12-*-*-*-c-*-*-iso8859-1")) ;;font = ProFontWindows 9pt
(when (eq system-type 'Interix) ; I use Xming, and I add the Windows font path to Xming's font path; this profont is the same as the profont above, so as long as I've installed ProFontWindows and can use it, this should work too
  (defvar myfont "-*-profontwindows-medium-r-normal--*-*-0-*-*-*-iso8859-1"))
(when (eq window-system 'mac)
  (add-to-list 'exec-path "/sw/bin") ;add fink's path
;  (setq mac-option-modifier 'meta)
;  (setq mac-command-key-is-meta 'alt) ;wait what does this do again
;  (setq mac-command-modifier 'alt) ;otherwise cmd AND opt are meta. 
  (modify-frame-parameters (selected-frame) '((active-alpha . 0.9))) ;transparency - foreground
  (modify-frame-parameters (selected-frame) '((inactive-alpha . 0.9))) ;transparency - background
  (defvar myfont "-apple-profontx-medium-r-normal--9-90-72-72-m-90-iso10646-1"))
(when (eq window-system 'x)
  (defvar myfont "-*-profontwindows-medium-r-normal--*-*-0-*-*-*-iso8859-1"))

(unless (eq window-system nil) ;if we are NOT running in the console
  (setq default-frame-alist ; this actually sets the font and colours
    (list
      (cons 'font  myfont)
      (cons 'foreground-color  "white")
      (cons 'background-color  "black")
      (cons 'cursor-color'  "green")))
  (setq initial-frame-alist default-frame-alist)
)


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

;; no-word: use antiword to view .doc in emacs
(autoload 'no-word "no-word" "word to txt")
(add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))

;; my own functions
(defun insert-time ()
  (interactive)
  (insert (format-time-string "%Y%m%d-%H%M")))
(defun insert-time-spacey ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))
(defun insert-time-long ()
  (interactive)
  (insert (format-time-string "%Y%m%d-%H%M%S")))
(defun insert-date ()
  (interactive)
  (insert (format-time-string "%Y%m%d")))
(defun insert-time-blos ()
  (interactive) 
  (insert (format-time-string "%Y-%m-%d-%H-%M")))

;; just for a time, while I need to do this a lot at Neuric
; inserts the example text so I don't have to type it out a hojillion time
(defun insert-eg ()
  (interactive) 
  (insert "// Example: "))
(global-set-key "\C-c\C-e" 'insert-eg)

