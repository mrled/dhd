;; mrl's emacs file
; see:
;    http://bc.tech.coop/emacs.html
;    http://homepages.inf.ed.ac.uk/s0243221/emacs/
;    http://www.student.northpark.edu/pemente/emacs_tabs.htm

(setq inhibit-startup-message t)   ; inhibit startup
(setq initial-scratch-message nil) ; inhibit splash
(fset 'yes-or-no-p 'y-or-n-p) ; Make all "yes or no" prompts show "y or n" instead
(setq make-backup-files t) ; Enable backup files.
(setq version-control t) ; Enable backup versioning 
(setq backup-directory-alist (quote ((".*" . "~/Backup/emacs/")))) ;; Save all backups here
(setq delete-old-versions t) ; don't ask me to delete old backups, just do it
(setq mouse-autoselect-window t) ; focus-follows-mouse. NOT frames... just for emacs' WINDOWS only. 
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(line-number-mode 1) ;; Show line-number in the mode line
(column-number-mode 1) ;; Show column-number in the mode line
(tool-bar-mode 0)

; for the love of mercy, indent the same way every time!
(setq-default indent-tabs-mode nil) ; only ever use regular spaces, never tab character
(setq default-tab-width 4) ; when i read tab char from disk, display as 4 chars wide
(define-key text-mode-map (kbd "TAB") 'tab-to-tab-stop) ; bind [TAB] key to tab-to-tab-stop
;; set the tab stop list such that a tab = 4 spaces, not 8
(setq tab-stop-list '(4 8 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

;(set-default-font "-*-ProFontX-normal-r-*-*-12-*-*-*-c-*-*-iso8859-1") ;;font = ProFont 9pt
(global-font-lock-mode t) ;; syntax highlighting?
(modify-frame-parameters (selected-frame) '((active-alpha . 0.9)))     ;; alpha transparency - foreground
(modify-frame-parameters (selected-frame) '((inactive-alpha . 0.7)))   ;; alpha transparency - background

(when (eq system-type 'windows-nt)
  (setq pr-gs-command "c:\\Program Files\\gs\\gs8.54\\bin\\gswin32c.exe")
  (setq pr-gv-command "C:\\Program Files\\Ghostgum\\gsview\\gsview32.exe")
  ;; Windows PowerShell - THIS DOESN'T WORK (WELL)
  ;(require 'powershell-mode) ; powershell script editing mode (entirely optional)
  ;(setq exec-path (cons "C:/system32/WindowsPowerShell/v1.0" exec-path)) ; The path to PowerShell
  ;(setq explicit-shell-file-name "powershell")      ; Filename of the PowerShell shell
  ;(setq shell-file-name explicit-shell-file-name)   ; Tell Emacs to use PowerShell
  ;(setq shell-command-switch "-Command")            ; Argument to use when executing a single command
  ;(setq explicit-powershell-args '("-command" "-")) ; Arguments when starting an interactive shell
  (defvar myfont "-*-ProFontWindows-normal-r-*-*-12-*-*-*-c-*-*-iso8859-1")) ;;font = ProFontWindows 9pt

(setq default-frame-alist ; this actually sets the font, as well as colours
  (list
    (cons 'font  myfont)
    (cons 'foreground-color  "white")
    (cons 'background-color  "black")
    (cons 'cursor-color'  "green")))
(setq initial-frame-alist default-frame-alist)


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
		 (const :tag "Ask" t)
		 (const :tag "Always save" always))
  :group 'eshell-hist)
