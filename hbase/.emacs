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

;;;; for the love of mercy, indent the same way every time!
(setq-default indent-tabs-mode nil) ; only ever use regular spaces, never tab character
(setq default-tab-width 4) ; when i read tab char from disk, display as 4 chars wide
(define-key text-mode-map (kbd "TAB") 'tab-to-tab-stop) ; bind [TAB] key to tab-to-tab-stop
;; set the tab stop list such that a tab = 4 spaces, not 8
(setq tab-stop-list '(4 8 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

(line-number-mode 1) ;; Show line-number in the mode line
(column-number-mode 1) ;; Show column-number in the mode line

;(set-default-font "-*-ProFontX-normal-r-*-*-12-*-*-*-c-*-*-iso8859-1") ;;font = ProFont 9pt
(modify-frame-parameters (selected-frame) '((active-alpha . 0.9)))     ;; alpha transparency - foreground
(modify-frame-parameters (selected-frame) '((inactive-alpha . 0.7)))   ;; alpha transparency - background

;; See if we're on MS Windows or Mac OS X
(defvar mswindows-p (string-match "windows" (symbol-name system-type)))
(defvar macosx-p (string-match "darwin" (symbol-name system-type)))

;(if mswindows-p
;  (global-font-lock-mode t) ;; syntax highlighting?
;  (set-default-font "-*-ProFontWindows-normal-r-*-*-12-*-*-*-c-*-*-iso8859-1") ;;font = ProFontWindows 9pt
;  (set-cursor-color "green") ;; the blinking text insertion cursor thing
;  (set-face-background 'region "blue") ;; Set region background color
;  (set-background-color "black") ;; set bg
;  (set-foreground-color "white") ;; set fg
;  (defun eshell/op (FILE)
;    "Invoke (w32-shell-execute \"Open\" FILE) and substitute slashes for backslashes"
;    (w32-shell-execute "Open" (substitute ?\\ ?/ (expand-file-name FILE)))
;    )
;  )
