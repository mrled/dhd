; dont create those dumb file~ files every time we edit something
(setq make-backup-files nil) 

; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;;;; for the love of mercy, indent the same way every time!
;; see: 
;   code/Neuric Technologies/Neuric/Concept Words/
;   http://www.student.northpark.edu/pemente/emacs_tabs.htm
;; make it never use \t; it only uses a regular space: ' '
(setq-default indent-tabs-mode nil)
; this makes the TAB character (ASCII byte #9), when read from disk,
; to be displayed as four characters wide
(setq default-tab-width 4)
;; bind [tab] to tab-to-tab-stop
;;   ... in every mode
;global-set-key (kbd "TAB") 'tab-to-tab-stop);
;;   ... in text  mode
(define-key text-mode-map (kbd "TAB") 'tab-to-tab-stop);
;; set the tab stop list such that a tab = 4 spaces, not 8
(setq tab-stop-list '(4 8 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

;; Show line-number in the mode line
(line-number-mode 1)
;; Show column-number in the mode line
(column-number-mode 1)