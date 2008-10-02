; mrl's emacs file
; see:
;    http://bc.tech.coop/emacs.html
;    http://homepages.inf.ed.ac.uk/s0243221/emacs/
;    http://www.student.northpark.edu/pemente/emacs_tabs.htm

; "If you are an idiot, then you should use Emacs." 

; my vars:
(setq host-name (nth 0 (split-string system-name  "\\."))) ; emacs doesnt set by default. CHANGE if it does. 

;; Add the given path to the load-path variable.
(defun add-to-load-path (path-string)
  (message (format "Passed %S..." path-string))
  (if (stringp path-string)
      (when (file-exists-p path-string)
        (message (format "Adding %S to load-path..." path-string))
        (add-to-list 'load-path (expand-file-name path-string)))
    (crs-add-to-load-path (car path-string))
    (if (cdr path-string)
        (crs-add-to-load-path (cdr path-string)))))
(defun add-to-load-path-if-exists (dir)
     (if (file-exists-p (expand-file-name dir))
         (add-to-load-path (expand-file-name dir))))
(add-to-load-path-if-exists "~/opt/emacs/site-lisp")
(add-to-load-path-if-exists "~/doc/remote/dhd/hbase/emacs")
(add-to-load-path-if-exists "/usr/local/share/emacs/site-lisp")

; if I have a host-specific emacs file, load it. 
;(if (file-exists-p 
;  (setq host-specific-init-file (concat "~/doc/remote/dhd/host/" host-name "/emacs.el")))
;    (load-file host-specific-init-file))
(if (file-exists-p 
  (setq elisprc "~/doc/remote/dhd/hbase/emacs/eshellrc.el"))
    (load-file elisprc))

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

;; trying to make Info behave
(require 'info)
(add-to-list 'Info-directory-list "/usr/share/emacs/info")
;; "/usr/local/share/emacs/info" "/opt/csw/share/info")
(setq Info-directory-list
 (cons (expand-file-name "/usr/share/emacs/info")
       Info-directory-list))

;(require 'motion-and-kill-dwim)


; for the love of mercy, indent the same way every time!
(setq-default indent-tabs-mode nil) ; only ever use regular spaces, never tab
(setq default-tab-width 4) ; when tab char on disk, display as 4 chars wide
(define-key text-mode-map (kbd "TAB") 'tab-to-tab-stop) ; [TAB]key = tab2tab-stop
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)) ;; tab = 4 spaces, not 8


(when (eq window-system 'w32)
  (setq pr-gs-command "c:\\Program Files\\gs\\gs8.54\\bin\\gswin32c.exe")
  (setq pr-gv-command "C:\\Program Files\\Ghostgum\\gsview\\gsview32.exe")
  (defvar myfont "-*-ProFontWindows-normal-r-*-*-12-*-*-*-c-*-*-iso8859-1")) ;;font = ProFontWindows 9pt

  ; let Emacs use the special win keys, don't pass them to the OS
  ; you can also use :
  ;      w32-pass-lwindow-to-system nil 
  ;      w32-pass-rwindow-to-system nil 
  ;      w32-lwindow-modifier 'super ;; Left Windows key 
  ;      w32-rwindow-modifier 'super ;; Right Windows key 
  ; to use this under Interix, you should bind the win keys in your .xinitrc somehow I think
  (setq w32-pass-apps-to-system nil 
        w32-apps-modifier 'hyper) ;; Menu key 

 (when (eq system-type 'Interix) 
  ; I use Xming, and I add the Windows font path to Xming's font path; this profont is the same as the profont above, 
  ; so as long as I've installed ProFontWindows and can use it, this should work too
  (defvar myfont "-*-profontwindows-medium-r-normal--*-*-0-*-*-*-iso8859-1"))

;; now I also need 
(when (or (eq window-system 'mac) (eq window-system 'ns))
  (add-to-list 'exec-path "/sw/bin") ;add fink's path
  (setq mac-option-modifier 'meta)
  (setq mac-command-key-is-meta 'alt) ;wait what does this do again
  (setq mac-command-modifier 'alt) ;otherwise cmd AND opt are meta. 
  (modify-frame-parameters (selected-frame) '((active-alpha . 0.9))) ;transparency - foreground
  (modify-frame-parameters (selected-frame) '((inactive-alpha . 0.9))) ;transparency - background
  (defvar myfont "-apple-profontx-medium-r-normal--9-90-72-72-m-90-iso10646-1"))

(when (eq window-system 'x)
  (defvar myfont 
    ;"-*-profontwindows-medium-r-normal--12-*-0-*-*-*-iso8859-1"))
    ;"-unknown-ProFontX-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"))
    "-unknown-ProFont-normal-normal-normal-*-11-*-*-*-m-*-iso10646-1"))

(unless (eq window-system nil) ;if we are NOT running in the console
  (setq default-frame-alist ; this actually sets the font and colours
    (list
      (cons 'font  myfont)
      (cons 'foreground-color  "white")
      (cons 'background-color  "black")
      (cons 'cursor-color'  "green")))
  (setq initial-frame-alist default-frame-alist)
  (set-face-background 'hl-line "#335")     ;; Emacs 22 Only
  ;(set-face-background 'highlight "#330")  ;; Emacs 21 Only
  )

;; keybindings
; http://steve.yegge.googlepages.com/effective-emacs
(global-set-key "\C-w" 'backward-kill-word) ; replaces the kill-region default
(global-set-key "\C-c\C-w" 'kill-region)    ; rebinds kill-region to my space
; you should use the "\C-c" "name"space for defining your own keys
;(global-set-key "\C-cr"    'revert-buffer)
(global-set-key "\C-c\C-r" 'revert-buffer)
(global-set-key (kbd "C-c o") 'occur)

(global-set-key [(meta down)] 'forward-block-dwim)
(global-set-key [(meta up)]  'backward-block-dwim)

(global-set-key "\C-c\C-d" 'wordnet-current-word)



;; no-word: use antiword to view .doc in emacs
(autoload 'no-word "no-word" "word to txt")
(add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))


;; For use with WordNet
(defun get-current-word ()
  "Returns the current, or the last entered word."
  (save-excursion
    (backward-word)
    (setq start (point))
    (forward-word)
    (setq end (point))
    (buffer-substring-no-properties start end)))

(defvar wordnet-bin-path
  "C:/Programs/WordNet/2.1/bin/wn.exe"
  "This should point to the full path of the wordnet command")

(defun wordnet-current-word ()
  "Shows the Wordnet overview for the current word."
  (interactive)
  (save-window-excursion
    (let ((buf (get-buffer-create "*wordnet*"))
          (word (get-current-word)))
      (save-window-excursion
        (set-buffer buf)
        (clear-buffer buf)
        (insert (concat "Wordnet overview for " word ": "))
        (call-process wordnet-bin-path nil "*wordnet*" t word "-over")
        (switch-to-buffer "*wordnet*")
        (beginning-of-buffer)
        (read-string "Press Enter to continue...")))))

(defun clear-buffer (buf)
  "Clear a buffer"
  (save-excursion
    (set-buffer buf)
    (kill-region (point-min) (point-max))))

;; from the illustrious & preeminent Sacha Chua
(defun sacha/isearch-yank-current-word ()
  "Pull current word from buffer into search string."
  (interactive)
  (save-excursion
    (skip-syntax-backward "w_")
    (isearch-yank-internal
     (lambda ()
       (skip-syntax-forward "w_")
       (point)))))
(global-set-key "\C-c" 'backward-kill-word) ; replaces the kill-region default



;; my own functions
(defun mrled/insert-time ()
  (interactive)
  (insert (format-time-string "%Y%m%d-%H%M")))
(defun mrled/insert-time-spacey ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))
(defun mrled/insert-time-long ()
  (interactive)
  (insert (format-time-string "%Y%m%d-%H%M%S")))
(defun mrled/insert-date ()
  (interactive)
  (insert (format-time-string "%Y%m%d")))
(defun mrled/insert-time-blos ()
  (interactive) 
  (insert (format-time-string "%Y-%m-%d-%H-%M")))



