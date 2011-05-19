; mrl's emacs file

;    http://bc.tech.coop/emacs.html
;    http://homepages.inf.ed.ac.uk/s0243221/emacs/
;    http://www.student.northpark.edu/pemente/emacs_tabs.htm

; "If you are an idiot, you should use Emacs." 

; my vars:
(setq host-name (nth 0 (split-string system-name  "\\."))) ; emacs doesnt set by default? 

;; paths that Emacs should look for executables, since (LAME) bashrc isn't being read. 
(setq exec-path (split-string ":/bin:/sbin:/usr/bin:/usr/local/bin:/usr/sbin:/usr/local/sbin:/opt/local/bin:/opt/local/sbin:/sw/bin:/sw/sbin:~/opt/bin" path-separator))
(setenv "PATH" (mapconcat 'identity exec-path ":"))

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
(add-to-load-path-if-exists "~/opt/share/emacs")
(add-to-load-path-if-exists "~/opt/share/emacs/site-lisp")
(add-to-load-path-if-exists "~/doc/dhd/opt/emacs")
(add-to-load-path-if-exists (concat "~/doc/dhd/host/" host-name "/emacs/"))
(add-to-load-path-if-exists "~/.dhd/opt/emacs")
(add-to-load-path-if-exists (concat "~/.dhd/host/" host-name "/emacs/"))
;(add-to-load-path-if-exists "~/doc/remote/dhd/hbase/emacs")
(add-to-load-path-if-exists "~/doc/uenc/emacs")
(add-to-load-path-if-exists "/usr/local/share/emacs/site-lisp")
(add-to-load-path-if-exists "/usr/local/share/emacs/site-lisp/w3m")
(add-to-load-path-if-exists "/usr/share/emacs/site-lisp/apel")
(add-to-load-path-if-exists "/usr/share/emacs/site-lisp/flim")
(add-to-load-path-if-exists "/usr/share/emacs/site-lisp/semi")
(add-to-load-path-if-exists "/usr/share/emacs/site-lisp/wl")
(add-to-load-path-if-exists "/usr/local/share/emacs/site-lisp/erc")
(add-to-load-path-if-exists "~/opt/src/zenburn-el")

; settings (not custom variables)
;;;; fix the visible bell! w/ ring-bell-function or something
(setq visible-bell t              ; Is this vi? Should there be beeping? 
      inhibit-startup-message t   ; inhibit startup
      initial-scratch-message nil ; inhibit splash
      make-backup-files t         ; Enable backup files
      version-control t           ; Enable backup versioning 
      backup-directory-alist (quote ((".*" . "~/Backup/emacs/"))) ; Save backups
      delete-old-versions t       ; don't ask me to delete old backups, just do it
      mouse-autoselect-window t   ; focus-follows-mouse in WINDOWS, NOT frames
      display-time-24hr-format t
      display-time-day-and-date t
      vc-follow-symlinks t       ; don't ask ARE YOU SURE if symlink->version-controlled file
      truncate-partial-width-windows nil) ; do NOT change behavior of truncate-lines (see toggle-truncate-lines) when working in C-x 3 horizontally split windows
(fset 'yes-or-no-p 'y-or-n-p) ; "yes or no" = "y or n"
(line-number-mode 1) ;; Show line-number in the mode line
(column-number-mode 1) ;; Show column-number in the mode line
(show-paren-mode t) ;; show matching paren when your curser is on a paren
(global-font-lock-mode t) ;; syntax

(require 'motion-and-kill-dwim)
(require 'hide-lines)
(require 'tail)
(require 'highlight-tail)

; I feel like it should do this for me, ugh
(server-start)


; markdown shit
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.mdwn" . markdown-mode) auto-mode-alist))

; because markdown-mode + longlines-mode = fucked up [return] key
(add-hook 'markdown-mode-hook
          (function (lambda ()
                      (local-set-key [return] 'newline))))
;; I'll probably be interested in flyspell and longlines if I'm in markdown...
;(add-hook 'markdown-mode-hook 'flyspell-mode)
;(add-hook 'markdown-mode-hook 'longlines-mode)
(global-set-key (kbd "C-c C-l") 'longlines-mode)
(global-set-key (kbd "C-c l")   'longlines-mode)
(setq line-move-visual nil) ; necessary I think b/c of something markdown-mode does

; irc
;(load-file "~/doc/uenc/hbase/ercrc.el")

;; w3/w3m stuff
;(require 'w3m-load)
;(require 'mime-w3m) 
(setq w3m-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8
      w3m-use-cookies t
      browse-url-browser-function 'w3m-browse-url
      w3m-use-title-buffer-name t)                ; html title is buffer name
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

; this lets you do C-c y (and C-c C-y) to invoke a YubNub command with 
; emacs-w3m in the current w3m buffer, or, if the current buffer is not
; a w3m buffer, the last-opened w3m buffer. 
; If you prefix it with the universal argument (C-u C-c y), it will 
; use a *clone* of the buffer (just like doing C-c C-t from within w3m)
; It would probably be ideal if that were not the case, but ah well. 
; Note that this is a function of browse-url.
; from <http://www.yubnub.org/yubnub-emacs.txt>
(defun yubnub (command)
  "Use `browse-url' to submits a command to yubnub and opens
;; result in an external browser defined in `browse-url-browser-function'.

To get started  `M-x yubnub <RET> ls <RET>' will return a list of 
all yubnub commands."
  (interactive "sYubNub: ")
  (browse-url 
   (concat "http://yubnub.org/parser/parse?command=" command)))
(global-set-key "\C-cy"    'yubnub)
(global-set-key "\C-c\C-y" 'yubnub)


;; optional keyboard short-cut
; (global-set-key "\C-xm" 'browse-url-at-point)


; for the love of mercy, indent the same way every time!
(setq-default indent-tabs-mode nil) ; only ever use regular spaces, never tab
(setq default-tab-width 4) ; when tab char on disk, display as 4 chars wide
(define-key text-mode-map (kbd "TAB") 'tab-to-tab-stop) ; [TAB]key = tab2tab-stop
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)) ;; tab = 4 spaces, not 8

; When opening two files of the same name from different directories
; like me/file.txt and you/file.txt
; default is to have file.txt<1> and file.txt<2>
; this makes it so you have file.txt:me and file.txt:you - rad.
(require 'uniquify) 
(setq 
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")



(when (eq window-system 'w32)
  (setq pr-gs-command "c:\\Program Files\\gs\\gs8.54\\bin\\gswin32c.exe")
  (setq pr-gv-command "C:\\Program Files\\Ghostgum\\gsview\\gsview32.exe")
;  (defvar myfont "-*-ProFontWindows-normal-r-*-*-12-*-*-*-c-*-*-iso8859-1")) ;;font = ProFontWindows 9pt
   (defvar myfont "-outline-ProFontWindows-normal-normal-normal-mono-12-*-*-*-c-*-iso8859-1"))
;   (defvar myfont "-outline-Consolas-normal-normal-normal-mono-12-*-*-*-c-*-iso8859-1"))


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
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key "\M-h" 'ns-do-hide-emacs)
  (defvar myfont "-apple-profontx-medium-r-normal--9-90-72-72-m-90-iso10646-1"))

(when (eq window-system 'x)
  (defvar myfont 
    ;"-*-profontwindows-medium-r-normal--12-*-0-*-*-*-iso8859-1"))
    ;"-unknown-ProFontX-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"))
    "-unknown-ProFont-normal-normal-normal-*-11-*-*-*-m-*-iso10646-1")

    ;; for stumpwm
    (defvar stumpwm-shell-program "~/opt/src/stumpwm/contrib/stumpish")
    (require 'stumpwm-mode))

(unless (eq window-system nil) ;if we are NOT running in the console
  (setq default-frame-alist ; this actually sets the font and colours
    (list
      (cons 'font  myfont)
      (cons 'foreground-color  "white")
      (cons 'background-color  "black")
      (cons 'cursor-color'  "green")))
  (setq initial-frame-alist default-frame-alist)
  (tool-bar-mode 0)    ; this just gets rid of the silly toolbar w/ icons below the menu bar
  ;(menu-bar-mode nil)  ; this used to do nothing under osx but since emacs23 it DOES, so define it in a window-system section above instead
  (global-hl-line-mode t) ;; Highlight the current line. 
  (set-face-background 'hl-line "#335")     ;; Emacs 22 Only
  ;(set-face-background 'highlight "#330")  ;; Emacs 21 Only
  )

;; keybindings
; http://steve.yegge.googlepages.com/effective-emacs
;(global-set-key "\C-w" 'backward-kill-word) ; replaces the kill-region default
;(global-set-key "\C-c\C-w" 'kill-region)    ; rebinds kill-region to my space
; you should use the "\C-c" "name"space for defining your own keys
;(global-set-key "\C-cr"    'revert-buffer)
(global-set-key "\C-c\C-r" 'revert-buffer)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "C-c C-k") 'copy-region-as-kill)

(global-set-key [(meta down)] 'forward-block-dwim)
(global-set-key [(meta up)]  'backward-block-dwim)


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
(global-set-key "\C-c\C-d" 'wordnet-current-word)

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
;(global-set-key "\C-c" 'backward-kill-word) ; replaces the kill-region default

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


; Search the load path for a file
; <http://www.emacswiki.org/emacs/SearchingLoadPath>
(defmacro project-filter (var condition list)
  `(loop for ,var in ,list when ,condition collect ,var))

(defun scour-load-path-for (filename)
  "Search for a file throughout the load-path"
  (interactive)
  (let ((files (project-filter dir (file-exists-p (directory-file-name dir))
				   load-path)))
    (loop for dir in files
	  when (member filename (directory-files dir))
	  return (format "%s/%s" (directory-file-name dir)
			 filename))))

; this is probably horribly embarrassing but I am so fucking sick of different fucking modes redefinig my fucking spacebar fuck
(defun mrled/eight-fucking-spaces ()
  (interactive)
  (insert "        "))
(defun mrled/four-fucking-spaces ()
  (interactive)
  (insert "        "))
(global-set-key "\C-c\C-t" 'mrled/four-fucking-spaces)
(global-set-key "\C-ct"    'mrled/four-fucking-spaces)
(global-set-key "\C-c\C-T" 'mrled/eight-fucking-spaces)
(global-set-key "\C-cT"    'mrled/eight-fucking-spaces)
