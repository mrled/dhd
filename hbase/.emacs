; mrl's emacs file

;    http://bc.tech.coop/emacs.html
;    http://homepages.inf.ed.ac.uk/s0243221/emacs/
;    http://www.student.northpark.edu/pemente/emacs_tabs.htm

; "If you are an idiot, you should use Emacs." 

; my vars:
(setq host-name (nth 0 (split-string system-name  "\\."))) ; emacs doesnt set by default? 

(setq mrl-home (if (getenv "HOME") ; need this to work on Windows and Unix :)
                   (getenv "HOME")
                 (getenv "USERPROFILE")))
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
(add-to-load-path-if-exists "C:/opt/site-lisp")

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
      truncate-partial-width-windows nil ; do NOT change behavior of truncate-lines (see toggle-truncate-lines) when working in C-x 3 horizontally split windows
      vc-follow-symlinks t       ; don't ask ARE YOU SURE if symlink->version-controlled file
      display-time-24hr-format t
      display-time-day-and-date t)
(fset 'yes-or-no-p 'y-or-n-p) ; "yes or no" = "y or n"
(line-number-mode 1) ;; Show line-number in the mode line
(column-number-mode 1) ;; Show column-number in the mode line
(show-paren-mode t) ;; show matching paren when your curser is on a paren
(global-font-lock-mode t) ;; syntax

; use a different version of python than the default in python-shell (and elsewhere?)
(if (file-exists-p "/usr/local/bin/python3")
    (setq python-python-command "/usr/local/bin/python3")
  (if (file-exists-p "~/opt/homebrew/bin/python3")
      (setq python-python-command "~/opt/homebrew/bin/python3")))

(when (eq system-type 'windows-nt) ; windows-specific settings & overrides for python
  (if (file-exists-p "C:/Python32/python.exe")
      (setq python-python-command "C:/Python32/python.exe")
    (if (file-exists-p "C:/Python/python.exe")
        (setq python-python-command "C:/Python/python.exe"))))

(require 'python)

(require 'motion-and-kill-dwim)
(require 'hide-lines)
(require 'tail)
(require 'highlight-tail)
(require 'apache-mode)

; I feel like it should do this for me, ugh
(server-start)

;;;;; tramp shit
(setq tramp-default-method "ssh")
; this next line: you can `C-xC-f /sudo:root@host:/path/to/file` and it will 
; ssh to the host using your default user, then run sudo, then find file. 
(setq mrl/tramp-sudo-proxy (quote ((".*" "\\`root\\'" "/ssh:%h:")))) 
(when (eq system-type 'windows-nt) ; windows-specific settings & overrides for tramp
  (setq tramp-default-method "plink")
  (setq mrl/tramp-sudo-proxy (quote ((".*" "\\`root\\'" "/plink:%h:")))))
(set-default 'tramp-default-proxies-alist mrl/tramp-sudo-proxy)

; markdown shit
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.mdwn"     . markdown-mode) auto-mode-alist) ; ikiwiki's extension
      auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist) ; Github's extension
      auto-mode-alist (cons '("\\.text"     . markdown-mode) auto-mode-alist) ; Gruber's extension
      auto-mode-alist (cons '("\\.mkd"      . markdown-mode) auto-mode-alist) ; VS Markdown Mode
      auto-mode-alist (cons '("\\.md"       . markdown-mode) auto-mode-alist) ; MarkdownPad, others
      auto-mode-alist (cons '("\\.mdown"    . markdown-mode) auto-mode-alist) ; MarkdownPad
      markdown-command (concat mrl-home "/.dhd/opt/bin/Markdown.pl")
      markdown-css-path (concat mrl-home "/.dhd/doc/css/mrl-swiss.css"))

; because markdown-mode + longlines-mode = fucked up [return] key
(add-hook 'markdown-mode-hook
          (function (lambda ()
                      (local-set-key [return] 'newline))))
;; I'll probably be interested in flyspell and longlines if I'm in markdown...
;(add-hook 'markdown-mode-hook 'flyspell-mode)

(setq ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=ultra"))



;(add-hook 'markdown-mode-hook 'longlines-mode)
(global-set-key (kbd "C-c C-l") 'longlines-mode)
(global-set-key (kbd "C-c l")   'longlines-mode)
(setq line-move-visual nil) ; necessary I think b/c of something markdown-mode does
; Marked (Mac OS X only):
(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command 
   (format "open -a /Applications/Marked.app %s" 
       (shell-quote-argument (buffer-file-name))))
)
(global-set-key "\C-cm" 'markdown-preview-file)


; ikiwiki stuff
(setq younix-blog-dir "~/Documents/yus")
(defun iki/new-blog-post ()
  "Creates a new younix.us/blog post with a temporary name."
  (interactive)
  (find-file (concat younix-blog-dir "/soc/" (format-time-string "%Y%m%d") "-tmp.markdown")))
(defun iki/get-title ()
  "Read the contents of the current file and return the title specified in [[!m\
eta title=\"\"]]"
  (interactive)
  (string-match ".*\\[\\[!meta title=\"\\(.*\\)\"\\]\\]" (buffer-string))
  (setq title-regexp
        (match-string-no-properties 1 (buffer-string))))
(defun iki/urlify-title ()
  "Returns a filename based on what iki/get-title returns. Alphanumerics, _, and - are left as-is, blanks are converted to -, and everything else is stripped out."
  (concat 
   (downcase
    (replace-regexp-in-string "[^-_a-zA-Z0-9]" "" 
                              (replace-regexp-in-string "[ 	]" "-" (iki/get-title))))
   ".markdown"))
(defun iki/rename-to-title ()
  "Renames current buffer and associated file to the result of iki/urlify-title"
  (interactive)
  (rename-file-and-buffer (iki/urlify-title)))
(defun iki/insert-meta-title ()
  (interactive)
  (insert "[[!meta title=\"\"]]"))
(defun iki/insert-meta-date ()
  (interactive)
  ;(insert (concat "[[!meta date=\"" (format-time-string "%Y%m%d") "\"]]")))
  (insert (concat "[[!meta date=\"\"]]")))
(defun iki/insert-directive-tag ()
  (interactive)
  (insert "[[!tag]]"))
(global-set-key "\C-cir" 'iki/rename-to-title)
(global-set-key "\C-cit" 'iki/insert-meta-title)
(global-set-key "\C-cid" 'iki/insert-meta-date)
(global-set-key "\C-ciy" 'iki/insert-directive-tag)


; from stevey:   
(defun rename-file-and-buffer (new-name)
 "Renames both current buffer and file it's visiting to NEW-NAME." 
 (interactive "sNew name: ")
 (let ((name (buffer-name))
       (filename (buffer-file-name)))
   (if (not filename)
       (message "Buffer '%s' is not visiting a file!" name)
     (if (get-buffer new-name)
         (message "A buffer named '%s' already exists!" new-name)
       (progn   (rename-file name new-name 1)   (rename-buffer new-name)    
                (set-visited-file-name new-name)    (set-buffer-modified-p nil))))))
(defun move-buffer-file (dir)
 "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
 (let* ((name (buffer-name))
        (filename (buffer-file-name))
        (dir
         (if (string-match dir "\\(?:/\\|\\\\)$")
             (substring dir 0 -1) dir))
        (newname (concat dir "/" name)))
   (if (not filename)
       (message "Buffer '%s' is not visiting a file!" name)
     (progn (copy-file filename newname 1) (delete-file filename) 
            (set-visited-file-name newname) (set-buffer-modified-p nil) t))))

(defun fix-amazon-url ()
  "Minimizes the Amazon URL under the point.  You can paste an Amazon
URL out of your browser, put the cursor in it somewhere, and invoke
this method to convert it. Via: <http://sites.google.com/site/steveyegge2/saving-time>"
  (interactive)
  (and (search-backward "http://www.amazon.com" (point-at-bol) t)
       (search-forward-regexp
	".+/\\([A-Z0-9]\\{10\\}\\)/[^[:space:]\"]+" (point-at-eol) t)
       (replace-match
	(concat "http://www.amazon.com/o/asin/"
		(match-string 1)
		(match-string 3)
        "&tag=younixus-20"))))



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



(when (eq system-type 'windows-nt)
  ; some things are useful to have here just in case they're not in your system %PATH%
  (add-to-list 'exec-path "C:/Program Files/PuTTY")
  (add-to-list 'exec-path "C:/Program Files (x86)/PuTTY")
  (add-to-list 'exec-path "C:/opt/UnxUtils")
  (setq pr-gs-command "c:\\Program Files\\gs\\gs8.54\\bin\\gswin32c.exe"
        pr-gv-command "C:\\Program Files\\Ghostgum\\gsview\\gsview32.exe"
        w32-pass-apps-to-system nil ; let Emacs interpret meta keys
        w32-apps-modifier 'hyper) ;; Menu key -> Hyper
)

; Note: on OS X, it reads initial path info from your .MacOSX/Environment.plist file, not .bashrc!
(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/sw/bin")
  (add-to-list 'exec-path "~/opt/bin")
  (setq mac-option-modifier 'alt
        mac-command-modifier 'meta
        mac-allow-anti-aliasing nil)
  (global-set-key "\M-h" 'ns-do-hide-emacs))
;(when (eq window-system 'x) ...)

(unless (eq window-system nil) ;if we are NOT running in the console

  (setq default-frame-alist ; this actually sets the font and colours
    (list
      (cons 'foreground-color  "white")
      (cons 'background-color  "black")
      (cons 'cursor-color'  "green")))
  (setq initial-frame-alist default-frame-alist)

  (if (member "ProFontX" (font-family-list))
      (set-default-font "ProFontX-9")
    (if (member "Terminus" (font-family-list))
        (set-default-font "Terminus-8")))
  (tool-bar-mode 0)    ; this just gets rid of the silly toolbar w/ icons below the menu bar

  (global-hl-line-mode t) ;; Highlight the current line. 
  (set-face-background 'hl-line "#335")
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

(global-set-key "\C-xs" 'save-buffer) ; so tired of 'save-some-buffers, the default

(global-set-key "\M-`" 'other-frame) ; mimic the way macosx switches between windows of the same application


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


; sprunge.us owns
(defun sprunge (prefix)
  "Posts the current buffer to sprunge, and shows the resulting URL in a new buffer"
  (interactive "P")
  (let ((filename "/tmp/sprunge-post"))
    (if prefix (write-file filename) (write-region (region-beginning) (region-end) filename)) ; if invoked with the universal argument / prefix, upload the whole file, else upload just the region
    (insert (shell-command-to-string (concat "curl -s -F 'sprunge=<" filename "' http://sprunge.us")))
    (delete-char -1))) ; Newline after URL
