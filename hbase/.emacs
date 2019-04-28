;; "If you are an idiot, you should use Emacs." 

;; Add MELPA, per http://melpa.org/#/getting-started
;; See packages with M-x list-packages; mark packages for installation/upgrade with 'i'/'u', then execute with 'x'
;; Note that this require GNUTLS on Windows
;; Note that we expect Emacs 24 or higher
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)
;(package-refresh-contents) ; Results in slower startup, but ensures the later lines actually work
(setq mrled-package-list '(
			   ansible
			   ansible-doc
			   dockerfile-mode
			   jinja2-mode
			   markdown-mode
			   poly-ansible
			   poly-markdown
			   polymode
			   yaml-mode
			   ))
(dolist (package mrled-package-list)
  (unless (package-installed-p package)
    (package-install package)))


; See installed pacakges with C-h v package-activated-list, copy those to this list to install them on startup

;; Save session on close
(require 'desktop)
;; (setq desktop-change-dir "~/.emacs-desktop")
(desktop-save-mode 1)
(defun mrled/save-desktop ()
  ;; Ganked from <https://www.emacswiki.org/emacs/Desktop>
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'mrled/save-desktop)

(setq vc-handled-backends nil) ; I never use Version Control in Emacs, just slows it down

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

;; ; because markdown-mode + longlines-mode = fucked up [return] key
;; (add-hook 'markdown-mode-hook
;;           (function (lambda ()
;;                       (local-set-key [return] 'newline))))

(global-set-key (kbd "C-c C-l") 'longlines-mode)
(global-set-key (kbd "C-c l")   'longlines-mode)
;; (setq line-move-visual nil) ; necessary I think b/c of something markdown-mode does


; ido shit
;; ido makes competing buffers and finding files easier
;; http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings
(require 'ido) 
(ido-mode 'both) ;; for buffers and files
(setq 
;  ido-ignore-buffers ;; ignore these guys
;  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
;     "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
  ido-case-fold  t                 ; be case-insensitive

  ido-enable-last-directory-history t ; remember last used dirs
  ido-max-work-directory-list 30   ; should be enough
  ido-max-work-file-list      50   ; remember many
  ido-use-filename-at-point nil    ; don't use filename at point (annoying)
  ido-use-url-at-point nil         ; don't use url at point (annoying)
;  ido-enable-flex-matching nil     ; don't try to be too smart
;  ido-max-prospects 8              ; don't spam my minibuffer
;  ido-confirm-unique-completion t) ; wait for RET, even with unique completion

;; when using ido, the confirmation is rather annoying...
; (setq confirm-nonexistent-file-or-buffer nil)
)

; ido requires tramp. requiring it here means I don't have to load it when 
; I do my first C-x C-f
(require 'tramp) 


; for the love of mercy, indent the same way every time!
;; (setq-default indent-tabs-mode nil) ; only ever use regular spaces, never tab
;; (setq default-tab-width 4) ; when tab char on disk, display as 4 chars wide
;; (define-key text-mode-map (kbd "TAB") 'tab-to-tab-stop) ; [TAB]key = tab2tab-stop
;; (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)) ;; tab = 4 spaces, not 8

; When opening two files of the same name from different directories
; like me/file.txt and you/file.txt
; default is to have file.txt<1> and file.txt<2>
; this makes it so you have file.txt:me and file.txt:you - rad.
(require 'uniquify) 
(setq 
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")



; Note: on OS X, it reads initial path info from your .MacOSX/Environment.plist file, not .bashrc!
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (global-set-key "\M-h" 'ns-do-hide-emacs))

(blink-cursor-mode 0)

(unless (eq window-system nil)
  (setq default-frame-alist ; this actually sets the font and colours
    (list
      (cons 'foreground-color  "white")
      (cons 'background-color  "black")
      (cons 'cursor-color'  "green")))
  (setq initial-frame-alist default-frame-alist)
  ;; (set-frame-font "Fira Code 14")
  
  (tool-bar-mode 0)    ; this just gets rid of the silly toolbar w/ icons below the menu bar
  (global-hl-line-mode t) ;; Highlight the current line. 
  (set-face-background 'hl-line "#335")
  )

;; Fira Code bullshit, lol
;; See https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
;; (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
;;                (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
;;                (36 . ".\\(?:>\\)")
;;                (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
;;                (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
;;                (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
;;                (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
;;                (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
;;                (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
;;                (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
;;                (48 . ".\\(?:x[a-zA-Z]\\)")
;;                (58 . ".\\(?:::\\|[:=]\\)")
;;                (59 . ".\\(?:;;\\|;\\)")
;;                (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
;;                (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
;;                (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
;;                (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
;;                (91 . ".\\(?:]\\)")
;;                (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
;;                (94 . ".\\(?:=\\)")
;;                (119 . ".\\(?:ww\\)")
;;                (123 . ".\\(?:-\\)")
;;                (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
;;                (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
;;                )
;;              ))
;;   (dolist (char-regexp alist)
;;     (set-char-table-range composition-function-table (car char-regexp)
;;                           `([,(cdr char-regexp) 0 font-shape-gstring]))))



;; keybindings
; http://steve.yegge.googlepages.com/effective-emacs
(global-set-key "\C-c\C-r" 'revert-buffer)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key "\C-xs" 'save-buffer) ; so tired of 'save-some-buffers, the default
(global-set-key "\M-`" 'other-frame) ; mimic the way macosx switches between windows of the same application
(global-set-key "\C-z" 'undo)

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


; I feel like it should do this for me, ugh
(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (dockerfile-mode sr-speedbar tabbar poly-ansible poly-markdown polymode yaml-mode)))
 '(sr-speedbar-delete-windows t)
 '(sr-speedbar-right-side nil)
 '(sr-speedbar-skip-other-window-p t))

(global-linum-mode t)
(setq mode-require-final-newline t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
