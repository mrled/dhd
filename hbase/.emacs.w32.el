; just copy this to your homedir in Windows... 
; you can't make symlinks that work reliably with all programs in Windows XP
; (this has changed with Vista but I still use XP sometimes), and hardlinks
; keep just getting unlinked, so this is just easier. 

; Also a fun note: I can't figure out why this is (new in 24.3? something 
; weird about the environment at DLP?) but I installed Emacs and found that 
; "~" expands to C:\\Users\\Micah\\AppData\\Roaming -- ?
; So I gotta change HOME to be just my user folder, and then ~ works like I 
; expect. 

(setenv "HOME" 
	(replace-regexp-in-string 
	 "\\(\\\\AppData\\\\Roaming\\)" "" (getenv "HOME")))

(load-file "~/.dhd/hbase/.emacs")

