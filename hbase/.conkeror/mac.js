function hide_conkeror() {
    spawn_process_blind("/usr/bin/osascript", 
                        ["osascript", "-e", 'tell application "System Events" to set visible of process "conkeror" to false']);
}
interactive("hide-conkeror", "Hide Conkeror (in Mac OS X).", hide_conkeror);
define_key(content_buffer_normal_keymap, "M-h", "hide-conkeror");
