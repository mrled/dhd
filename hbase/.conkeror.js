function reinit() { load_rc_file("/Users/mrled/.conkeror.js"); }
add_command("reinit", reinit, []);

require("global-overlay-keymap.js");
define_sticky_modifier("escape", MOD_META);

// It seems that I can just type the first few letters of a webjump
// and it works. google -> g or go; NOT google -> gg
//add_webjump("gg", "http://google.com/search?q=%s");
//add_webjump("wk", "http://en.wikipedia.org/wiki/%s");
//add_webjump("am", "http://en.wikipedia.org/wiki/%s");
//add_webjump("a", "http://www.amazon.com/s?field-keywords=%s");
require("daemon.js");

