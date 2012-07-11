// load urls from cli into new buffers, not new windows; must be at top of file:
url_remoting_fn = load_url_in_new_buffer;



require("session.js");
session_auto_save_auto_load = true;


add_hook("mode_line_hook", mode_line_adder(buffer_count_widget), true);
require("favicon");
// favicon in modeline: 
add_hook("mode_line_hook", mode_line_adder(buffer_icon_widget), true);
// favicon in buffer list: 
read_buffer_show_icons = true;


url_completion_use_history = true;

can_kill_last_buffer = false;
// add_hook("before_quit_hook",
//          function () {
//              var w = get_recent_conkeror_window();
//              var result = (w == null) ||
//                  "y" == (yield w.minibuffer.read_single_character_option(
//                      $prompt = "Quit Conkeror? (y/n)",
//                      $options = ["y", "n"]));
//              yield co_return(result);
//          });

