# via: http://brettterpstra.com/2012/05/17/markdown-editing-for-sublime-text-2-humble-beginnings/

import sublime, sublime_plugin
 
class CopyScopeCommand(sublime_plugin.TextCommand):
    def run(self, edit):
        sublime.set_clipboard(self.view.scope_name(self.view.sel()[0].begin()))
        sublime.status_message("Copied scope")
 
    def is_enabled(self):
        return True
  
# To call the command, add the following to Preferences->Key Bindings - User:
# { "keys": ["shift+super+alt+p"], "command": "copy_scope" }

