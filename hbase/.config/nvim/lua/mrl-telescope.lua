-- Telescope (ctrl-p, except not with that key combo)
--
require('telescope').setup{
  defaults = {
    file_ignore_patterns = {
      ".git/",
      "node_modules/",
    }
  }
}

