# nvim config readme

When setting up a new host:

Link this directory to ~/.config/nvim

Then, install Packer:

```
git clone --depth 1 https://github.com/wbthomason/packer.nvim\
 ~/.local/share/nvim/site/pack/packer/start/packer.nvim
```

Then launch nvim and run `:PackerSync`.

Now quit and relaunch neovim

## Adding new plugins

* Add the plugin to `./lua/plugins.lua`.
* Source `init.lua`.
* Run `:PackerSync`

## Installing language servers

Then you have to install a language server for all the languages you care about.

`:MasonInstall bash-language-server` for example.

This installs to `~/.local/share/nvim/mason`,
which avoids using the execrable `npm -g` that every fool recommends in their readme files.

