# nvim config readme

When setting up a new host:

Link this directory to ~/.config/nvim

Then, install Packer:

```
git clone --depth 1 https://github.com/wbthomason/packer.nvim\
 ~/.local/share/nvim/site/pack/packer/start/packer.nvim
```

Then launch nvim and run `:PackerSync` and then `:PackerInstall`

Now quit and relaunch neovim
