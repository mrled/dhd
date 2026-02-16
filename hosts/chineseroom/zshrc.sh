# chineseroom zsh configuration

# linuxbrew ruins everything
zsh_compinit_unsafe=yes

test -e /etc/profile.d/chineseroom-prompt.sh && . /etc/profile.d/chineseroom-prompt.sh
test -e ~/.config/aliases.sh && . ~/.config/aliases.sh
