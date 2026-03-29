# chineseroom zsh configuration

# linuxbrew ruins everything
zsh_compinit_unsafe=yes

# test -e /etc/profile.d/chineseroom-prompt.sh && . /etc/profile.d/chineseroom-prompt.sh
PROMPT='%F{white}%* %F{yellow}%n %F{red}Ꙩ %F{green}%~ %F{blue}$ %f'
test -e ~/.config/aliases.sh && . ~/.config/aliases.sh

