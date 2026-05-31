# Apparently zsh reads this and NOT .profile for noninteractive nonlogin shells? Ugh
test -f "$HOME/.shdetect_dhd.sh" && . "$HOME/.shdetect_dhd.sh"

