#!/bin/tcsh

# this script isn't ported to work with the 'set backslash_quote'  option
# (which breaks compatibility with csh, maybe?)

unset backslash_quote

# this would be a darn difficult-looking bunch of ugliness
# except for the setting of variables below
# I can't believe I didn't think of it myself. heh.

set ansi_norm		= '\033[0m'
set ansi_bold		= '\033[1m'
set ansi_blink		= '\033[5m'
set ansi_rev		= '\033[7m'
set ansi_fg_black	= '\033[30m'
set ansi_fg_red		= '\033[31m'
set ansi_fg_green	= '\033[32m'
set ansi_fg_yellow	= '\033[33m'
set ansi_fg_blue	= '\033[34m'
set ansi_fg_magenta	= '\033[35m'
set ansi_fg_cyan	= '\033[36m'
set ansi_fg_white	= '\033[37m'
set ansi_bg_black	= '\033[40m'
set ansi_bg_red 	= '\033[41m'
set ansi_bg_gree 	= '\033[42m'
set ansi_bg_yellow 	= '\033[43m'
set ansi_bg_blue	= '\033[44m'
set ansi_bg_magenta	= '\033[45m'
set ansi_bg_cyan	= '\033[46m'
set ansi_bg_white	= '\033[47m'

# for c&p purposes, w/ $
# $ansi_norm		= '\033[0m'
# $ansi_bold		= '\033[1m'
# $ansi_blink		= '\033[5m'
# $ansi_rev		= '\033[7m'
# $ansi_fg_black	= '\033[30m'
# $ansi_fg_red		= '\033[31m'
# $ansi_fg_green	= '\033[32m'
# $ansi_fg_yellow	= '\033[33m'
# $ansi_fg_blue		= '\033[34m'
# $ansi_fg_magenta	= '\033[35m'
# $ansi_fg_cyan		= '\033[36m'
# $ansi_fg_white	= '\033[37m'
# $ansi_bg_black	= '\033[40m'
# $ansi_bg_red 		= '\033[41m'
# $ansi_bg_gree 	= '\033[42m'
# $ansi_bg_yellow 	= '\033[43m'
# $ansi_bg_blue		= '\033[44m'
# $ansi_bg_magenta	= '\033[45m'
# $ansi_bg_cyan		= '\033[46m'
# $ansi_bg_white	= '\033[47m'

setenv HOST `hostname`

if ( $HOST == "samantha" ) then
#
# Super-cute cat. looks like:
# (stolen unabashedly from someone else)
#       |\      _,,,---,,_
# ZZZzz /,`.-'`'    -.  ;-;;,_
#      |,4-  ) )-,_. ,\ (  `'-'
#     '---''(_/--'  `-'\_)
#
# had to cut this up, since I needed to escape ' and ` characters. 
#
	printf "$ansi_fg_yellow       |\\      _,,,---,,_\n"  
	printf "$ansi_fg_white$ansi_bold ZZZzz $ansi_norm$ansi_fg_yellow" 
		printf '/,`.-' 
		printf "'"
		printf '`'
		printf "'"
		printf "    -.  ;-;;,_\n"
	printf "      |,4-  ) )-,_. ,\\ (  "
		printf '`'
		printf "'-'\n"
	printf "     '---''(_/--'  "
		printf '`-'
		printf "'\\_)\n"
	printf "	$ansi_fg_white$ansi_bold Shh! Samantha is sleeping!\n"

# else if ( $HOST == "tiana" ) then

else if ( $HOST == "kerrigan" ) then
# 
#  hammer+sickle:
# ... actually my idea. I stole the ASCII art though. 
#
#      >   .    
#    ',     :
#   <. `.  .:
# CCCP   `.:     ALL HAIL THE IRON
#      ,.:'`.    FIST OF COMMUNISM
#     /'
printf "$ansi_bold$ansi_fg_magenta"
printf "Sarah Kerrigan welcomes you to:$ansi_norm\n"
printf "$ansi_bold$ansi_fg_red$ansi_bg_yellow      >   .                       $ansi_norm\n"

printf "$ansi_bold$ansi_fg_red$ansi_bg_yellow    \',     :                      $ansi_norm\n"

printf "$ansi_bold$ansi_fg_red$ansi_bg_yellow"
printf '   <. `.  .:'
printf "                      $ansi_norm\n"

printf "$ansi_bold$ansi_fg_red$ansi_bg_yellow"
printf ' CCCP   `.:     ALL HAIL THE IRON '
printf "$ansi_norm\n"

printf "$ansi_bold$ansi_fg_red$ansi_bg_yellow      ,.:\'"
printf '`.    FIST OF COMMUNISM '
printf "$ansi_norm\n"

printf "$ansi_bold$ansi_fg_red$ansi_bg_yellow     /'                           $ansi_norm\n"

else if ( $HOST == "marajade" || $HOST == "mj" ) then

printf "{HHHHHH<@>HHHHHH}[========================================="

else
#
# Metroid. looks like: 
# (stolen unabashedly from soneone else, though all I saw was a .gif
#  or something... it was in a webcomic.)
#  .---.
# /  @  \
# \ @ @ /
#  {'^'}
#     -- SQUEE!!
#
	printf "  $ansi_fg_green.---.\n"
	printf " $ansi_fg_green/  $ansi_fg_red@  $ansi_fg_green\\\n"
	printf " $ansi_fg_green\\ $ansi_fg_red@ @ $ansi_fg_green/\n"
	printf "  $ansi_fg_yellow{'$ansi_fg_green^$ansi_fg_yellow'}\n"
	printf "$ansi_fg_white$ansi_bold     -- SQUEE\!\!\n"
endif

# clean up after yourself, Junior. 
set backslash_quote
