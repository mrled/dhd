#!/bin/sh
set -eu

# Disable dock bouncing globally
defaults write com.apple.dock no-bouncing -bool TRUE
killall Dock
# To re-enable:
# defaults delete com.apple.dock no-bouncing
