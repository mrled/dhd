#!/bin/sh
set -eu

osascript <<EOF
use framework "Foundation"
use framework "AppKit"

set thePasteboard to current application's NSPasteboard's generalPasteboard()
set theHTML to thePasteboard's stringForType:(current application's NSPasteboardTypeHTML)

if theHTML is missing value then
    return "No HTML content found in clipboard."
else
    return theHTML as text
end if
EOF
