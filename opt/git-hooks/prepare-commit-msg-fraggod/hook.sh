#!/bin/sh
msg_file=$1
commit_src=$2
hash=$3
test -z "$msg_file" || test "$GIT_EDITOR" = ":" && exit 0
test -z "$commit_src" || exit 0 # cherry-picks, merges, etc

echo >>"$msg_file" '#'
echo >>"$msg_file" "# Commit dir: ${GIT_PREFIX%/}"
echo >>"$msg_file" "#   Repo dir: $(realpath "$PWD")"
echo >>"$msg_file" '#'
echo >>"$msg_file" '# Last 10 log messages:'
git log -10 --format='#   %s' >>"$msg_file"
echo >>"$msg_file" '#'
echo >>"$msg_file" '#'
echo >>"$msg_file" '# git diff:'
git diff --staged | sed 's/^/#   /' >> "$msg_file"

