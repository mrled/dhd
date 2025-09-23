#!/bin/sh
set -eu

termglyph="❯"
llmglyph="∑"

usage() {
    cat <<EOF
Usage: $0 [-h] <WORKSPACE|UTIL>
Workspace setup operations

ARGUMENTS
    -h | --help     Show this help message and exit.
    WORKSPACE       The workspace to set up.
                    dhd:
                        Workspace 7
                        Open a terminal in ~/.dhd and a VS Code window in that directory.
                    understatement-web:
                        Workspace q
                        Opens up two web browsers and a terminal in the understatement repo
                    understatement-dev-1:
                        Workspace w
                        Opens VS Code and two terminals ssh'd into chineseroom
                    understatement-dev-2:
                        Workspace e
                        Opens VS Code and two terminals ssh'd into chineseroom
                    me.micahrl.com:
                        Workspace t
                        Opens VS Code and a terminal in the me.micahrl.com repo
    UTIL            A utility command to run.
                    find_app_window_id <APP_BUNDLE_ID> <TITLE_REGEX>
                        Find the window ID of an application window with a title matching the given regex.
                    terminal_move_or_open <WORKSPACE> <TITLE> <COMMAND>
                        Move an existing Terminal window matching TITLE to WORKSPACE,
                        or open a new one running COMMAND.
EOF
    echo "Usage: $0 <MODE>"
}

if [ $# -eq 0 ]; then
    usage
    exit 1
fi

# Open a new Terminal window running the given script.
open_terminal_with_script() {
    script="$1"
    osascript -e '
tell application "Terminal"
    do script "'"$script"'"
    activate
end tell'
}

find_app_window_id() {
    app_bundle_id="$1"
    title_regex="$2"
    aerospace list-windows --app-bundle-id "$app_bundle_id" --monitor all --format '%{window-id} %{window-title}' |
        while read -r line; do
            wid=${line%% *}
            title=${line#* }
            if echo "$title" | grep -qE "$title_regex"; then
                # printf '%-8s' "$wid" 1>&2
                # echo "$title" 1>&2
                echo "$wid"
            fi
        done
}

vscode_move_or_open() {
    workspace="$1"
    window_regex="$2"
    uri="$3"

    existing_vscode=$(find_app_window_id 'com.microsoft.VSCode' "$window_regex" | head -n1  || true)
    if test "$existing_vscode"; then
        aerospace move-node-to-workspace --window-id "$existing_vscode" "$workspace"
    else
        # This is a no-op if there is already a VS Code window (on any workspace!) for this directory/URI.
        # Even with ?windowId=_blank, which forces a new window.
        open "$uri?windowId=_blank"
    fi
}

# Move or open a Terminal window by a special title, and run a command in it.
# Rely on a title that should be unique in any possible Terminal title text.
# It is recommended to use a special glyph to make it unique.
# Search for an existing window with that title, and if found, move it to the given workspace.
# If not found, open a new Terminal window running the given command,
# and set its title to the given title with a terminal escape sequence.
# (Note that setting the title will only actually set part of the window title,
# depending on your Terminal settings.)
terminal_move_or_open() {
    workspace="$1"
    title="$2"
    cmd="$3"

    # Create a temp file to hold the script to run.
    # This gets around quoting issues with osascript.
    # The script will delete itself when done ---
    # it's better to do it that way than try to do it from the parent shell
    # where we might delete it before Terminal has a chance to run it.
    # TMPDIR on macOS is guaranteed to exist and be writable, and allow no other users.
    tmpfile="$TMPDIR/terminal_move_or_open_${workspace}_${title}.sh"
    cat > $tmpfile << ENDSCRIPT
#!/bin/sh
# Run with set -x so the cmd is shown to the user when the terminal opens.
set -x
printf '\033]0;%s\007' '$title'
$cmd
rm -f $tmpfile
set +x
ENDSCRIPT

    existing_terminal=$(find_app_window_id 'com.apple.Terminal' "$title" | head -n1 || true)
    if test "$existing_terminal"; then
        aerospace move-node-to-workspace --window-id "$existing_terminal" "$workspace"
    else
        # Run by dot-sourcing so that commands like 'cd' will work
        open_terminal_with_script ". '$tmpfile'"
    fi
}


workspace_dhd() {
    aerospace workspace 7
    vscode_move_or_open 7 '\.dhd$' "vscode://file/${HOME}/.dhd"
    terminal_move_or_open 7 "${termglyph} dhd" "cd '${HOME}/.dhd'"
    terminal_move_or_open 7 "${llmglyph} dhd" "cd '${HOME}/.dhd'; /Users/mrled/.bun/bin/claude;"
}

workspace_understatement_web() {
    aerospace workspace q
    existing_firefox_count="$(aerospace list-windows --app-bundle-id org.mozilla.firefox --workspace q | wc -l | tr -d '[:space:]' || true)"
    # Two windows: one for LLMs and one for normal browsing.
    # (Case statement fallthrough is intentional.)
    case "$existing_firefox_count" in
        0) open -na "Firefox" --args --new-window;;
        1) open -na "Firefox" --args --new-window;;
    esac
}

workspace_understatement_dev_1() {
    aerospace workspace w
    vscode_move_or_open w \
        'understatement1 \[SSH: chineseroom.micahrl.com' \
        'vscode://vscode-remote/ssh-remote+chineseroom.micahrl.com/home/callista/work/understatement1?windowId=_blank'
    terminal_move_or_open w "${termglyph} understatement1" "ssh chineseroom.micahrl.com -t 'cd ~/work/understatement1 && exec \$SHELL -l'"
    terminal_move_or_open w \
        "${llmglyph} undersetatement1" \
        "ssh chineseroom.micahrl.com -t 'cd ~/work/understatement1 && exec \$SHELL -l -i -c /home/callista/.local/bin/claude --dangerously-skip-permissions'"
}

workspace_understatement_dev_2() {
    aerospace workspace e
    vscode_move_or_open e \
        'understatement2 \[SSH: chineseroom.micahrl.com' \
        'vscode://vscode-remote/ssh-remote+chineseroom.micahrl.com/home/callista/work/understatement2?windowId=_blank'
    terminal_move_or_open w "${termglyph} understatement2" "ssh chineseroom.micahrl.com -t 'cd ~/work/understatement2 && exec \$SHELL -l'"
    terminal_move_or_open w \
        "${llmglyph} undersetatement2" \
        "ssh chineseroom.micahrl.com -t 'cd ~/work/understatement2 && exec \$SHELL -l -i -c /home/callista/.local/bin/claude --dangerously-skip-permissions'"
}

workspace_me_micahrl_com() {
    aerospace workspace t
    vscode_move_or_open t 'me\.micahrl\.com$' "vscode://file/${HOME}/mrldata/Repositories/me.micahrl.com"
    terminal_move_or_open t "${termglyph} me.micahrl.com" "cd '${HOME}/mrldata/Repositories/me.micahrl.com'"
    terminal_move_or_open t "${llmglyph} me.micahrl.com" "cd '${HOME}/mrldata/Repositories/me.micahrl.com'; /Users/mrled/.bun/bin/claude;"
}

case "$1" in
    -h|--help) usage; exit 0;;
    # Workspaces
    "dhd") workspace_dhd; shift;;
    "understatement-web") workspace_understatement_web; shift;;
    "understatement-dev-1") workspace_understatement_dev_1; shift;;
    "understatement-dev-2") workspace_understatement_dev_2; shift;;
    "me.micahrl.com") workspace_me_micahrl_com; shift;;
    # Utilities
    "find_app_window_id") shift; find_app_window_id "$@"; exit $?;;
    "terminal_move_or_open") shift; terminal_move_or_open "$@"; exit $?;;
    *) echo "Unknown workspace: $1" >&2; exit 1;;
esac
