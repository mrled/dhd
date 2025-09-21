#!/bin/sh
set -eu

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
EOF
    echo "Usage: $0 <MODE>"
}

if [ $# -eq 0 ]; then
    usage
    exit 1
fi

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

terminal_move_or_open() {
    workspace="$1"
    window_regex="$2"
    cmd="$3"

    existing_terminal=$(find_app_window_id 'com.apple.Terminal' "$window_regex" | head -n1 || true)
    if test "$existing_terminal"; then
        aerospace move-node-to-workspace --window-id "$existing_terminal" "$workspace"
    else
        open_terminal_with_script "$cmd"
    fi
}

workspace_dhd() {
    aerospace workspace 7
    vscode_move_or_open 7 '\.dhd$' "vscode://file/${HOME}/.dhd"
    terminal_move_or_open 7 '^\.dhd$' "cd '${HOME}/.dhd'"
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
}

workspace_understatement_dev_2() {
    aerospace workspace e
    vscode_move_or_open e \
        'understatement2 \[SSH: chineseroom.micahrl.com' \
        'vscode://vscode-remote/ssh-remote+chineseroom.micahrl.com/home/callista/work/understatement2?windowId=_blank'
}

workspace_me_micahrl_com() {
    aerospace workspace t
    vscode_move_or_open t 'me\.micahrl\.com$' "vscode://file/${HOME}/mrldata/Repositories/me.micahrl.com"
    terminal_move_or_open t '^me\.micahrl\.com$' "cd '${HOME}/mrldata/Repositories/me.micahrl.com'"
}

case "$1" in
    -h|--help) usage; exit 0;;
    # Workspaces
    "dhd") workspace_dhd; shift;;
    "understatement-web") workspace_understatement_web; shift;;
    "understatement-dev-1") workspace_understatement_dev_1; shift;;
    "understatement-dev-2") workspace_understatement_dev_2; shift;;
    # Utilities
    "find_app_window_id") shift; find_app_window_id "$@"; exit $?;;
    *) echo "Unknown workspace: $1" >&2; exit 1;;
esac


