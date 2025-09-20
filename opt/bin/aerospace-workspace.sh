#!/bin/sh
set -eu

usage() {
    cat <<EOF
Usage: $0 [-h] <WORKSPACE>
Workspace setup operations

ARGUMENTS
    -h | --help     Show this help message and exit.
    WORKSPACE       The workspace to set up.
                    dhd:
                        Open a terminal in ~/.dhd and a VS Code window in that directory.
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

open_terminal_in_dir() {
    dir="$1"
    osascript -e '
tell application "Terminal"
    do script "cd \"'"$dir"'\""
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
                echo "$wid"
            fi
        done
}

workspace_dhd() {
    aerospace workspace 7

    existing_vscode=$(find_app_window_id 'com.microsoft.VSCode' '\.dhd$' | head -n1  || true)
    if test "$existing_vscode"; then
        aerospace move-node-to-workspace --window-id "$existing_vscode" 7
    else
        # This is a no-op if there is already a VS Code window (on any workspace!) for this directory
        open -na "Visual Studio Code" --args --new-window "${HOME}/.dhd"
    fi

    existing_terminal=$(find_app_window_id 'com.apple.Terminal' '^\.dhd' | head -n1 || true)
    if test "$existing_terminal"; then
        aerospace move-node-to-workspace --window-id "$existing_terminal" 7
    else
        open_terminal_in_dir "${HOME}/.dhd"
    fi
}

case "$1" in
    -h|--help) usage; exit 0;;
    "dhd") workspace_dhd; shift;;
    "find_app_window_id") shift; find_app_window_id "$@"; exit $?;;
    *) echo "Unknown workspace: $1" >&2; exit 1;;
esac


