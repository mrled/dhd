# Visual Studio Code shit

## Install the commandline

[See docs](https://code.visualstudio.com/docs/editor/command-line).
This is only required on macOS.

Command Palette (Cmd-Shift-P) -> Shell Command: Install 'code' command in PATH


## Symlink the directory:

    # Windows w/ Powershell
    <# PS #> Set-Location -Path "${ENV:AppData}\Code"
    <# PS #> Remove-Item -Path User
    <# PS #> cmd /C mklink /D User ..\..\..\.dhd\opt\vscodeuser User

    # macOS with bash
    (: bash) cd "~/Library/Application Support/Code"
    (: bash) rm -rf User
    (: bash) ln -s ../../../.dhd/opt/vscodeuser User

## List extensions

    # Windows w/ Powershell
    <# PS #> code --list-extensions | Out-File -Encoding ASCII -FilePath extensions.txt

    # macOS w/ bash
    (: bash) code --list-extensions > extensions.txt

## Restore extensions

    <# PS #> Get-Content extensions.txt |% { code --install-extension "$_" }

    # macOS with bash
    (: bash) while read extension; do code --install-extension "$extension"; done < extensions.txt
