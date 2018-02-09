# Visual Studio Code shit

Symlink the directory:

    <# PS #> cd "${ENV:AppData}\Code"
    <# PS #> cmd /C mklink /D User ..\..\..\.dhd\opt\vscodeuser User

List extensions

    <# PS #> code --list-extensions > extensions.txt

Restore extensions

    <# PS #> Get-Content extensions.txt |% { code --install-extension "$_" }