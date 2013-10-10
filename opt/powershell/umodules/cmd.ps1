# shitty cmd.exe workarounds

# mklink isn't an exe - it's a cmd.exe builtin! what the fuck. 
# also note that you cannot do this without elevating the prompt first lolololololol
function mklink {
    cmd /c mklink $args
}
function ftype {
    cmd /c ftype $args
}
function assoc {
    cmd /c assoc $args
}

