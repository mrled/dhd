<#
.SYNOPSIS
Aliases for use in my profile
#>

Set-Alias -Name man -Value Get-MrlHelp -Option AllScope -Force
Set-Alias -Name syntax -Value Get-Syntax -Force

Set-Alias -Name pwgen -Value New-Password -Force

Set-Alias -Name l -Value less -Force
Set-Alias -Name vl -Value vless -Force
Set-Alias -Name wh -Value Display-AllCommands -Force

# By default, touch is aliased to Set-FileTime, which doesn't create new empty files.
Set-Alias -Name touch -Value Set-MrlFile -Force

# By default, cd is aliased to Set-Location, which does not cd to $Home if no argument is passed
Set-Alias -Name cd -Value Set-LocationMrl -Option AllScope -Force

Set-Alias -Name reimport -Value Import-ModuleIdempotently -Force
Set-Alias -Name getmo -Value Get-Module -Force
Set-Alias -Name rmmo -Value Remove-Module -Force

Set-Alias -Name bat -Value Start-BatchFile -Force

Set-Alias -Name Fucking-Extract -Value Extract-FuckingArchive -Force
Set-Alias -Name fex -Value Extract-FuckingArchive -Force

Set-Alias -Name Rename-Tab -Value Set-ConEmuTabTitle -Force
