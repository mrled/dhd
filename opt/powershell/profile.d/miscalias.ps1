<#
.SYNOPSIS
Aliases for use in my profile
#>


#### Aliases for built-in commands
Set-Alias -Name getmo -Value Get-Module -Force
Set-Alias -Name rmmo -Value Remove-Module -Force


#### Aliases for MrlInteractive functions
Set-Alias -Name bat -Value MrlInteractive\Start-BatchFile -Force
Set-Alias -Name cd -Value MrlInteractive\Set-LocationMrl -Option AllScope -Force
Set-Alias -Name fex -Value MrlInteractive\Extract-FuckingArchive -Force
Set-Alias -Name man -Value MrlInteractive\Get-MrlHelp -Option AllScope -Force
Set-Alias -Name pwgen -Value MrlInteractive\New-Password -Force
Set-Alias -Name reimport -Value MrlInteractive\Import-ModuleIdempotently -Force
Set-Alias -Name Rename-Tab -Value MrlInteractive\Set-ConEmuTabTitle -Force
Set-Alias -Name syntax -Value MrlInteractive\Get-Syntax -Force
Set-Alias -Name touch -Value MrlInteractive\Set-MrlFile -Force
Set-Alias -Name vl -Value MrlInteractive\vless -Force
Set-Alias -Name wh -Value MrlInteractive\Display-AllCommands -Force


#### Aliases for external binaries
Set-Alias -Name l -Value less -Force
