<#
.SYNOPSIS
Aliases for use in my profile
#>


#### Aliases for built-in commands
Set-Alias -Name getmo -Value Get-Module -Force
Set-Alias -Name rmmo -Value Remove-Module -Force


#### Aliases for MrlInteractive functions
Set-Alias -Name .. -Value MrlInteractive\Set-LocationParent -Force
Set-Alias -Name ... -Value MrlInteractive\Set-LocationGrantParent -Force
Set-Alias -Name .... -Value MrlInteractive\Set-LocationGreatGrandParent -Force
Set-Alias -Name bat -Value MrlInteractive\Start-BatchFile -Force
Set-Alias -Name cd -Value MrlInteractive\Set-MrlLocation -Option AllScope -Force
Set-Alias -Name fex -Value MrlInteractive\Expand-FuckingArchive -Force
Set-Alias -Name llm -Value MrlInteractive\Get-ChildItemSortedLastWriteTime -Force
Set-Alias -Name man -Value MrlInteractive\Get-MrlHelp -Option AllScope -Force
Set-Alias -Name omg -Value MrlInteractive\Show-InputCommandline -Force
Set-Alias -Name pwgen -Value MrlInteractive\New-Password -Force
Set-Alias -Name reimport -Value MrlInteractive\Import-ModuleIdempotently -Force
Set-Alias -Name Rename-Tab -Value MrlInteractive\Set-ConEmuTabTitle -Force
Set-Alias -Name syntax -Value MrlInteractive\Get-Syntax -Force
Set-Alias -Name touch -Value MrlInteractive\Set-MrlFile -Force
Set-Alias -Name vl -Value MrlInteractive\Invoke-VimLessMacro -Force
Set-Alias -Name wh -Value MrlInteractive\Show-AllCommands -Force


#### Aliases for external binaries
Set-Alias -Name l -Value less -Force
