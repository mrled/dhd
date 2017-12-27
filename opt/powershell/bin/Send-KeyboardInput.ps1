<#
.synopsis
Send keyboard input
.parameter keys
A string representing the keys to send
.example
Send-KeyboardInput -keys "%{tab}"
Sends alt-tab
.link
https://technet.microsoft.com/en-us/library/ff731008.aspx
#>
[CmdletBinding()] Param(
    [Parameter(Mandatory=$true)] [string] $keys
)

[System.Reflection.Assembly]::LoadWithPartialName("System.Windows.Forms") | Out-Null

[System.Windows.Forms.SendKeys]::SendWait($keys)
