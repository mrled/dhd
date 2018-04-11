<#
.SYNOPSIS
Convert a string to Base64
.PARAMETER String
A string to convert
#>
[CmdletBinding()] Param(
    [Parameter(Mandatory, ValueFromPipeline=$True)] $String
)
[Convert]::ToBase64String([Text.Encoding]::UTF8.GetBytes($String))
