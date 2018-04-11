<#
.SYNOPSIS
Convert a Base64-encoded string to UTF8 text
.PARAMETER Base64
A Base64-encoded string to convert
#>
[CmdletBinding()] Param(
    [Parameter(Mandatory, ValueFromPipeline=$True)] $Base64
)
[Text.Encoding]::UTF8.GetString([Convert]::FromBase64String($Base64))
