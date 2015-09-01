##############################################################################
##
## Get-FileEncoding
##
## From Windows PowerShell Cookbook (O'Reilly)
## by Lee Holmes (http://www.leeholmes.com/guide)
##
##############################################################################

<#
.SYNOPSIS
Gets the encoding of a file

.EXAMPLE
Get-FileEncoding.ps1 .\UnicodeScript.ps1

BodyName          : unicodeFFFE
EncodingName      : Unicode (Big-Endian)
HeaderName        : unicodeFFFE
WebName           : unicodeFFFE
WindowsCodePage   : 1200
IsBrowserDisplay  : False
IsBrowserSave     : False
IsMailNewsDisplay : False
IsMailNewsSave    : False
IsSingleByte      : False
EncoderFallback   : System.Text.EncoderReplacementFallback
DecoderFallback   : System.Text.DecoderReplacementFallback
IsReadOnly        : True
CodePage          : 1201
#>

param(
    [parameter(mandatory=$true)] [string[]] $Path
)
Set-StrictMode -Version Latest



<#
.synopsis
A pseudoconstructor for encoding types
#>
function New-FileEncodingType {
    param(
        [parameter(mandatory=$true)] $preamble,
        [parameter(mandatory=$true)] [string] $encodingName
    )
    $encoding = new-object PSObject -property @{
        Preamble = $preamble
        Name = $encodingName
        PreambleLength = ($preamble -split "-").count
    }
    return $encoding
}

<#
.synopsis
Get all encodings supported by the .NET framework
#>
function Get-SystemFileEncodings {
    $encodings = @()
    foreach ($encMemb in [System.Text.Encoding] | Get-Member -Static -MemberType Property) {
        #$preamble = [System.Text.Encoding]::($encMemb.Name).GetPreamble() -join '-'
        $preamble = [System.Text.Encoding]::($encMemb.Name).GetPreamble()
        $encodings += @(New-FileEncodingType -preamble $preamble -encodingName $encMemb.Name)
    }
    return $encodings
}

$encodings = Get-SystemFileEncodings
$results = @()

foreach ($pp in $path) {
    $result = "UTF7" # Assume the encoding is UTF7 by default
    foreach ($enc in $encodings) {
        $bytes = (get-content -encoding byte -readcount $enc.PreambleLength -path $pp)[0]
        if ($bytes -eq $enc.Preamble) { 
            $result = $enc
            break
        }
    }
    $results += @([System.Text.Encoding]::$result)
}

return $results

