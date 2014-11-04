<#
.synopsis
Show a representation of a hexadecimal string using emoji
.notes
Can be useful for e.g. verifying SSH fingerprints 
Based on windytan's perl script: https://gist.github.com/windytan/7910910#file-emoji-pl
#>
[cmdletbinding()]
param(
    [parameter(mandatory=$true)] [ValidatePattern('^([0-9a-f][0-9a-f]:?)+$')] $hexString
)

$emoString = get-content $PSScriptRoot\emojis.txt
$emoArray = @()

# Note that emojis are two-byte characters, if you do $emojiString[$i], you're only getting half of it
for ($i=0; $i -lt $emoString.length; $i+=2) {
    $emoji = $emoString[$i,($i+1)] -join ''
    $emoArray += @($emoji)
}

$emoOut = ""
for ($i=0; $i -lt $hexString.length; $i+=2) {
    $hit = $hexString[$i..($i+1)] -join '' # hit == Hex digIT, analogous to bit.
    $hexVal = [Convert]::ToInt32($hit, 16)
    $emoVal = $emoArray[$hexVal]
    $emoOut += $emoVal
    write-verbose "$hit = $emoVal"
}
$emoOut