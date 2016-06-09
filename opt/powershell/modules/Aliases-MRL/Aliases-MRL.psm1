<#
A module that finds executables in the filesystem and sets aliases

Placing this in a module means they're all grouped together
#>

<#
A hashtable of filesystempath=alias
NOTE: this is UNORDERED
If the value is $null, use the executable basename (i.e. VBoxControl.exe -> VBoxControl)
Special replacements in the key:
- Use $env:SystemDrive and friends where appropriate
- Use :ProgramFiles: to try to expand to either "Program Files" or "Program Files (x86)"
- However, note that apps w/ both 32 and 64 bit versions (7-zip for example) will end up in ${ENV:SystemDrive}\Program Files no matter what, so that trick is unnecessary
- If the key is a DIRECTORY, then *.exe will be aliased. If the value is present, it is used as a prefix. For instance, ":ProgramFiles:\Graphviz*\bin\" = "graphviz-" will result in graphviz-dotty etc
- If the value is an array, set the alias to both
#>
$Executables = @{

    # Why does MinGW make me do this
    "${ENV:SystemDrive}\tools\mingw64\bin\mingw32-make.exe" = "gmake" 

    "${ENV:ProgramFiles}\7-Zip\7z.exe" = @("7z","sz")

    "${ENV:LocalAppData}\Pandoc\pandoc.exe" = $null

    "${ENV:ProgramFiles}\Oracle\VirtualBox" = $null

    ":ProgramFiles:\Git\bin\diff.exe" = "unixdiff"
    ":ProgramFiles:\Git\bin\sed.exe" = "unixsed"

    ":ProgramFiles:\Graphviz*\bin" = "graphviz-"

    ":ProgramFiles:\Amazon\AWSCLI\aws.exe" = $null

    # From http://slproweb.com/products/Win32OpenSSL.html
    ":ProgramFiles:\OpenSSL\bin\openssl.exe" = $null

    ":ProgramFiles:\GNU\GnuPG\gpg2.exe" = @("gpg2","gpg")
    ":ProgramFiles:\GNU\GnuPG\gpgtar.exe" = $null
    ":ProgramFiles:\GNU\GnuPG\gpgv2.exe" = $null
    ":ProgramFiles:\GNU\GnuPG\gpgconf.exe" = $null
    ":ProgramFiles:\GNU\GnuPG\bin\kleopatra.exe" = $null
    ":ProgramFiles:\GNU\GnuPG\md5sum.exe" = $null
    ":ProgramFiles:\GNU\GnuPG\sha1sum.exe" = $null
    ":ProgramFiles:\GNU\GnuPG\sha256sum.exe" = $null
}

$FoundAliases = @()

function Add-Alias {
    [CmdletBinding()] Param(
        [Parameter(Mandatory=$true)] [String] $Path,
        [String[]] [AllowEmptyString()] $Alias
    )
    if (-not $Alias) { $Alias = Get-Item $Path | Select-Object -Expand BaseName }
    foreach ($AliasName in $Alias) {
        Set-Alias -Name $AliasName -Value $Path -Scope Global
        $FoundAliases += @($AliasName)
    }
}

function Find-MRLAliases {
    [CmdletBinding()] Param()

    # Do special replacements:
    $NewExecutables = @{}
    foreach ($ExecKey in $Executables.Keys) {
        $Value = $Executables[$ExecKey]
        if ($ExecKey -match ':ProgramFiles:') {
            $KeyBare = $ExecKey -replace ":ProgramFiles:","${ENV:ProgramFiles}"
            $KeyX86 = $ExecKey -replace ":ProgramFiles:","${ENV:ProgramFiles(x86)}"
            $NewExecutables[$KeyBare] = $Value
            $NewExecutables[$KeyX86] = $Value
        }
        else {
            $NewExecutables[$ExecKey] = $Value
        }
    }

    # Now find existing files and go
    foreach ($ExecKey in $NewExecutables.Keys) {
        if (Test-Path $ExecKey) {
            $FileInfo = Get-Item $ExecKey
            $Alias = $NewExecutables[$ExecKey]
            if ($FileInfo.GetType().Name -match "DirectoryInfo") {
                foreach ($Child in (Get-ChildItem $FileInfo -Filter *.exe)) {
                    Add-Alias -Path $Child.FullName -Alias "${Alias}$($Child.BaseName)"
                }
            }
            else {
                Add-Alias -Path $FileInfo.FullName -Alias $Alias
            }
        }
    }
}

function Show-MRLAliases {
    [CmdletBinding()] Param()
    Get-Alias | Where-Object -Property Module -Match "Aliases-MRL"
}

Find-MRLAliases

Export-ModuleMember -Alias $FoundAliases -Function Show-MRLAliases,Find-MRLAliases
