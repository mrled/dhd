<#
.synopsis
Create a new dhd Powershell module
#>
[CmdletBinding(DefaultParameterSetName="Module")] Param(
    [Parameter(Mandatory)] [string] $Name,
    [Parameter(Mandatory)] [string] $Description,

    [Parameter(Mandatory, ParameterSetName="Module")] [switch] $Module,
    [Parameter(Mandatory, ParameterSetName="DscResource")] [switch] $DscResource,

    $Author = "Micah R Ledbetter",
    $CopyrightDate = $(Get-Date | Select-Object -ExpandProperty Year),
    $LicenseUri = "https://unlicense.org/",
    $ModuleVersion = "0.0.1",
    $PowerShellVersion = "5.0"
)

$dhdPsModulesPath = "$Home\.dhd\opt\powershell\modules"

$moduleDir = "$dhdPsModulesPath\$Name"
New-Item -Type Directory -Path $moduleDir -Force
$manifestPath = "$moduleDir\$Name.psd1"
$modulePath = "$moduleDir\$Name.psm1"

switch ($PSCmdlet.ParameterSetName) {
    "Module" {
        $nmmParams = @{
            AliasesToExport = @()
            Author = $Author
            CmdletsToExport = @()
            CompanyName = ""
            Description = $Description
            FunctionsToExport = "*"
            LicenseUri = $LicenseUri
            ModuleVersion = $ModuleVersion
            Path = $manifestPath
            PowerShellVersion = $PowerShellVersion
            RootModule = "$Name.psm1"
            VariablesToExport = @()
        }
        $moduleContents = @"
<#
.SYNOPSIS
$Description
#>
"@
    }
    "DscResource" {
        $nmmParams = @{
            AliasesToExport = @()
            Author = $Author
            CmdletsToExport = @()
            CompanyName = ""
            Description = $Description
            DscResourcesToExport = @($Name)
            FunctionsToExport = @()
            LicenseUri = $LicenseUri
            ModuleVersion = $ModuleVersion
            Path = $manifestPath
            PowerShellVersion = $PowerShellVersion
            RootModule = "$Name.psm1"
            VariablesToExport = @()
        }
        $moduleContents = @"
<#
.SYNOPSIS
$Description
#>

enum Ensure {
    Absent
    Present
}

[DscResource()] class $Name {
    [DscProperty(Key)]
    [string] `$Name

    [DscProperty()]
    [Ensure] `$Ensure = [Ensure]::Present

    [void] Set() {
        if (`$Ensure -eq [Ensure]::Present) {
            Write-Verbose -Message "Setting configuration..."
            throw "NOT IMPLEMENTED"
        } else {
            Write-Verbose -Message "Unsetting configuration..."
            throw "NOT IMPLEMENTED"
        }
    }

    [bool] Test() {
        throw "NOT IMPLEMENTED"
    }

    [$Name] Get() {
        throw "NOT IMPLEMENTED"
        return `$this
    }
}
"@
    }
    default {
        throw "Unknown ParameterSetName '$($PSCmdlet.ParameterSetName)"
    }
}
New-ModuleManifest @nmmParams

# Fix the encoding since git _still_ can't handle UTF16 lolololol
# Also get rid of the miles of crap that New-ModuleManifest tracks thru ur files
$manifestContents = Get-Content -LiteralPath $manifestPath | Select-String -NotMatch -Pattern @("^$", "^ *#.*")
Out-File -InputObject $manifestContents -LiteralPath $manifestPath -Encoding utf8

Out-File -InputObject $moduleContents -LiteralPath $modulePath -Encoding utf8
