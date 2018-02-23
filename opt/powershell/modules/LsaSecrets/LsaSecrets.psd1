@{
    RootModule = 'LsaSecrets.psm1'
    GUID = '938b191e-5839-4164-ae15-21d05a589c14'
    ModuleVersion = '0.0.1'
    Author = 'Micah R Ledbetter'
    CompanyName = ''
    Copyright = '(c) 2018 Micah R Ledbetter. All rights reserved.'
    PowerShellVersion = '5.0'

    FunctionsToExport = @(
        'Enable-TSDuplicateToken'
        'Get-TSLsaSecret'
        'Get-TSLsaSecretNames'
    )
    CmdletsToExport = @()
    VariablesToExport = @()
    AliasesToExport = @()
    DscResourcesToExport = @()

    PrivateData = @{
        PSData = @{
        }
    }
}
