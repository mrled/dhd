@{
    RootModule = 'MrlPuttyHelper.psm1'
    ModuleVersion = '0.0.1'
    GUID = '1b383f7e-a075-4c14-9bb5-e5a9a5a6385a'
    Author = 'Micah R Ledbetter'
    CompanyName = ''
    Copyright = '(c) 2018 Micah R Ledbetter. All rights reserved.'
    Description = 'Putty helper functions'
    PowerShellVersion = '3.0'

    FunctionsToExport = @(
        'Convert-PuttyRsaPublicKey'
        'Export-PuttySession'
        'Get-PuttySession'
        'Publish-PuttySshPublicKey'
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
