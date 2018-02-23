@{
    RootModule = 'UserAccounts.psm1'
    GUID = 'f98f8de7-c6a3-42a0-a618-3d890a0a75ba'
    ModuleVersion = '0.0.1'
    Author = 'Micah R Ledbetter'
    CompanyName = ''
    Copyright = '(c) 2018 Micah R Ledbetter. All rights reserved.'
    PowerShellVersion = '5.0'

    FunctionsToExport = @(
        'Get-AccountSid'
        'Get-AccountNameFromSid'
        'Get-ServiceAccountName'
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
