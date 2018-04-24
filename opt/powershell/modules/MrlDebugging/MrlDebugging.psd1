@{
    RootModule = 'MrlDebugging.psm1'
    ModuleVersion = '0.0.1'
    GUID = 'efccfb84-5d5c-4a4f-b3b1-ffb6c86585b4'
    Author = 'Micah R Ledbetter'
    CompanyName = ''
    Copyright = '(c) 2018 Micah R Ledbetter. All rights reserved.'
    Description = 'Powershell debugging helpers'
    PowerShellVersion = '5.0'

    FunctionsToExport = @(
        'Clear-Error'
        'Get-Assembly'
        'Get-AvailableExceptionsList'
        'Get-ErrorType'
        'Get-Type'
        'Show-ErrorReport'
        'Show-ObjectProperties'
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
