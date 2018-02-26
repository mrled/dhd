@{
    RootModule = 'MrlOpensslHelper.psm1'
    ModuleVersion = '0.0.1'
    GUID = '0424f249-5c1e-4fe9-9efb-e3ec9518eb97'
    Author = 'Micah R Ledbetter'
    CompanyName = ''
    Copyright = '(c) 2018 Micah R Ledbetter. All rights reserved.'
    Description = 'OpenSSL helper functions'
    PowerShellVersion = '3.0'

    FunctionsToExport = @(
        'Convert-OpenSSLPemToPfx'
        'Convert-OpenSSLPfxToPem'
        'Get-OpenSSLThumbprint'
        'Invoke-OpenSSL'
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
