@{
    RootModule = 'UserRights.psm1'
    GUID = 'ddfc0d5c-b22b-4397-a91c-f25f3f73d4f4'
    ModuleVersion = '0.0.1'
    Author = 'Tony Pombo'
    CompanyName = ''
    Copyright = '(c) 2016 Tony Pombo'
    PowerShellVersion = '3.0'

    FunctionsToExport = @(
        'Get-AccountsWithUserRight'
        'Get-UserRightsGrantedToAccount'
        'Grant-TokenPrivilege'
        'Grant-UserRight'
        'Revoke-TokenPrivilege'
        'Revoke-UserRight'
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
