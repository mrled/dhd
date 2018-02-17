

<#
.SYNOPSIS
Get the Security Identifier (SID) for a Windows account
#>
function Get-AccountSid {
    Param(
        [Parameter(Mandatory=$True)] [String] $accountName,
        [String] $domainName
    )
    if ($domainName) {
        $objUser = New-Object System.Security.Principal.NTAccount($domainName, $accountName)
    }
    else {
        $objUser = New-Object System.Security.Principal.NTAccount($accountName)
    }
    $objUser.Translate([System.Security.Principal.SecurityIdentifier])
}

<#
.SYNOPSIS
Get the name of a Windows account from the Security Identifier (SID)
#>
function Get-AccountNameFromSid {
    Param(
        [Parameter(Mandatory=$True)] [String] $accountSid
    )
    $objSID = New-Object System.Security.Principal.SecurityIdentifier $accountSid
    $objSID.Translate( [System.Security.Principal.NTAccount])
}

<#
.description
Return the name of the service account a particular service runs under
.notes
See the LsaSecrets module for information on stealing the service account password from the LSA secrets store
#>
function Get-ServiceAccountName {
    [CmdletBinding()] Param(
        [Parameter(Mandatory=$true)] $serviceName
    )
    $wmiObj = Get-WmiObject -Class Win32_Service -Filter "name='$serviceName'"
    $service = Get-Service $serviceName
    New-Object PSObject -Property @{
        ServiceName = $service.Name
        DisplayName = $service.DisplayName
        ServiceAccount = $wmiObj.StartName
    }
}

# privileges are stored in:
# HKLM:\SECURITY\Policy\Accounts\<ACCOUNTSID>\Privilgs
# However, this is not accessible unless you fuck with the security settings of HKLM:\SECURITY
# (I think I did this with the LsaSecrets module. Lol.)
