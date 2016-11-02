

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

function Get-AccountNameFromSid {
    Param(
        [Parameter(Mandatory=$True)] [String] $accountSid
    )
    $objSID = New-Object System.Security.Principal.SecurityIdentifier $accountSid
    $objSID.Translate( [System.Security.Principal.NTAccount])
}


# privileges are stored in:
# HKLM:\SECURITY\Policy\Accounts\<ACCOUNTSID>\Privilgs
# However, this is not accessible unless you fuck with the security settings of HKLM:\SECURITY
# (I think I did this with the LsaSecrets module. Lol.)
