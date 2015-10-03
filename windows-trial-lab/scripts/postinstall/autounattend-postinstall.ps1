[cmdletbinding()]
param()

import-module $PSScriptRoot\wintriallab-postinstall.psm1

try {
    #Enable-WinRM
    Set-PasswordExpiry -accountName "vagrant" -expirePassword $false
    Disable-HibernationFile
    Enable-MicrosoftUpdate
}
catch {
    write-host "======== CAUGHT EXCEPTION ========"
    write-host "$_"
    write-host "======== CALL STACK ========"
    Get-PSCallStack | format-list
    write-host "======== ERROR STACK ========"
	for ($i=0; $i<$error.count; $i+=1) {
		write-host "`$error[$i]"
		write-host $error[$i]
	}
    write-host "======== ========"
    exit 666
}
