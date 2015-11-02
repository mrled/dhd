import-module $PSScriptRoot\wintriallab-postinstall.psm1

try {
    Enable-WinRM
}
catch {
    $message  = "======== CAUGHT EXCEPTION ========`r`n$_`r`n"
    $message += "======== ERROR STACK ========`r`n"
    $error |% { $message += "$_`r`n----`r`n" }
    $message += "======== ========"
    Write-EventLogWrapper $message
    exit 666
}
