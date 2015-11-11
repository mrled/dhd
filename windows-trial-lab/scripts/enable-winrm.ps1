import-module $PSScriptRoot\wintriallab-postinstall.psm1
$errorActionPreference = "Stop"
Invoke-ScriptblockAndCatch -scriptBlock {
    Enable-WinRM
}
