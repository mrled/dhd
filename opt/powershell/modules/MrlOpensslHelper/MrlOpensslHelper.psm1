<#
.synopsis
Start a process and wait for it to exit
.notes
This is a workaround for a stupid idiot bug in start-process that only triggers sometimes.
http://social.technet.microsoft.com/Forums/scriptcenter/en-US/37c1066e-b67f-4709-b195-aa2790216bd0
https://connect.microsoft.com/PowerShell/feedback/details/520554/
The bug has it return instantly even when -wait is passed to start-process, at least on Eric's local box.
When that happens, $process.ExitCode hasn't been populated, and won't be, even when the process does actually exit.
System.Diagnostics.Process doesn't have that behavior, so that's what we're going to use instead

TODO: UPDATE: 20180226: This hack is probably no longer necessary. Test and delete.
#>
function Invoke-ProcessAndWait {
    [cmdletbinding(DefaultParameterSetName="CheckExitCode")]
    param(
        [parameter(mandatory=$true)] [string] $command,
        [string[]] $argumentList,
        [switch] $RedirectStandardError,
        [switch] $RedirectStandardOutput,
        [parameter(ParameterSetName="Passthru")] [switch] $Passthru,
        [parameter(ParameterSetName="CheckExitCode")] [switch] $CheckExitCode
    )
    write-verbose "Running Invoke-ProcessAndWait in verbose mode. WARNING: this may show sensitive commandline arguments (passwords, connection strings) and should only be used in development!"
    write-verbose "Running '$command' with arguments '$argumentList'"
    $process = New-Object System.Diagnostics.Process
    $process.StartInfo = New-Object System.Diagnostics.ProcessStartInfo
    $process.StartInfo.FileName = $command

    #$process.StartInfo.RedirectStandardError = $RedirectStandardError
    #if ($ShowStandardOutput) {
    if ($RedirectStandardOutput) {
        $process.StartInfo.RedirectStandardOutput = $true
    }
    if ($RedirectStandardError) {
        $process.StartInfo.RedirectStandardError = $true
    }
    $process.StartInfo.UseShellExecute = $false # AKA don't run in a new window
    $process.StartInfo.Arguments = $argumentList
    $process.Start() | Out-Null
<#
    if ($RedirectStandardOutput) {
        $line = $process.StandardOutput.ReadLine()
        while ($line -ne $null) {
            Write-Host $line
            $line = $process.StandardOutput.ReadLine()
        }
    }
#>
    $process.WaitForExit()
    write-verbose "Process exited with exit code $($process.ExitCode)"
    if ($PSCmdlet.ParameterSetName -eq "CheckExitCode") {
        if ($process.ExitCode -ne 0) {
            write-verbose "Command $command with arguments '$argumentList' exited with code $($process.ExitCode)"
            throw "Command '$command' with $($argumentList.count) arguments exited with code $($process.ExitCode)"
        }
    }
    else {
        return $process
    }
}

function Invoke-OpenSSL {
    [cmdletbinding()]
    param(
        [string[]] $argumentList = @(),
        [switch] $Passthru
    )
    $env:OPENSSL_CONF = "$Home\.dhd\opt\win32\openssl.cnf"
    $sslProc = Invoke-ProcessAndWait -Passthru -RedirectStandardError -RedirectStandardOutput -command 'OpenSSL.exe' -argumentList $argumentList

    $stdout = $sslProc.StandardOutput.ReadToEnd()
    $stderr = $sslProc.StandardError.ReadToEnd()

    write-verbose "===== Standard Output ====="
    write-verbose $stdout
    write-verbose "===== Standard  Error ====="
    write-verbose $stderr

    # In normal mode, always display output.
    # In Passthru mode, only display output if the ExitCode was not zero

    if ($Passthru) {
        if ($sslProc.ExitCode -ne 0) {
            if ($stdout) { write-host $stdout }
            if ($stderr) { write-host $stderr -foregroundcolor DarkGray }
            throw "OpenSSL exited with code '$($sslProc.ExitCode)'"
        }
        # Necessary because the .ReadToEnd() method can't get called more than once
        $sslProc | Add-Member -MemberType NoteProperty -Name SerializedStandardOutput -Value $stdout
        $sslProc | Add-Member -MemberType NoteProperty -Name SerializedStandardError -Value $stderr
        return $sslProc
    }
    else {
        if ($stdout) { write-host $stdout }
        if ($stderr) { write-host $stderr -foregroundcolor Red }
    }
}

function Convert-OpenSSLPemToPfx {
    param(
        [parameter(mandatory=$true)] [string] $certFile,
        [string] $keyFile = ((resolve-path $certFile).path -replace '(\.pem$)|(\.crt$)|(\.cert$)','.key'),
        [SecureString] $pfxPassword,
        [string] $outfile = ((resolve-path $certFile).path -replace '(\.pem$)|(\.crt$)|(\.cert$)','.pfx'),
        [string] $displayname = ((split-path -leaf $certFile) -replace '(\.pem$)|(\.crt$)|(\.cert$)','')
    )
    if (-not $pfxPassword) {
        $pfxPassword = read-host "Enter a password for the PFX file" -AsSecureString
    }
    $decryptedPass = (New-Object -TypeName PSCredential -ArgumentList $("ignored", $pfxPassword)).GetNetworkCredential().Password
    $certFile = resolve-path $certFile
    $arguments = @("pkcs12", "-export", "-out", "`"$outfile`"", "-in", "`"$certfile`"",
        "-name", "`"$displayname`"", "-passout", "`"pass:$decryptedPass`"")
    if ($keyFile) {
        $keyFile = resolve-path $keyFile
        $arguments += @("-inkey", "`"$keyFile`"")
    }
    Invoke-OpenSsl -argumentList $arguments
}

function Convert-OpenSSLPfxToPem {
    param(
        [parameter(mandatory=$true)] [string] $pfxfile,
        [string] $outfile = ((resolve-path $pfxfile).path -replace ".pfx$","") + ".pem"
    )
    Invoke-OpenSsl -argumentList @("pkcs12", "-in", "`"$pfxfile`"", "-out", "`"$outfile`"", "-nodes")
}

function Get-OpenSSLThumbprint {
    param(
        [parameter(mandatory=$true)] [string] $pemFile
    )
    $pemFile = resolve-path $pemFile
    $sslProc = Invoke-OpenSsl -Passthru -argumentList @("x509", "-in", "$pemFile", "-sha1", "-noout", "-fingerprint")
    $thumbprint = $sslProc.SerializedStandardOutput.Split('=')[1].Replace(':','')
    write-output $thumbprint
}
