$possibleOpenSSL = @(
    "${env:programfiles}\OpenSSL-Win64\bin\openssl.exe"
    "${env:programfiles(x86)}\Git\bin\openssl.exe"
    'C:\STRAWBERRY\C\BIN\openssl.exe'
)
foreach ($o in $possibleOpenSSL) {
    if (test-path $o) {
        set-alias OpenSslExe $o
        $env:OPENSSL_CONF="$Home\.dhd\opt\win32\openssl.cnf"
        break
    }
}

function Invoke-OpenSSL {
    [cmdletbinding()]
    param(
        [string[]] $argumentList = @(),
        [switch] $Passthru
    )
    $OpenSslPath = (gcm OpenSslExe |? {$_.commandtype -eq 'Alias'}).definition
    $sslProc = Invoke-ProcessAndWait -Passthru -RedirectStandardError -RedirectStandardOutput -command $OpenSslPath -argumentList $argumentList

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
        $pfxPassword,
        [string] $outfile = ((resolve-path $certFile).path -replace '(\.pem$)|(\.crt$)|(\.cert$)','.pfx'),
        [string] $displayname = ((split-path -leaf $certFile) -replace '(\.pem$)|(\.crt$)|(\.cert$)','')
    )
    if (-not $pfxPassword) {
        $securePfxPassword = read-host "Enter a password for the PFX file" -AsSecureString
        $pfxPassword = Decrypt-SecureString $securePfxPassword
    }
    $certFile = resolve-path $certFile
    $arguments = @("pkcs12", "-export", "-out", "`"$outfile`"", "-in", "`"$certfile`"", 
        "-name", "`"$displayname`"", "-passout", "`"pass:$pfxPassword`"")
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
