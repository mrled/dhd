$script:PuttySessionsRegistryKeyPathPsdrive = "HKCU:\Software\SimonTatham\PuTTY\Sessions"
$script:PuttySessionsRegistryKeyPathRegexe = "HKCU\Software\SimonTatham\PuTTY\Sessions"

<#
.SYNOPSIS
List PuTTY session names
#>
function Get-PuttySession {
    [CmdletBinding()] Param()
    foreach ($key in $(Get-ChildItem -Path $script:PuttySessionsRegistryKeyPathPsdrive)) {
        $key.Name -replace "HKEY_CURRENT_USER\\Software\\SimonTatham\\PuTTY\\Sessions\\",""
    }
}

# These functions are useful because the default colors are far too dark (blue especially)
# And I hate having to click around in the fucking thing just to fucking get the fucking colors so I can fucking see them again.
<#
.SYNOPSIS
Export a PuTTY session by name

.PARAMETER filename
A path to save the file

.PARAMETER sessionName
The session name to save

.PARAMETER force
Force the export, bypassing an "are you sure" prompt.

.NOTES
Reimport the session with
    reg import session.reg /y
#>
function Export-PuttySession {
    [CmdletBinding()] Param(
        [parameter(mandatory=$true)] [string] $filename,
        [string] $sessionName = "Default%20Settings",
        [switch] $force
    )
    $call = "reg.exe export '$script:PuttySessionsRegistryKeyPathRegexe\$sessionName' '$filename'"
    if ($force) { $call += " /y" }
    Invoke-Expression -Command $call
}

<#
.SYNOPSIS
Convert a PuTTY public key file to OpenSSH format

.PARAMETER ppbfile
Path to a PuTTY public key file
#>
function Convert-PuttyRsaPublicKey {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)] [string] $ppbfile
    )
    $pcontent = Get-Content -Path $ppbfile
    $newcontent = "ssh-rsa "
    for ($i=2; $i -lt $pcontent.count -1; $i++) {
        $newcontent += $pcontent[$i]
    }
    $comment = $pcontent[1].split("`"")[1]
    $newcontent += " $comment"
    return $newcontent
}

<#
.SYNOPSIS
Upload a public key to a remote SSH server, using PuTTY's plink.exe

.PARAMETER hostname
Remote hostname. Can include a user@ prefix to specify the user.

.PARAMETER keyfile
The path to a public key in OpenSSH (not PuTTY!) format.
#>
function Publish-PuttySshPublicKey {
    param(
        [parameter(mandatory=$True)]  [alias("host")]  [string]  $hostname,
        [string]  $keyfile="$home\.ssh\id_rsa.pub"
    )
    $keydata = get-content $keyfile
    write-host "using keyfile $keyfile" -color green
    write-host "key data: $keydata" -color green

    # if its in the putty format, fix it first.
    if ($keydata.startswith("---- BEGIN")) {
        $keydata = Convert-PuttyRsaPublicKey $keyfile
    }

    $akeys = "~/.ssh/authorized_keys"
    $sshPass = read-host "Password for $hostname"
    "",$keydata | plink -pw "$sshPass" $hostname "mkdir -p ~/.ssh && cat - >> $akeys && chmod 700 ~/.ssh && chmod 600 $akeys"
}

Set-Alias -Name uploadid -Value Publ9sh-SshPublicKey
