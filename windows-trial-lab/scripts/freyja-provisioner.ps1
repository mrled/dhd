<#
.description
Provision the 'Freyja' boxes, which I use for DLP
#>

$ErrorActionPreference = "Stop"

Invoke-WebRequest -UseBasicParsing https://raw.githubusercontent.com/mrled/dhd/master/opt/powershell/magic.ps1 | Invoke-Expression

# Arkansas VPN:
$akVpnName = "Arkansas"
if (-not (Get-VpnConnection -Name $akVpnName -ErrorAction SilentlyContinue)) {
    Add-VpnConnection -Name Arkansas -ServerAddress vpn.dis.state.ar.us -TunnelType PPTP -EncryptionLevel Optional -AuthenticationMethod MSChapv2 -RememberCredential
    # [-SplitTunneling] [-EapConfigXmlStream <xml>]
}

$neVpnName = "Nebraska"
if (-not (Get-VpnConnection -Name $neVpnName -ErrorAction SilentlyContinue)) {
    Add-VpnConnection -Name Nebraska -ServerAddress 162.127.2.14 -TunnelType Automatic -EncryptionLevel Optional -AuthenticationMethod MSChapv2 -RememberCredential
}

# MSDF VPN software (unconfigured):
# First install the publisher cert, so that the VPN device driver can be installed silently
# NOTE: We got the cert by installing the NetExtender client manually, then grabbing it from the machine's TrustedPublisher store
Import-Certificate -FilePath \\VBOXSVR\VMShare\Vagrant\Freyja\SonicWallLlcPublisherCert.cer -CertStoreLocation Cert:\LocalMachine\TrustedPublisher
& \\VBOXSVR\VMShare\Vagrant\Freyja\NXSetupU.exe /S
