<#
.description
Provision the 'Freyja' boxes, which I use for DLP
#>

Invoke-WebRequest -UseBasicParsing https://raw.githubusercontent.com/mrled/dhd/master/opt/powershell/magic.ps1 | Invoke-Expression

# Arkansas VPN:
$akVpnName = "Arkansas"
if (-not (Get-VpnConnection -Name $akVpnName)) {
    Add-VpnConnection -Name Arkansas -ServerAddress vpn.dis.state.ar.us -TunnelType PPTP -EncryptionLevel Optional -AuthenticationMethod MSChapv2 -RememberCredential
    # [-SplitTunneling] [-EapConfigXmlStream <xml>]
}

# MSDF VPN software (unconfigured):
# First install the publisher cert, so that the VPN device driver can be installed silently
# NOTE: We got the cert by installing the NetExtender client manually, then grabbing it from the machine's TrustedPublisher store
Import-Certificate -FilePath \\VBOXSVR\VMShare\Vagrant\Freyja\SonicWallLlcPublisherCert.cer -CertStoreLocation Cert:\LocalMachine\TrustedPublisher
& \\VBOXSVR\VMShare\Vagrant\Freyja\NXSetupU.exe /S
