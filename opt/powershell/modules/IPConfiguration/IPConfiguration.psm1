function Get-IPConfiguration {
    [CmdletBinding(DefaultParameterSetName="select")]
    param(
        [parameter(ParameterSetName="adapter", Position=0)] [string] $adaptername,
        [parameter(ParameterSetName="select")]  [switch] [alias("virtual")] $VirtualAdapters,
        [parameter(ParameterSetName="select")]  [switch] [alias("disconnected")] $DisconnectedAdapters,
        [parameter(ParameterSetName="select")]  [switch] [alias("tunnels")] $TunnelAdapters,
        [parameter(ParameterSetName="select")]  [switch] [alias("loopback")] $LoopbackAdapters,
        [parameter(ParameterSetName="all")]     [switch] [alias("all")] $allAdapters,
        [parameter(ParameterSetName="global")]  [switch] $globalProperties
    )
    $requestednics = @()

    $paramset = $PSCmdlet.ParameterSetName.tolower()
    if ($paramset -eq "global") {
        $gloprops = [System.Net.NetworkInformation.IPGlobalProperties]::GetIPglobalProperties()
        $requestednics = $gloprops
    }
    elseif ($paramset -eq "all") {
        $requestednics = [System.Net.NetworkInformation.NetworkInterface]::GetAllNetworkInterfaces()
    }
    elseif ($paramset -eq "adapter") {
        $allnics = [System.Net.NetworkInformation.NetworkInterface]::GetAllNetworkInterfaces()
        foreach ($n in $allnics) {
            if ($n.name -eq $adaptername) {
                $requestednics += $n
            }
            if ($requestednics.length -eq 0) {
                throw "Requested a NIC called $adaptername, but no such NIC exists"
            }
        }
    }
    elseif ($paramset -eq "select") {
        $allnics = [System.Net.NetworkInformation.NetworkInterface]::GetAllNetworkInterfaces()
        foreach ($n in $allnics) {
            $desc = $n.Description
            if ($n.OperationStatus -eq "Down") {
                if ($DisconnectedAdapters.ispresent) {
                    $requestednics += @($n)
                }
            }
            elseif ($desc -and ($desc.contains("VMware Virtual Ethernet Adapter") -or $desc.contains("VirtualBox Host-Only Ethernet Adapter"))) {
                if ($VirtualAdapters.ispresent) {
                    $requestednics += @($n)
                }
            }
            elseif (([string]$n.NetworkInterfaceType).tolower() -eq "tunnel") {
                if ($TunnelAdapters.ispresent) {
                    $requestednics += @($n)
                }
            }
            elseif (([string]$n.NetworkInterfaceType).tolower() -eq "loopback") {
                if ($LoopbackAdapters.ispresent) {
                    $requestednics += @($n)
                }
            }
            else {
                $requestednics += @($n)
            }
        }
    }
    return $requestednics 
}

function Convert-HexStringToMac {
    param([parameter(mandatory=$true)] [string] $hex)
    if ($hex.length%2 -eq 1) {
        $hex = "0" + "$hex"
    }
    $mac = ""
    $i = 0
    while ($i -lt $hex.length) {
        $mac += $hex[$i]
        if (($i%2 -eq 1) -and ($i -ne $hex.length-1)) {
            $mac += ":"
        }
        $i += 1
    }
    return $mac.tolower()
}

function Get-Listeners {
    $gloprops = [System.Net.NetworkInformation.IPGlobalProperties]::GetIPglobalProperties()
    $listeners = $gloprops.GetActiveTcpListeners()
    $listeners
}
set-alias listens get-listeners
set-alias listeners get-listeners

# Sucks because it's a netstat call, but this is the only way to get the process that's using a port. 
function Get-Listeners2 {
    netstat -a -n -o |% {$_ -match "LISTENING"}
}

function Get-PublicIPAddress {
    invoke-restmethod icanhazip.com
}

# todo: 
# - show more ipv6 info
# - show dhcp 
# - show whether ip routing / wins gateway is enabled in the global section like ipconfig
# - throw an error if you pass -global to this function 
# - show the number of adapters that are hidden
# I think the thing to do is make Powershell objects for each one with all the data *I* want
# and then add the .NET object in there as _DotNetAdapter or something
# And then make it so Get-IPConfiguration displays data as nicely as ifconfig does now
function Show-IPConfiguration {
    $global = Get-IPConfiguration -global
    write-output "Hostname: $($global.HostName).$($global.DomainName)`n"
    #write-output "    Public IP: $(Get-PublicIPAddress)"

    $requestedconfigs = Get-IPConfiguration @args
    foreach ($adapter in $requestedconfigs) {
        $props = $adapter.GetIPProperties()
        write-output $adapter.name
        write-output "    $($adapter.Description)"
        foreach ($addr in $props.UnicastAddresses) {
            write-output "    IP Address: $($addr.Address.IPAddressToString)/$($addr.PrefixLength)"
        }
        write-output "    Default Gateway: $($props.GatewayAddresses.Address.IPAddressToString)"
        write-output "    DNS Servers: $($props.DnsAddresses.IPAddressToString)"
        $mac = $adapter.GetPhysicalAddress()
        if ($mac.tostring().length -gt 0) { 
            $mac = Convert-HexStringToMac $mac
        }
        else {
            $mac = "NONE"
        }
        write-output "    Link type: $($adapter.NetworkInterfaceType), MAC: $mac"
        write-output ""
    }
}

set-alias ifconfig Show-IPConfiguration
set-alias getip Get-IPConfiguration


$exFunction = @(
    'Get-IPConfiguration'
    'Show-IPConfiguration'
    'Get-Listeners'
    'Get-PublicIPAddress'
)
$exAlias = @(
    'ifconfig'
    'getip'
    'listens'
    'listeners'
)
export-modulemember -function $exFunction -alias $exAlias
