# todo: 
# - show more ipv6 info
# - show dhcp 
# - show whether ip routing / wins gateway is enabled in the global section like ipconfig
# - figure out the right way to include global IP information in this (probably a separate command honestly)
# - show the number of adapters that are hidden - ?? probably not
# - create a class for IPAddress objects and use that here
# - create views in the format.ps1xml file for tables and lists as well
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
    $reqDotNetNics = @()

    $paramset = $PSCmdlet.ParameterSetName.tolower()
    if ($paramset -eq "global") {
        $gloprops = [System.Net.NetworkInformation.IPGlobalProperties]::GetIPglobalProperties()
        $reqDotNetNics = $gloprops
    }
    elseif ($paramset -eq "all") {
        $reqDotNetNics = [System.Net.NetworkInformation.NetworkInterface]::GetAllNetworkInterfaces()
    }
    elseif ($paramset -eq "adapter") {
        $allnics = [System.Net.NetworkInformation.NetworkInterface]::GetAllNetworkInterfaces()
        foreach ($n in $allnics) {
            if ($n.name -eq $adaptername) {
                $reqDotNetNics += $n
            }
            if ($reqDotNetNics.length -eq 0) {
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
                    $reqDotNetNics += @($n)
                }
            }
            elseif ($desc -and ($desc.contains("VMware Virtual Ethernet Adapter") -or $desc.contains("VirtualBox Host-Only Ethernet Adapter"))) {
                if ($VirtualAdapters.ispresent) {
                    $reqDotNetNics += @($n)
                }
            }
            elseif (([string]$n.NetworkInterfaceType).tolower() -eq "tunnel") {
                if ($TunnelAdapters.ispresent) {
                    $reqDotNetNics += @($n)
                }
            }
            elseif (([string]$n.NetworkInterfaceType).tolower() -eq "loopback") {
                if ($LoopbackAdapters.ispresent) {
                    $reqDotNetNics += @($n)
                }
            }
            else {
                $reqDotNetNics += @($n)
            }
        }
    }

    $reqNetworkAdapters = @()
    foreach ($nic in $reqDotNetNics) {
        $props = $nic.GetIPProperties()
        $ip = @()
        foreach ($addr in $props.UnicastAddresses) {
            #$ip += @(@{ IPAddress = $addr.Address.IPAddressToString; CIDR = $addr.PrefixLength; })
            #$ip += @("$($addr.Address.IPAddressToString)/$($addr.PrefixLength)")
            $ip += $addr.Address.IPAddressToString
        }
        $mac = $nic.GetPhysicalAddress()
        if ($mac.tostring().length -gt 0) { 
            $mac = Convert-HexStringToMac $mac
        }
        else {
            $mac = "NONE"
        }
        $property = @{
            Name = $nic.name
            Description = $nic.Description
            IPAddress = $props.UnicastAddresses.Address.IPAddressToString
            IPv4Mask = $props.UnicastAddresses.IPv4Mask.IPAddressToString
            DefaultGateway = $props.GatewayAddresses.Address.IPAddressToString
            DNSServer = $props.DnsAddresses.IPAddressToString
            MAC = $mac
            NetworkInterfaceType = $nic.NetworkInterfaceType
            DotNetObject = $nic
        }
        $nicobj = new-object PSObject -property $property
        $nicobj.PSObject.Typenames.insert(0,'IPConfigurationNetworkAdapter')
        $reqNetworkAdapters += @($nicobj)
    }
    return $reqNetworkAdapters
}
set-alias ifconfig Get-IPConfiguration
set-alias getip Get-IPConfiguration

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

# Much faster and less lame than parsing the output of netstat.exe
# However, you can't get PIDs at all this way. 
function Get-Listeners2 {
    $gloprops = [System.Net.NetworkInformation.IPGlobalProperties]::GetIPglobalProperties()
    $listeners = $gloprops.GetActiveTcpListeners()
    $listeners
}

function Get-Listeners {
    $listeners = @()
    foreach ($line in (netstat -ano)) {
        if ($line -match "LISTENING") {
            $item = $line.split(" ", [system.stringsplitoptions]::removeemptyentries)

            if (($a = $item[1] -as [ipaddress]).AddressFamily -eq 'InterNetworkV6') {
                $address = $a.IPAddressToString
                $port = $item[1].split('\]:')[-1] 
                $addressFamily = 'InterNetworkV6'
            } 
            else {
                $address = $item[1].split(':')[0] 
                $port = $item[1].split(':')[-1] 
                $addressFamily = 'InterNetwork'
            }  

            $p = @{
                Protocol = $item[0]
                Address = $address
                Port = [int]$port
                PID = [int]$item[4]
                ProcessName = (get-process -id $item[4] -erroraction SilentlyContinue).name
                AddressFamily = $addressFamily
            }
            $l = new-object PSObject -property $p
            $l.PSObject.Typenames.insert(0,'IPConfigurationListener')
            $listeners += @($l)
        }
    }
    $listeners | sort-object -property 'AddressFamily','Port'
}
set-alias listens get-listeners
set-alias listeners get-listeners

function resolve-hostname {
    param(
        [parameter(mandatory=$true)] [string[]] $domainName
    )
    foreach ($dn in $domainName) {
        write-host "$dn`:"
        foreach ($a in [System.Net.Dns]::Resolve($dn).AddressList) {
            write-host "    $a"
        }
        
    }
}
set-alias resolve resolve-hostname

function Get-PublicIPAddress {
    invoke-restmethod icanhazip.com
}
set-alias icanhazip Get-PublicIPAddress
set-alias canhazip Get-PublicIPAddress

$exFunction = @(
    'Get-IPConfiguration'
    'Show-IPConfiguration'
    'Get-Listeners'
    'Get-Listeners2'
    'Get-PublicIPAddress'
    'Resolve-Hostname'
)
$exAlias = @(
    'ifconfig'
    'getip'
    'listens'
    'listeners'
    'icanhazip'
    'canhazip'
    'resolve'
)
export-modulemember -function $exFunction -alias $exAlias
