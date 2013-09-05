
<#
Get-IPConfiguration returns a hashtable representing a network adapter based on the output of ipconfig /all

ipconfig /all returns a series of headings with subinformation. The first heading is "Windows IP ConfiguratioN", 
and contains general host information. All the other headings are for individual network adapters. 

This is an example of a network adapter section from ipconfig /all:

Ethernet adapter lan:

   Connection-specific DNS Suffix  . :
   Description . . . . . . . . . . . : Intel(R) 82579LM Gigabit Network Connection
   Physical Address. . . . . . . . . : F0-1F-AF-2E-DC-3A
   DHCP Enabled. . . . . . . . . . . : No
   Autoconfiguration Enabled . . . . : Yes
   Link-local IPv6 Address . . . . . : fe80::c50f:a495:405:9a17%12(Preferred)
   IPv4 Address. . . . . . . . . . . : 192.168.1.229(Preferred)
   Subnet Mask . . . . . . . . . . . : 255.255.255.0
   Default Gateway . . . . . . . . . : 192.168.1.1
   DNS Servers . . . . . . . . . . . : 198.153.192.50
                                       198.153.194.50
                                       192.168.1.1
   NetBIOS over Tcpip. . . . . . . . : Enabled

#>


# this is kinda hacky
# ideally you'd generate Adapter objects or something and be able to display them differently
# also if I just run "ifconfig"  it shows me too much info, not separated by adapter
# But it works for now. :)
function Get-IPConfiguration {
    param(
        [string] $adaptername
    )
    $ipconfig = ipconfig /all
    $ipcs = @()
    $adapter = @{}
    $entry = @{}
    foreach ($line in $ipconfig) {
        # ensure the line isn't empty
        if (-not ($line -replace '\s+','') -eq "") {

            # if the 4th character is a space, it's a sub entry thing, like for multiple DNS servers
            if ($line[3] -eq " ") {
                $k = [string]$entry.keys[0] # there will only be one key
                $sanitized = $line -replace '\s+',''
                $curvalue = $entry.$k
                $newvalue = @($curvalue) + @($sanitized)
                $entry.$k = $newvalue
            }

            # if the 1st character is a space, it's (the first line of) an entry, like for an ip address or a Description
            elseif ($line[0] -eq " ") {
                if ($entry.count -gt 0) {
                    $adapter += $entry
                }

                $f,$v = $line -split " ?[. ]*.? ?: "
                $f = $f.substring(3)

                $entry = @{$f = $v}
            }

            # if the first character isn't whitespace, it's the header for a new adapter
            else {
                if ($entry.count -gt 0) {
                    $adapter += $entry
                    $entry = @()
                }
                if ($adapter.count -gt 0) {
                    $ipcs += @($adapter)
                }

                # this is general config info that appears at the top
                if ($line -eq "Windows IP Configuration") {
                    $n = "Windows IP Configuration"
                    $t = ""
                }
                # if it's not that, it's a specific adapter
                else {
                    $t, $n = $line -split " adapter "
                    $n = $n.substring(0, $n.length -1)
                }
                $adapter = @{ Type = $t; Name = $n }
            }
        }
    }

    if ($entry.count -gt 0) {
        $adapter += $entry
    }
    if ($adapter.count -gt 0) {
        $ipcs += @($adapter)
    }

    if ($adaptername) {
        foreach ($a in $ipcs) {
            if ($a.name -eq $adaptername) {
                return $a
            }
        }
    }
    else {
        return $ipcs
    }
}

# Another idea for this: 
function Get-IPConfig{
    param ( $RemoteComputer="LocalHost",
        $OnlyConnectedNetworkAdapters=$true
    )

    gwmi -Class Win32_NetworkAdapterConfiguration -ComputerName $RemoteComputer | Where { $_.IPEnabled -eq $OnlyConnectedNetworkAdapters } | Format-List @{ Label="Computer Name"; Expression= { $_.__SERVER }}, IPEnabled, Description, MACAddress, IPAddress, IPSubnet, DefaultIPGateway, DHCPEnabled, DHCPServer, @{ Label="DHCP Lease Expires"; Expression= { [dateTime]$_.DHCPLeaseExpires }}, @{ Label="DHCP Lease Obtained"; Expression= { [dateTime]$_.DHCPLeaseObtained }}
} 


function Show-IPConfiguration {
    [CmdletBinding(DefaultParameterSetName="Show All")]
    #[parameter(ParameterSetName="Show Disconnected")] [switch] $hideDisconnected = $false,
    param(
        [parameter(ParameterSetName="Show All")] [switch] $all,
        [parameter(ParameterSetName="By Adapter")] $adaptername
    )
    $allconfigs = Get-IPConfiguration
    $requestedconfigs = @()

    if ($adaptername.length -gt 0) {
        foreach ($adapter in $allconfigs) {
            if ($adapter.name -eq $adaptername) {
                $requestedconfigs += @($adapter)
            }
        }
    }
    elseif ($all.ispresent) {
        $requestedconfigs = $allconfigs
    }
    else {
        foreach ($a in $allconfigs) {
            if (-not ($a.'Media State' -eq 'Media disconnected')) {
                $requestedconfigs += @($a)
            }
        }
    }

    foreach ($adapter in $requestedconfigs) {
        write-output $adapter.name
        foreach ($key in $adapter.keys) {
            if (-not ($key -eq "Name")) {
                write-output "    $key`: $($adapter.$key)"
            }
        }
        write-output ""
    }
}

set-alias ifconfig Show-IPConfiguration

export-modulemember -function Get-IPConfiguration, Show-IPConfiguration -alias ifconfig