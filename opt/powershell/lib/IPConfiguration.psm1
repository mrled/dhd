
$MRLDEBUG = $false
function Write-MrlDebug {
    param( 
        [string] $message,
        [switch] $nonewline
    )
    $params = @{
        foreground = "yellow"
        nonewline = $nonewline
        object = $message
    }
    if ($MRLDEBUG) {
        write-host @params
    }
}

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

<#
I'd like to be able to easily show any of these: 


- all
- all connected
- all physical
- connected, physical -- the default
- virtual not connected
- specific adapter by name

But for now, all I can do is: 

- all
- all connected, physical 
- specific adapter by name

#>

<#
You'd kind of like to have a separate object type (with separate default display properties)
for the Windows IP Configuration section, but that fucks up the display of the rest of the
interfaces.

If you specify too many default display properties, it displays each item as a list, not a 
table, which is also not desirable. 

In the end, I decided to leave in Show-IPConfiguration/ifconfig because it's more compact. 
Maybe I can bring Get-IPConfiguration up to par eventually though as I learn more shit.
#>

function Get-IPConfiguration {
    [CmdletBinding(DefaultParameterSetName="Show All")]
    param(
        [parameter(ParameterSetName="Show All")] [switch] $all,
        [parameter(ParameterSetName="By Adapter")] $adaptername
    )

    $ipconfig = ipconfig /all
    $allconfigs = @()
    $adapter = new-object psobject
    $entry = @{}
    foreach ($line in $ipconfig) {
        write-mrldebug -nonewline $line 
        # ensure the line isn't empty
        if (-not ($line -replace '\s+','') -eq "") {

            # if the 4th character is a space, it's a sub entry thing, like for multiple DNS servers
            if (($line[0] -eq " ") -and ($line[3] -eq " ")) {
                write-mrldebug -nonewline " +subentry"
                $k = [string]$entry.keys[0] # there will only be one key
                $sanitized = $line -replace '\s+',''
                $curvalue = $entry.$k
                $newvalue = @($curvalue) + @($sanitized)
                $entry.$k = $newvalue
            }

            # if the 1st character is a space, it's (the first line of) an entry, like for an ip address or a Description
            elseif ($line[0] -eq " ") {
                write-mrldebug -nonewline " +entry"
                if ($entry.count -gt 0) {
                    write-mrldebug -nonewline ", saving old entry"
                    add-member -inputobject $adapter -notepropertymembers $entry
                }

                $f,$v = $line -split " ?[. ]*.? ?: "
                $f = $f.substring(3)

                $entry = @{$f = $v}
            }

            # if the first character isn't whitespace, it's the header for a new adapter
            else {
                write-mrldebug -nonewline " _new"
                if ($entry.count -gt 0) {
                    write-mrldebug -nonewline ", saving old entry"
                    add-member -inputobject $adapter -notepropertymembers $entry
                    $entry = @{}
                }
                if ($adapter.name) {
                    write-mrldebug -nonewline ", saving old adapter"
                    $allconfigs += @($adapter)
                }

                # this is general config info that appears at the top
                if ($line -eq "Windows IP Configuration") {
                    $n = "Windows IP Configuration"
                    $t = ""

                    $adapter = new-object psobject
                    add-member -inputobject $adapter -notepropertymembers @{ Name = $n }

                    # $defaultprops = @('Name', 'Host Name', 'DNS Suffix Search List')
                    # $defaultdisplayprops = new-object System.Management.Automation.PSPropertySet('DefaultDisplayPropertySet',
                    #     [string[]]$defaultprops)
                    # $pssm = [system.management.automation.psmemberinfo[]] @($defaultdisplayprops)
                    # add-member -inputobject $adapter MemberSet PSStandardMembers $pssm 
               }
                # if it's not that, it's a specific adapter
                else {
                    $t, $n = $line -split " adapter "
                    $n = $n.substring(0, $n.length -1)

                    $adapter = new-object psobject
                    add-member -inputobject $adapter -notepropertymembers @{ Type = $t; Name = $n }
                }
                #$defaultprops = @('Name', 'Type', 'IPv4 Address', 'Subnet Mask', 'Default Gateway', 'DNS Servers')
                $defaultprops = @('Name', 'IPv4 Address', 'Default Gateway')
                $defaultdisplayprops = new-object System.Management.Automation.PSPropertySet('DefaultDisplayPropertySet',
                    [string[]]$defaultprops)
                $pssm = [system.management.automation.psmemberinfo[]] @($defaultdisplayprops)
                add-member -inputobject $adapter MemberSet PSStandardMembers $pssm
            }
        }
        write-mrldebug ""
    }
    if ($entry.count -gt 0) {
        add-member -inputobject $adapter -notepropertymembers $entry
    }
    if ($adapter.name) {
        $allconfigs += @($adapter)
    }
    write-mrldebug "`nNumber of configurations found: $($allconfigs.length)"

    $requestedconfigs = @()
    if ($adaptername.length -gt 0) {
        write-mrldebug "getting adapter: $adaptername"
        foreach ($adapter in $allconfigs) {
            foreach ($an in $adaptername) {
                if ($adapter.Name.tolower() -eq $an.tolower()) {
                    $requestedconfigs += @($adapter)
                }
            }
        }
    }
    elseif ($all.ispresent) {
        write-mrldebug "returning all adapters..."
        $requestedconfigs = $allconfigs
    }
    else {
        write-mrldebug "getting only physical, connected adapters..."
        foreach ($a in $allconfigs) {
            $ms = $a.'Media State'
            $desc = $a.Description
            $name = $a.Name

            if ($ms -eq 'Media disconnected') {} # do nothing
            elseif ($name -eq 'Windows IP Configuration') {} #do nothing
            elseif (($desc) -and ($desc.contains("VMware Virtual Ethernet Adapter"))) {} # do nothing
            elseif (($desc) -and ($desc.contains("Microsoft ISATAP Adapter"))) {} # do nothing
            elseif (($desc) -and ($desc.contains("VirtualBox"))) {} # do nothing
            else {
                write-mrldebug "getting adapter: $($a.Name)"
                $requestedconfigs += @($a)
            }
        }
    }

    return $requestedconfigs
}

# Another idea for this: 
function Get-IPConfig{
    param ( $RemoteComputer="LocalHost",
        $OnlyConnectedNetworkAdapters=$true
    )

    gwmi -Class Win32_NetworkAdapterConfiguration -ComputerName $RemoteComputer | Where { $_.IPEnabled -eq $OnlyConnectedNetworkAdapters } | Format-List @{ Label="Computer Name"; Expression= { $_.__SERVER }}, IPEnabled, Description, MACAddress, IPAddress, IPSubnet, DefaultIPGateway, DHCPEnabled, DHCPServer, @{ Label="DHCP Lease Expires"; Expression= { [dateTime]$_.DHCPLeaseExpires }}, @{ Label="DHCP Lease Obtained"; Expression= { [dateTime]$_.DHCPLeaseObtained }}
} 


function Show-IPConfiguration {
    $requestedconfigs = get-ipconfiguration @args

    foreach ($adapter in $requestedconfigs) {
        write-output $adapter.name
        foreach ($np in ($adapter | get-member -membertype noteproperty)) {
            if (-not ($np -eq "Name")) {
                write-output "    $($np.name)`: $($adapter.$($np.name))"
            }
        }
        write-output ""
    }
}

set-alias ifconfig Show-IPConfiguration
export-modulemember -function Get-IPConfiguration, Show-IPConfiguration -alias ifconfig