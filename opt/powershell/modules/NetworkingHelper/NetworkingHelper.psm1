function Get-IpV4AddressBytes {
    [CmdletBinding()] Param(
        [Parameter(ValueFromPipeline=$True, Mandatory=$True)] [IPAddress] $address,
        [String] [ValidateSet(
            "DecimalString", "Decimal", "Dec",
            "HexadecimalString", "Hexadecimal", "Hex",
            "Bytes",
            "Integer"
        )] $format = "Bytes"
    )
    $addrBytes = $address.GetAddressBytes()

    if ([BitConverter]::IsLittleEndian) {
        # .GetAddressBytes() always returns in big endian order, so correct for that here if necessary
        [Array]::Reverse($addrBytes)
    }

    $addrInt = [System.BitConverter]::ToUInt32($addrBytes, 0)

    switch -regex ($format) {
        "Bytes" { return $addrBytes }
        "Integer" { return $addrInt }
        "Dec*" { return '{0}' -f $addrInt }
        "Hex*" { return '{0:x}' -f $addrInt }
        Default { throw "Unknown format '$format'" }
    }
}

function Convert-IntegerToIpV4Address {
    [CmdletBinding()] Param(
        [Parameter(ValueFromPipeline=$True, Mandatory=$True)] [uint32] $address
    )
    $newAddr = [IPAddress] $address
    if ([System.BitConverter]::IsLittleEndian) {
        $newAddrBytes = $newAddr.GetAddressBytes()
        [Array]::Reverse($newAddrBytes)
        $newAddr = [IPAddress]$newAddrBytes
    }
    return $newAddr
}

<#
.description
Increment an IP address
.parameter address
Any string, integer, or array that the IPAddress class can convert to an IP address
Examples: [String]'192.168.1.1', [Int]22222, [String]'::1', [Array]@(192, 168, 0, 1)
.parameter addend
A positive integer to increment, or a negative integer to decrement
#>
function Add-ToIpV4Address {
    [CmdletBinding()] Param(
        [Parameter(ValueFromPipeline=$True, Mandatory=$True)] [IPAddress] $address,
        [Int64] $addend = 1
    )

    $addrInt = Get-IpV4AddressBytes -address $address -format Integer
    $newAddrInt = $addrInt + $addend
    $newAddr = Convert-IntegerToIpV4Address -address $newAddrInt

    return $newAddr
}

Export-ModuleMember -Function *
