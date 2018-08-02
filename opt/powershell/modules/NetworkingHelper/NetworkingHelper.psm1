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

<#
.DESCRIPTION
Serve a directory over HTTP
.PARAMETER HostIdentifier
The host to listen on, or a plus character (+) for all hosts
.PARAMETER Port
A port to listen on
.NOTES
Original version: https://obscuresecurity.blogspot.com/2014/05/dirty-powershell-webserver.html
#>
function Start-WebServer {
    [CmdletBinding()] Param(
        [string] $HostIdentifier = '+',
        [Int] $Port = 8080
    )
    $prefix = "http://${HostIdentifier}:${Port}/"
    $httpListener = New-Object -TypeName Net.HttpListener
    $httpListener.Prefixes.Add($prefix)
    $httpListener.Start()
    try {
        while ($httpListener.IsListening) {
            $listenerContext = $httpListener.GetContext()

            $reqFilePath = Join-Path -Path $Pwd -ChildPath $listenerContext.Request.RawUrl
            $reqFileContents = Get-Content -Path $reqFilePath
            $responseBuffer = [Text.Encoding]::UTF8.GetBytes($reqFileContents)

            $response = $listenerContext.Response
            $response.Headers.Add("Content-Type", "text/plain")
            $response.ContentLength64 = $responseBuffer.Length
            $response.OutputStream.Write($responseBuffer, 0, $responseBuffer.Length)
            $response.Close()
        }
    } finally {
        $httpListener.Stop()
    }
}
