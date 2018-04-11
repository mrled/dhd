<#
.SYNOPSIS
Send data over the network

.DESCRIPTION
Send data over the network
Sort of like you might wanna do with netcat/nc, but too different to be "nc for powershell"
Source: https://gist.github.com/jstangroome/9adaa87a845e5be906c8

.PARAMETER Computer
A remote machine to send data to

.PARAMETER Port
The port to send data to

.PARAMETER Data
The data to send

.PARAMETER Encoding
The encoding to use when sending data

.PARAMETER Timeout
If this timeout is exceeded while sending the data, abort

.EXAMPLE 'GET / HTTP/1.0', '' | Send-NetworkData -Computer www.powershellmagazine.com -Port 80
Pipe in a HTTP request

.EXAMPLE Send-NetworkData -Data 'GET / HTTP/1.0', '' -Computer www.powershellmagazine.com -Port 80 -Timeout 0:00:02
Use the Data parameter to do the same but only wait 2 seconds for a response:

.EXAMPLE Send-NetworkData -Data "EHLO $Env:ComputerName", "QUIT" -Computer mail.example.com -Port 25
Say hello to an SMTP server
#>
[CmdletBinding()]
param (
    [Parameter(Mandatory)] [string] $Computer,
    [Parameter(Mandatory)] [ValidateRange(1, 65535)] [Int16] $Port,
    [Parameter(ValueFromPipeline)] [string[]] $Data,
    [System.Text.Encoding] $Encoding = [System.Text.Encoding]::ASCII,
    [TimeSpan] $Timeout = [System.Threading.Timeout]::InfiniteTimeSpan
)
begin {
    # establish the connection and a stream writer
    $Client = New-Object -TypeName System.Net.Sockets.TcpClient
    $Client.Connect($Computer, $Port)
    $Stream = $Client.GetStream()
    $Writer = New-Object -Type System.IO.StreamWriter -ArgumentList $Stream, $Encoding, $Client.SendBufferSize, $true
}
process {
    # send all the input data
    foreach ($Line in $Data) {
        $Writer.WriteLine($Line)
    }
}
end {
    # flush and close the connection send
    $Writer.Flush()
    $Writer.Dispose()
    $Client.Client.Shutdown('Send')

    # read the response
    $Stream.ReadTimeout = [System.Threading.Timeout]::Infinite
    if ($Timeout -ne [System.Threading.Timeout]::InfiniteTimeSpan) {
        $Stream.ReadTimeout = $Timeout.TotalMilliseconds
    }

    $Result = ''
    $Buffer = New-Object -TypeName System.Byte[] -ArgumentList $Client.ReceiveBufferSize
    do {
        try {
            $ByteCount = $Stream.Read($Buffer, 0, $Buffer.Length)
        }
        catch [System.IO.IOException] {
            $ByteCount = 0
        }
        if ($ByteCount -gt 0) {
            $Result += $Encoding.GetString($Buffer, 0, $ByteCount)
        }
    } while ($Stream.DataAvailable -or $Client.Client.Connected)
    Write-Output $Result
    # cleanup
    $Stream.Dispose()
    $Client.Dispose()
}
