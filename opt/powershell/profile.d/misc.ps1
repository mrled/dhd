#### Miscellaneous / Everything ####


#### Functions

<#
.description
Retrieve powershell platform (cross platform)
#>
function Get-PowershellPlatform {
    [CmdletBinding()] Param()
    if ($PSVersionTable.Keys -Contains 'Platform') {
        return $PsVersionTable.Platform
    } else {
        # This value is what is returned by $PsVersionTabble.Platform on Powershell Core
        return "Win32NT"
    }
}

<#
.description
Test whether the current session has administrative privileges (cross platform)
#>
function Test-AdminRole {
    [CmdletBinding()] Param()
    if ((Get-PowershellPlatform) -eq "Win32NT") {
        $identity = [Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()
        $adminRole = [Security.Principal.WindowsBuiltInRole] "Administrator"
        return $identity.IsInRole($adminRole)
    } else {
        return (id -u) -eq 0
    }
}

<#
.description
Retrieve the current machine's hostname (cross platform)
#>
function Get-Hostname {
    [CmdletBinding()] Param()
    if ($env:COMPUTERNAME) {
        return $env:COMPUTERNAME
    } else {
        return hostname
    }
}

function ConvertTo-Base64($string) {
   $bytes  = [System.Text.Encoding]::UTF8.GetBytes($string)
   [System.Convert]::ToBase64String($bytes)
}
function ConvertFrom-Base64($string) {
   $bytes  = [System.Convert]::FromBase64String($string)
   [System.Text.Encoding]::UTF8.GetString($bytes)
}

function Test-PowershellSyntax {
    [cmdletbinding(DefaultParameterSetName='FromFile')]
    param(
        [parameter(mandatory=$true, Position=0, ValueFromPipeline=$true, ParameterSetName='FromText')] [string] $text,
        [parameter(mandatory=$true, Position=0, ParameterSetName='FromFile')] [string] $fileName,
        [switch] $ThrowOnFailure
    )
    $tokens = @()
    $parseErrors = @()
    $parser = [System.Management.Automation.Language.Parser]
    if ($pscmdlet.ParameterSetName -eq 'FromText') {
        $parsed = $parser::ParseInput($text, [ref]$tokens, [ref]$parseErrors)
    }
    elseif ($pscmdlet.ParameterSetName -eq 'FromFile') {
        $fileName = resolve-path $fileName
        $parsed = $parser::ParseFile($fileName, [ref]$tokens, [ref]$parseErrors)
    }
    write-verbose "$($tokens.count) tokens found."

    if ($parseErrors.count -gt 0) {
        $message = "$($parseErrors.count) parse errors found in file '$fileName':`r`n"
        $parseErrors |% { $message += "`r`n    $_" }
        if ($ThrowOnFailure) { throw $message } else { write-verbose $message }
        return $false
    }
    return $true
}

function Format-XML {
    Param (
        [Parameter(ValueFromPipeline=$true,Mandatory=$true,Position=0)] [System.Array] $xml
    )
    foreach ($xmlItem in $xml) {
        $StringWriter = New-Object system.io.stringwriter
        $XmlWriter = New-Object system.xml.xmltextwriter($StringWriter)
        $XmlWriter.Formatting = [System.xml.formatting]::Indented
        $xmlItem.WriteContentTo($XmlWriter)
        $StringWriter.ToString()
    }
}

<#
.synopsis
Send data over the network

.description
Send data over the network
Sort of like you might wanna do with netcat/nc, but too different to be "nc for powershell"

Source: https://gist.github.com/jstangroome/9adaa87a845e5be906c8

.example 'GET / HTTP/1.0', '' | Send-NetworkData -Computer www.powershellmagazine.com -Port 80
Pipe in a HTTP request

.example Send-NetworkData -Data 'GET / HTTP/1.0', '' -Computer www.powershellmagazine.com -Port 80 -Timeout 0:00:02
Use the Data parameter to do the same but only wait 2 seconds for a response:

.example Send-NetworkData -Data "EHLO $Env:ComputerName", "QUIT" -Computer mail.example.com -Port 25
Say hello to an SMTP server
#>
function Send-NetworkData {
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
}
