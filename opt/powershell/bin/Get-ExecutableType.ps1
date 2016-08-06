#Requires -Version 2.0

<#
.Synopsis
Determines whether an executable file is 16-bit, 32-bit or 64-bit.
.DESCRIPTION
Attempts to read the MS-DOS and PE headers from an executable file to determine its type.
The command returns one of four strings (assuming no errors are encountered while reading the
file):
"Unknown", "16-bit", "32-bit", or "64-bit"
.PARAMETER Path
Path to the file which is to be checked.
.EXAMPLE
Get-ExecutableType -Path C:\Windows\System32\more.com
.INPUTS
None.  This command does not accept pipeline input.
.OUTPUTS
String
.LINK
http://msdn.microsoft.com/en-us/magazine/cc301805.aspx
#>

[CmdletBinding()] param (
    [Parameter(Mandatory = $true)] [string] $Path
)

if (-not (Test-Path -LiteralPath $Path -PathType Leaf)) {
    throw "Must specify an existing executable file"
}

try {
    try {
        $stream = New-Object System.IO.FileStream(
            $PSCmdlet.GetUnresolvedProviderPathFromPSPath($Path),
            [System.IO.FileMode]::Open,
            [System.IO.FileAccess]::Read,
            [System.IO.FileShare]::Read
        )
    }
    catch {
        throw "Error opening file $Path for Read: $($_.Exception.Message)"
    }

    $exeType = 'Unknown'

    if ([System.IO.Path]::GetExtension($Path) -eq '.COM') {
        # 16-bit .COM files may not have an MS-DOS header.  We'll assume that any .COM file with no header
        # is a 16-bit executable, even though it may technically be a non-executable file that has been
        # given a .COM extension for some reason.
        $exeType = '16-bit'
    }

    $bytes = New-Object byte[](4)

    if ($stream.Length -ge 64 -and
        $stream.Read($bytes, 0, 2) -eq 2 -and
        $bytes[0] -eq 0x4D -and $bytes[1] -eq 0x5A)
    {
        $exeType = '16-bit'

        if ($stream.Seek(0x3C, [System.IO.SeekOrigin]::Begin) -eq 0x3C -and
            $stream.Read($bytes, 0, 4) -eq 4)
        {
            if (-not [System.BitConverter]::IsLittleEndian) { [Array]::Reverse($bytes, 0, 4) }
            $peHeaderOffset = [System.BitConverter]::ToUInt32($bytes, 0)

            if ($stream.Length -ge $peHeaderOffset + 6 -and
                $stream.Seek($peHeaderOffset, [System.IO.SeekOrigin]::Begin) -eq $peHeaderOffset -and
                $stream.Read($bytes, 0, 4) -eq 4 -and
                $bytes[0] -eq 0x50 -and $bytes[1] -eq 0x45 -and $bytes[2] -eq 0 -and $bytes[3] -eq 0)
            {
                $exeType = 'Unknown'

                if ($stream.Read($bytes, 0, 2) -eq 2) {
                    if (-not [System.BitConverter]::IsLittleEndian) { [Array]::Reverse($bytes, 0, 2) }
                    $machineType = [System.BitConverter]::ToUInt16($bytes, 0)

                    switch ($machineType) {
                        0x014C { $exeType = '32-bit' }
                        0x0200 { $exeType = '64-bit' }
                        0x8664 { $exeType = '64-bit' }
                    }
                }
            }
        }
    }

    return $exeType
}
finally {
    if ($null -ne $stream) { $stream.Dispose() }
}

