<#
.SYNOPSIS
Convert line endings in a file
.PARAMETER Path
The path to the file
.PARAMETER Format
The line ending format to use
.NOTES
This reads the entire file into memory because I'm lazy
#>
[CmdletBinding()] Param(
    [Parameter(Mandatory, ValueFromPipeline=$True)] $Path,
    [ValidateSet('Unix', 'Windows')] $Format = "Unix"
)
$resolvedPath = Get-Item -Force -Path $Path | Select-Object -ExpandProperty FullName
switch ($Format) {
    "Unix" { $replacement = @("`r`n", "`n") }
    "Windows" { $replacement = @("`n", "`r`n") }
    default { Write-Error -Message "Unknown format '$Format'" }
}
$replaced = [IO.File]::ReadAllText($resolvedPath) -Replace $replacement
[IO.File]::WriteAllText($resolvedPath, $replaced)
