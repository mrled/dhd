

<#
.SYNOPSIS
Show an object's members and other properties.

.DESCRIPTION
Show an object's members and other properties. Useful during debugging.
Displays the default output, the result of .ToString(), the type, and each object member.
Each member is displayed as member type (e.g. property), value type (e.g. string), member name, and value.
Eliminates "is that an empty property, or a property with an empty .ToString() method" question.

.PARAMETER InputObject
An object to inspect
#>
function Show-ObjectProperties {
    [CmdletBinding()] Param(
        [Parameter(Mandatory, ValueFromPipeline)] $InputObject
    )

    $outMembers = @()
    foreach ($member in (Get-Member -InputObject $InputObject)) {
        $value = $InputObject.$($member.Name)
        $outMembers += New-Object -TypeName PSObject -Property @{
            Name = $member.Name
            MemberType = $member.MemberType
            ValueType = if ($value -Eq $null) {'$null'} else {$value.GetType()}
            Value = $value
        }
    }

    Write-Host -ForegroundColor Green -Object "`r`nPowershell's default representation of the input object:"
    Write-Output -InputObject $InputObject
    Write-Host -ForegroundColor Green -Object "`r`n`$InputObject.ToString():`r`n"
    Write-Output -InputObject $InputObject.ToString()
    Write-Host -ForegroundColor Green -Object "`r`nObject type:`r`n"
    Write-Output -InputObject $($InputObject.GetType().FullName)
    Write-Host -ForegroundColor Green -Object "`r`nObject members:"
    $outMembers | Format-Table -Property Name, MemberType, ValueType, Value
}

<#
.DESCRIPTION
Retrieves all available Exceptions in the current session. Returns an array of strings which represent the exception type names.

.NOTES
Originally from: http://www.powershellmagazine.com/2011/09/14/custom-errors/
#>
function Get-AvailableExceptionsList {
    [CmdletBinding()] Param()
    $irregulars = 'Dispose|OperationAborted|Unhandled|ThreadAbort|ThreadStart|TypeInitialization'
    foreach ($assembly in [AppDomain]::CurrentDomain.GetAssemblies()) {
        try {
            $AllExceptions = $assembly.GetExportedTypes() -match 'Exception' -notmatch $irregulars
        }
        catch {
            Write-Verbose -Message "Could not get exported types for assembly '$assembly'"
            continue
        }
        foreach ($exc in $AllExceptions) {
            if (-not $exc.GetConstructors()) {
                Write-Verbose -Message "No constructors for '$exc' in '$assembly'"
                continue
            }
            try {
                $TestException = New-Object -TypeName $exc.FullName
                $TestError = New-Object -TypeName Management.Automation.ErrorRecord -ArgumentList $($TestException, 'ErrorID', 'OpenError', 'Target')
            } catch {
                # Tests failed, don't add this as a relevant exception
                Write-Verbose -Message "Could not create test exception/error for '$exc' in '$assembly'"
                continue
            }
            Write-Output $exc.FullName
        }
    }
}

<#
.SYNOPSIS
Show an error report

.PARAMETER ErrorList
An array of errors to report on. (Defaults to the value of the $Error variable)

.PARAMETER ExitCode
An exit code to report on. (Defaults to the value of the $LASTEXITCODE variable)

.PARAMETER WrapWidth
Specify a custom wrap width.
By default, use $Host.UI.RawUI.Buffersize.Width if that property exists, or else 9999.

.PARAMETER ExitIfErrors
If any errors are present, exit with a nonzero code.
#>
function Show-ErrorReport {
    [CmdletBinding()] Param(
        [Array] $ErrorList = $global:Error,
        [Int] $ExitCode = $global:LASTEXITCODE,
        [switch] $ExitIfErrors,
        [Int] $WrapWidth
    )

    function WrapText {
        param($text, $width, $indentSpaces)
        $width = $width -1
        $indent = " " * $indentSpaces
        foreach ($line in ($text -split "`n")) {
            while ($line.length -gt $width) {
                $line = "$indent$line"
                Write-Output -InputObject $line.substring(0,$width)
                $line = $line.substring($width)
            }
            Write-Output -InputObject "$indent$line"
        }
    }

    if (-not $WrapWidth) {
        if ($Host.UI.RawUI.Buffersize.Width) {
            $WrapWidth = $Host.UI.RawUI.Buffersize.Width
        } else {
            $WrapWidth = 9999
        }
    }

    if ($ErrorList.count -or $ExitCode) {
        Write-Output -InputObject "ERROR Report: `$LASTEXITCODE=$ExitCode, `$Error.count=$($Error.count)"
        for ($i=$ErrorList.count -1; $i -ge 0; $i-=1) {
            $err = $ErrorList[$i]
            Write-Output -InputObject "`$Error[$i]:"

            # $error can contain at least 2 kind of objects - ErrorRecord objects, and things that wrap ErrorRecord objects
            # The information we need is found in the ErrorRecord objects, so unwrap them here if necessary
            if ($err.PSObject.Properties['ErrorRecord']) {$err = $err.ErrorRecord}

            WrapText -text $err.ToString() -width $WrapWidth -indentSpaces 4

            if ($err.ScriptStackTrace) {
                WrapText -text $err.ScriptStackTrace -width $WrapWidth -indentSpaces 8
            }
        }
        if ($ExitIfErrors) {
            exit 1
        }
    }
    else {
        Write-Output -InputObject "ERROR Report: No errors"
    }
}

<#
.SYNOPSIS
Clear the $Error and $LASTEXITCODE variables
#>
function Clear-Error {
    [CmdletBinding()] Param()
    $global:Error.Clear()
    $global:LASTEXITCODE = 0
}

<#
.SYNOPSIS
Get the type of an error

.PARAMETER ErrorObject
An error object to examine
#>
function Get-ErrorType {
    [CmdletBinding()] Param(
        $ErrorObject = $global:Error[0]
    )
    if (-not $ErrorObject) {
        throw "No object passed as -ErrorObject, and `$Error is empty"
    }

    $exception = if ($ErrorObject.Exception) {$ErrorObject.Exception} else {$ErrorObject}
    $errNamespace = $exception.GetType().Namespace
    $errName = $exception.GetType().Name
    return "${errNamespace}.${errName}"
}

Set-Alias -Name err -Value Show-ErrorReport
Set-Alias -Name clerr -Value Clear-Error

<#
.SYNOPSIS
Get all loaded assemblies

.PARAMETER Name
Only return an assembly whose Name or FullName is like this string

.OUTPUTS
System.Reflection.RuntimeAssembly

.NOTES
Originally from https://groups.google.com/forum/#!topic/microsoft.public.windows.powershell/U00MT33QMBs
#>
function Get-Assembly {
    [CmdletBinding()] Param(
        [string] $Name
    )
    foreach ($asm in [AppDomain]::CurrentDomain.GetAssemblies()) {
        if (-Not $Name -or $asm.GetName().Name -like $Name -or $asm.FullName -like $Name) {
            Add-Member -InputObject $asm -PassThru -Force -NotePropertyMembers @{
                Name = $asm.GetName().Name
                Version = $asm.GetName().Version
            }
        }
    }
}

<#
.SYNOPSIS
Show the origin of a type

.PARAMETER Name
Only return types whose Name or FullName is like this string

.PARAMETER Assembly
Only return types whose assembly Name or assembly FullName is like this string

.OUTPUTS
System.RuntimeType

.NOTES
Originally from https://groups.google.com/forum/#!topic/microsoft.public.windows.powershell/U00MT33QMBs
#>
function Get-Type {
    [CmdletBinding()] Param(
        [string] $Name,
        [string] $Assembly
    )
    foreach ($asm in (Get-Assembly -Name $Assembly)) {
        foreach ($type in $asm.GetTypes()) {
            if (-not $Name -or $type.Name -like $Name -or $type.FullName -like $Name) {
                Add-Member -InputObject $type -PassThru -Force -NotePropertyMembers @{
                    AssemblyName = $asm.Name
                    AssemblyVersion = $asm.Version
                }
            }
        }
    }
}
