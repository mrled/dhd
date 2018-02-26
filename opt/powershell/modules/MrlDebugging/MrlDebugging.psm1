

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