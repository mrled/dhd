

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
