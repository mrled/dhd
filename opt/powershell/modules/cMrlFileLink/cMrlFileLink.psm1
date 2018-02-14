enum Ensure {
    Absent
    Present
}

<#
Manage shortcuts
#>
[DscResource()] class cMrlFileLink {
    # Fully qualified path to the shortcut file
    # Note that the extension *must* end in .lnk, or we throw an error
    [DscProperty(Key)]
    [string] $LinkPath

    # Fully qualified path to the shortcut target
    [DscProperty(Mandatory)]
    [string] $LinkTarget

    [DscProperty(Mandatory)]
    [Ensure] $Ensure

    [void] Set() {
        $this.ValidateProperties()
        if ($this.Ensure -eq [Ensure]::Present -and -not $this.TestLink()) {
            Write-Verbose -Message "Creating link at path $($this.LinkPath) to target $($this.LinkTarget)"
            $linkParent = Split-Path -LiteralPath $this.LinkPath
            New-Item -Type Directory -Force -Path $linkParent | Out-Null
            if (Test-Path -LiteralPath $this.LinkPath) {
                Remove-Item -Force -LiteralPath $this.LinkPath
            }
            New-Item -ItemType SymbolicLink -Path $this.LinkPath -Target $this.LinkTarget
        } elseif ($this.Ensure -eq [Ensure]::Absent -and (Test-Path -LiteralPath $this.LinkPath)) {
            Write-Verbose -Message "Deleting the file $($this.LinkPath)"
            Remove-Item -LiteralPath $this.LinkPath -Force
        }
    }

    [bool] Test() {
        $this.ValidateProperties()
        if ($this.TestLink()) {
            Write-Verbose -Message "Link to $($this.LinkTarget) exists at $($this.LinkPath)"
        } else {
            Write-Verbose -Message "Link to $($this.LinkTarget) does not exist at $($this.LinkPath)"
        }
        return $this.TestLink() -and $this.Ensure -eq [Ensure]::Present
    }

    [cMrlFileLink] Get() {
        $this.ValidateProperties()
        if ($this.TestShortcut()) {
            $this.Ensure = [Ensure]::Present
        } else {
            $this.Ensure = [Ensure]::Absent
        }
        return $this
    }

    # Not sure if there's a better way to do this?
    [void] ValidateProperties() {
        if (-not [System.IO.Path]::IsPathRooted($this.LinkPath)) {
            throw "The value of the LinkPath DSC property is not a rooted path"
        }
        if (-not [System.IO.Path]::IsPathRooted($this.LinkTarget)) {
            throw "The value of the LinkTarget DSC property is not a rooted path"
        }
    }

    [bool] TestLink() {
        if (-not (Test-Path -LiteralPath $this.LinkPath)) {
            return $false
        }
        $item = Get-Item -LiteralPath $this.LinkPath
        $itemPropertyNames = Get-Member -InputObject $item | Select-Object -ExpandProperty Name
        if ($itemPropertyNames -Contains 'LinkType' -and $item.LinkType -eq 'SymbolicLink') {
            if ($itemPropertyNames -contains 'Target' -and $item.Target -eq $this.LinkTarget) {
                return $true
            }
        }
        return $false
    }
}
