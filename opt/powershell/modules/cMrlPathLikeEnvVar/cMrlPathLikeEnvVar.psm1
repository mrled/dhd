enum Ensure {
    Absent
    Present
}

enum InsertionMode {
    Append
    Prepend
}

[DscResource()] class cMrlPathLikeEnvVar {
    # The name of the environment variable
    [DscProperty(Key)]
    [string] $Name

    # The directory to add to the environment variable, if it exists
    [DscProperty(Key)]
    [string] $Location

    [DscProperty()]
    [Ensure] $Ensure = [Ensure]::Present

    # Whether we append it to the end of the list, or prepend it to the beginning
    [DscProperty()]
    [InsertionMode] $InsertionMode = [InsertionMode]::Append

    # Only insert the $Location if it exists on the filesystem
    [DscProperty()]
    [bool] $OnlyIfExists = $false

    # $Location includes a wildcard character like a * or ?
    # Take the last-ordered item that matches the $Location pattern on the filesystem
    # For instance, if C:\Python27 and C:\Python36 exist,
    # and $Location is "C:\Python*", select C:\Python36
    # Implies $OnlyIfExists
    [DscProperty()]
    [bool] $Wildcard = $false

    [DscProperty(NotConfigurable)]
    [bool] $LocationExists

    [void] Set() {
        if ($this.TestShouldInsert()) {
            Write-Verbose -Message "$($this.InsertionMode)ing '$($this.ExpandedLocation())' to $($this.GetTarget())'s $($this.Name) variable"
            $this.AddLocation()
        } elseif ($this.TestShouldRemove()) {
            Write-Verbose -Message "Removing '$($this.ExpandedLocation())' from $($this.GetTarget())'s $($this.Name) variable"
            $this.RemoveLocation()
        } else {
            Write-Verbose -Message "Nothing to do"
        }
    }

    [bool] Test() {
        if ($this.TestLocationInVar()) {
            Write-Verbose -Message "The $($this.GetTarget())'s $($this.Name) variable contains the location $($this.ExpandedLocation())"
        } else {
            Write-Verbose -Message "The $($this.GetTarget())'s $($this.Name) does not contain the location $($this.ExpandedLocation())"
        }
        return $this.TestLocationInVar() -and $this.Ensure -eq [Ensure]::Present
    }

    [cMrlPathLikeEnvVar] Get() {
        $this.Ensure = if ($this.TestLocationInVar()) {[Ensure]::Present} else {[Ensure]::Absent}
        $this.LocationExists = $this.TestLocationExists()
        return $this
    }

    # This module supports passing a string value with a wildcard in the
    # Location property
    # In that case, we take the _alphabetically last_ item returned by Get-Item
    # This lets us pass a location of say C:\Python3* on a system that has
    # multiple Pythons installed, say Python34, Python35, and Python36,
    # and only use the most recent version.
    # It also lets us use something reasonable in software like Python which
    # changes its god damned installation directory at each fucking version.
    [string] ExpandedLocation() {
        try {
            $item = Get-Item -Path $this.Location -ErrorAction Stop
            if ($item.Count -eq 1) {
                $expLoc = Select-Object -InputObject $item -ExpandProperty FullName
                Write-Verbose -Message "Expanding location pattern of '$($this.Location)' resulted in '$expLoc'"
            } elseif ($item.Count -lt 1) {
                $expLoc = $null
                Write-Verbose -Message "Expanding location pattern of '$($this.Location)' resulted in no existing paths"
            } else {
                $expLoc = Select-Object -InputObject $item -Last 1 -ExpandProperty FullName
                Write-Verbose -Message "Expanding location pattern of '$($this.Location)' resulted in  $($item.Count) items; we will use the alphabetically last one at '$expLoc'"
            }
            return $expLoc
        } catch {
            Write-Verbose -Message "Expanding location pattern of '$($this.Location)' returned no extant paths"
            return $null
        }
    }

    [bool] TestShouldInsert() {
        if ($this.OnlyIfExists -and -not $this.TestLocationExists()) {
            return $false
        }
        return $this.Ensure -eq [Ensure]::Present -and -not $this.TestLocationInVar()
    }

    [bool] TestShouldRemove() {
        if ($this.OnlyIfExists -and $this.TestLocationInVar() -and -not $this.TestLocationExists()) {
            return $true
        }
        return $this.Ensure -eq [Ensure]::Absent -and $this.TestLocationInVar()
    }

    [bool] TestLocationInVar() {
        return $this.GetVar() -Split ";" -Contains $this.ExpandedLocation()
    }

    [string] GetTarget() {
        if ($global:PsDscContext.RunAsUser) {
            Write-Verbose -Message "Running in User context for '$($global:PsDscContext.RunAsUser)'"
            return "User"
        } else {
            Write-Verbose -Message "Running in Machine context"
            return "Machine"
        }
    }

    [bool] TestLocationExists() {
        if (-not $this.ExpandedLocation()) {
            return $false
        }
        return Test-Path -LiteralPath $this.ExpandedLocation()
    }

    [void] AddLocation() {
        if ($this.InsertionMode -eq [InsertionMode]::Append) {
            $newValue = "$($this.GetVar());$($this.ExpandedLocation())"
        } else {
            $newValue = "$($this.ExpandedLocation());$($this.GetVar())"
        }
        [System.Environment]::SetEnvironmentVariable($this.Name, $newValue, $this.GetTarget())
    }

    [void] RemoveLocation() {
        [System.Environment]::SetEnvironmentVariable(
            $this.Name,
            $($this.GetVar() -Split ";" -ne $this.ExpandedLocation()) -Join ";",
            $this.GetTarget())
    }

    [string] GetVar() {
        return [System.Environment]::GetEnvironmentVariable($this.Name, $this.GetTarget())
    }
}
