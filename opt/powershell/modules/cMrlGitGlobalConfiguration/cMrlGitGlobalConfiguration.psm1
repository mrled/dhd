enum Ensure {
    Absent
    Present
}

[DscResource()] class cMrlGitGlobalConfiguration {
    # The name of the configuration option
    [DscProperty(Key)]
    [string] $Name

    # The value for the configuration option
    [DscProperty(Mandatory)]
    [string] $Value

    [DscProperty(Mandatory)]
    [Ensure] $Ensure

    [void] Set() {
        if ($this.Ensure -eq [Ensure]::Present -and $this.CurrentValue() -ne $this.Value) {
            Write-Verbose -Message "Setting git $($this.Name) setting to $($this.Value)"
            git config --global "$($this.Name)" "$($this.Value)"
        } elseif ($this.Ensure -eq [Ensure]::Absent) {
            Write-Verbose -Message "Unsetting git $($this.Name) setting"
            git config --global --unset "$($this.Name)"
        }
    }

    [bool] Test() {
        if ($this.CurrentValue() -eq $this.Value) {
            Write-Verbose -Message "The $($this.Name) property is already set to $($this.Value)"
        } else {
            Write-Verbose -Message "The $($this.Name) property is set to $($this.CurrentValue()), not $($this.Value))"
        }
        return $this.CurrentValue() -eq $this.Value -and $this.Ensure -eq [Ensure]::Present
    }

    [cMrlGitGlobalConfiguration] Get() {
        if ($this.CurrentValue() -eq $this.Value) {
            $this.Ensure = [Ensure]::Present
        } else {
            $this.Ensure = [Ensure]::Absent
            $this.Value = $this.CurrentValue()
        }
        return $this
    }

    [string] CurrentValue() {
        return $(git config --global --get "$this.Name")
    }
}
