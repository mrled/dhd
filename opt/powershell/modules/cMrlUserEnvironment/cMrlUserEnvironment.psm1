enum Ensure {
    Absent
    Present
}

[DscResource()]
class cMrlUserEnvironment {
    [DscProperty(Key)]
    [string] $Name

    [DscProperty()]
    [string] $Value

    # [DscProperty()]
    # [bool] $Path

    [DscProperty(Mandatory)]
    [Ensure] $Ensure

    [void] Set() {
        $curVal = [Environment]::GetEnvironmentVariable($this.Name, "User")

        if ($this.Ensure -eq [Ensure]::Present -and -not $curVal -eq $this.Value) {
            [Environment]::SetEnvironmentVariable($this.Name, $this.Value, "User")
        } elseif ($this.Ensure -eq [Ensure]::Absent) {
            [Environment]::SetEnvironmentVariable($this.Name, $null, "User")
        }
    }

    [bool] Test() {
        $curVal = [Environment]::GetEnvironmentVariable($this.Name, "User")
        if ($curVal -eq $this.Value) {
            Write-Verbose -Message "User environment variable '$($this.Name)' is set to '$($this.Value)'"
        } else {
            Write-Verbose -Message "User environment variable '$($this.Name)' is set to '$curVal', not '$($this.Value)'"
        }
        return $curVal -eq $this.Value -and $this.Ensure -eq [Ensure]::Present
    }

    [cMrlUserEnvironment] Get() {
        $curVal = [Environment]::GetEnvironmentVariable($this.Name, "User")
        if ($this.Value -eq $curVal) {
            $this.Ensure = [Ensure]::Present
        } else {
            $this.Ensure = [Ensure]::Absent
        }
        $this.Value = $curVal
        return $this
    }

}
