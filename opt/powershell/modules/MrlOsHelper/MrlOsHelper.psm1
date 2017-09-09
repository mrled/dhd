<#
.description
All OS-specific stuff should go in here
#>

switch ($PsVersionTable.OS) {
    "Windows" {
        ## Special Objects

        # The System.Environment+SpecialFolders enum has a lot of really useful stuff such as:
        # - StartMenu / CommonStartMenu: the start menu folder for my user / all users
        # - StartUp / CommonStartUp: the startup folder for my user / all users
        # ... and lots more. This makes a hashtable from that, so it's easier to access
        $SpecialFolders = New-Object PSObject
        foreach ($sf in [system.Enum]::GetValues([System.Environment+SpecialFolder])) {
            $sfpath = [Environment]::GetFolderPath($sf)
            add-member -inputobject $SpecialFolders -membertype NoteProperty -name $sf -value $sfpath -force
        }

        # Make an object analogous to $profile but for the start menu
        $StartMenu = New-Object PSObject -Property @{
            CurrentUser = "${env:AppData}\Microsoft\Windows\Start Menu\"
            AllUsers = "${env:ProgramData}\Microsoft\Windows\Start Menu\"
        }
        Add-Member -Force -InputObject $StartMenu -MemberType ScriptMethod -Name ToString -Value {$this.CurrentUser}
    }
}
