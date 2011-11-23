# In order to set this file as your profile, the easiest
# way is to open up PowerShell, type `$profile`, and save the 
# following text in that file: 
#     . $Home\.dhd\hbase\Microsoft.PowerShell_profile.ps1 

# Note that to run commands on remote machines you'll need WinRM
# enabled - it isn't by default. 
# <http://technet.microsoft.com/en-us/magazine/ff700227.aspx>

function .p {
    . $Home\.dhd\hbase\Microsoft.PowerShell_profile.ps1 
}
set-alias .b .p

# original version from <http://www.techmumbojumblog.com/?p=39>
# I changed it so it uses invoke-command rather than WMI for remoting
# this means it works only w/ PowerShell 2.0 I think
# neither WMI nor PS remoting enabled by default, but I always enable PS
# remoting on my domains anyway. It does limit it to PS 2.0 though (no XP?)
function Get-InstalledPrograms ($computer = 'localhost') {
	$programs_installed = @{};
    $win32_product = @(invoke-command -computername $computer -scriptblock {get-wmiobject -class 'Win32_Product'});
	foreach ($product in $win32_product) {
        $name = $product.Name;
        $version = $product.Version;
        if ($name -ne $null) {
            $programs_installed.$name = $version;
		}
	}
	return $programs_installed;
}

