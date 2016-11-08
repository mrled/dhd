# Detect the operating system

$powershellVersion = $psversiontable.PSVersion.ToString()
if (Test-Path /usr/bin/uname) {
    $osType = "Unix"
    $osVersion = /usr/bin/uname -a
}
else {
    $osType = "Windows"
    $osVersion = [System.Environment]::OSVersion.VersionString
}


