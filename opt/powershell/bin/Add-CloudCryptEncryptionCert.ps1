<#
.synopsis 
Import a certificate and private key from a PFX file on the CloudCrypt server
.description
Import a certificate and private key from a PFX file on the CloudCrypt server
It is still a bit fragile if you don't follow my assumptions, but I've tried to handle the normal case. 
Assumptions in the code: 
- You're logged on to your DLP domain account
- You have logged on to the CloudCrypt server before, and you have a %USERPROFILE% under C:\Users\<username>
.parameter pfxFile
An array of paths to PFX files
.parameter pfxPassword
An array of SecureString objects representing the PFX passwords
.parameter pfxPasswordInsecure
An array of String objects representing the PFX passwords. These get immediately converted to SecureString objects.
.parameter cloudCryptServer
The name of the CloudCrypt server. Defaults to "walnut.doubleline.us", but you can change it to e.g. "localhost" for testing. 
.example
Add-CloudCryptEncryptionCert -pfxFile ./certificate.pfx -pfxPasswordInsecure "password1" 
Import one certificate
.example
$pfxPW = read-host -AsSecureString; Add-CloudCryptEncryptionCert -pfxFile ./certificate.pfx -pfxPassword $pfxPW
Use a SecureString
.example
Add-CloudCryptEncryptionCert -pfxFile ./cert1.pfx,./cert2.pfx -pfxPasswordInsecure "password1","password2"
Import several certs at once
.example
Add-CloudCryptEncryptionCert -pfxFile ./certificate.pfx -pfxPasswordInsecure "password1" -cloudCryptServer localhost
Import certs to your local machine for testing purposes
#>
[cmdletbinding()] param(
    [parameter(mandatory=$true)] [string[]] 
    $pfxFile,

    [parameter(mandatory=$true,ParameterSetName="SecurePass")] 
    [System.Security.SecureString[]] [Alias("Secure")]
    $pfxPassword,

    [parameter(mandatory=$true,ParameterSetName="InsecurePass")]
    [string[]] [Alias("Insecure")]
    $pfxPasswordInsecure,

    [string] 
    $cloudCryptServer = "walnut.doubleline.us"
)

# These parameters are fragile and might need to change for you. 
# They assume that you are logged in to your local workstation with your domain account, 
# and they assume that you have logged on to the cloudCryptServer in the past
$userObj = [Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()
$username = $userObj.Identity.Name -replace "DOUBLELINE\\",""
$pfxDirUNC = "\\${cloudCryptServer}\C$\Users\${username}\Downloads"
$pfxDir = "C:\Users\${username}\Downloads"
$pfxFileName = $pfxFile |% { (get-item $_).Name }

if ($pscmdlet.ParameterSetName -eq "InsecurePass") {
    $pfxPassSec = $pfxPasswordInsecure |% { ConvertTo-SecureString -AsPlainText -Force $_ }
}
else {
    $pfxPassSec = $pfxPassword
}

if ($pfxFile.count -ne $pfxPassSec.count) {
    throw "Must use same number of -pfxFile and -pfxPassword/-pfxPasswordInsecure arguments"
}

copy-item $pfxFile $pfxDirUNC

invoke-command -computerName $cloudCryptServer -argumentList $pfxDir,$pfxFileName,$pfxPassSec -scriptblock {
    param(
        [parameter(mandatory=$true)] [string] $pfxDir,
        [parameter(mandatory=$true)] [string[]] $pfxFile,
        [parameter(mandatory=$true)] [System.Security.SecureString[]] $pfxPassword
    )
    if ($pfxFile.count -ne $pfxPassword.count) {
        throw "Must use same number of -pfxFile and -pfxPassword arguments"
    }

    $me = [Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()
    if (-not $me.isInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
        throw "I don't have permissions to add certificates to the store"
    }

    $storeLocation = "LocalMachine"
    $storeName = "My"
    $storePath = "cert:\${storeLocation}\${storeName}"

    for ($i=0; $i -lt $pfxFile.count; $i+=1) {
        $pfxPath = resolve-path "$pfxDir\$($pfxFile[$i])"
        $pfxPass = $pfxPassword[$i]

        # Flags: https://msdn.microsoft.com/en-us/library/system.security.cryptography.x509certificates.x509keystorageflags(v=vs.110).aspx
        # PersistKeySet: import the private key too (otherwise only the certificate is imported)
        # :MachineKeySet place the key in the machine key store (otherwise it is placed in the user keystore)
        #     *Different* from the "LocalMachine" store location! 
        #     Without this flag, certs imported to LM are stored in %USERPROFILE%\AppData\Microsoft\Crypto\RSA\MachineKeys
        #     When this flag is specified, certs imported to LM are stored in %PROGRAMDATA%\Microsoft\Crypto\RSA\MachineKeys
        $flags = [System.Security.Cryptography.X509Certificates.X509KeyStorageFlags]::PersistKeySet
        $flags = $flags -bxor [System.Security.Cryptography.X509Certificates.X509KeyStorageFlags]::MachineKeySet

        # Import the cert. We can't use Import-PfxCertificate for some reason... certs I've imported with that cmdlet do not 
        # seem to end up with a private key for the application to use. 
        $pfx = new-object System.Security.Cryptography.X509Certificates.X509Certificate2
        $pfx.import($pfxPath, $pfxPass, $flags)
        $store = new-object System.Security.Cryptography.X509Certificates.X509Store($storeName, $storeLocation)
        $store.open('MaxAllowed')
        $store.add($pfx)
        $store.close()

        # The keys are stored directly on the filesystem. Find the location...
        $containerName = (gci $storePath |? { $_.Thumbprint -like $pfx.thumbprint }).privateKey.CspKeyContainerInfo.UniqueKeyContainerName
        $keyPath = (resolve-path "${env:ProgramData}\Microsoft\Crypto\RSA\MachineKeys\$containerName").path

        # ... and use normal filesystem ACLs to give "Everyone" permission to use the private key
        $acl = get-acl $keyPath
        $newAR = new-object System.Security.AccessControl.FileSystemAccessRule "Everyone","Read","Allow"
        $acl.AddAccessRule($newAR)
        set-acl $keyPath $acl
    }
}


