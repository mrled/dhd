<#
.synopsis 
Import a certificate and private key from a PFX file on a remote server
.description
Import a certificate and private key from a PFX file on a remote server (intended to be used with https://github.com/danludwig/CloudConfigCrypto)
.parameter pfxFile
An array of paths to PFX files
.parameter pfxPassword
An array of SecureString objects representing the PFX passwords
.parameter pfxPasswordInsecure
An array of String objects representing the PFX passwords. These get immediately converted to SecureString objects.
.parameter computerName
The name of the remote server. Defaults to "localhost".
.parameter credential
A credential object (from e.g. Get-Credential) for an account with administrator authorization on the remote server. If unspecified, attempt to use the currently logged-in account
.example
Add-CloudCryptEncryptionCert -pfxFile ./certificate.pfx -pfxPasswordInsecure "password1" 
Import one certificate to your local machine
.example
$pfxPW = read-host -AsSecureString; Add-CloudCryptEncryptionCert -pfxFile ./certificate.pfx -pfxPassword $pfxPW
Use a SecureString when importing one certificate to your local machine
.example
Add-CloudCryptEncryptionCert -pfxFile ./cert1.pfx,./cert2.pfx -pfxPasswordInsecure "password1","password2"
Import several certs at once to your local machine
.example
Add-CloudCryptEncryptionCert -pfxFile ./certificate.pfx -pfxPasswordInsecure "password1" -computerName cloudcrypt.example.com
Import certs to a remote server
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

    [string] [Alias("cloudCryptServer")]
    $computerName = "localhost",

    [System.Management.Automation.PSCredential]
    $credential
)

if ($pscmdlet.ParameterSetName -eq "InsecurePass") {
    $pfxPassword = $pfxPasswordInsecure |% { ConvertTo-SecureString -AsPlainText -Force $_ }
}
if ($pfxFile.count -ne $pfxPassword.count) {
    throw "Must use same number of -pfxFile and -pfxPassword/-pfxPasswordInsecure arguments"
}

if ($credential) {
    $session = New-PSSession -computername $computerName -name "AddEncryptionCert" -credential $credential
}
else {
    $session = New-PSSession -computername $computerName -name "AddEncryptionCert"
}
$pfxFileName = $pfxFile |% { (get-item $_).Name }
$pfxDir = invoke-command -session $session -scriptBlock { 
    (mkdir -force "${env:temp}\AddEncryptionCert").fullname 
}
$pfxDriveLetter = $pfxDir[0]
$pfxDrivePath = $pfxDir.substring(2)
$pfxDirUNC = "\\$computerName\${pfxDriveLetter}$\$pfxDrivePath"
write-verbose "Using a UNC path of '$pfxDirUNC' to copy local file '$pfxFile' to remote server named '$computerName' at path '$pfxDir'"

copy-item $pfxFile $pfxDirUNC

invoke-command -session $session -argumentList $pfxDir,$pfxFileName,$pfxPassword -scriptblock {
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
        # MachineKeySet place the key in the machine key store (otherwise it is placed in the user keystore)
        #     See also: http://paulstovell.com/blog/x509certificate2
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

        rm $pfxPath
    }
}


