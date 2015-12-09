[cmdletbinding()]
param(
    $pfxPath,
    $pfxPass,
    $storeName = "My",
    $storeLocation = "LocalMachine"
)
$pfxPath = resolve-path $pfxPath
$storePath = "cert:\${storeLocation}\${storeName}"

# Flags: https://msdn.microsoft.com/en-us/library/system.security.cryptography.x509certificates.x509keystorageflags(v=vs.110).aspx
# PersistKeySet: import the private key too (otherwise only the certificate is imported)
# MachineKeySet place the key in the machine key store (otherwise it is placed in the user keystore)
#     See also: http://paulstovell.com/blog/x509certificate2
#     *Different* from the "LocalMachine" store location! 
#     Without this flag, certs imported to LM are stored in %USERPROFILE%\AppData\Roaming\Microsoft\Crypto\RSA
#     When this flag is specified, certs imported to LM are stored in %PROGRAMDATA%\Microsoft\Crypto\RSA\MachineKeys
$flags = [System.Security.Cryptography.X509Certificates.X509KeyStorageFlags]::PersistKeySet
$flags = $flags -bxor [System.Security.Cryptography.X509Certificates.X509KeyStorageFlags]::MachineKeySet

$pfx = new-object System.Security.Cryptography.X509Certificates.X509Certificate2
$pfx.import($pfxPath, $pfxPass, $flags)
$store = new-object System.Security.Cryptography.X509Certificates.X509Store($storeName, $storeLocation)
$store.open('MaxAllowed')
$store.add($pfx)
$store.close()

# The keys are stored directly on the filesystem. Find the location...
$importedCert = gci $storePath |? { $_.Thumbprint -like $pfx.thumbprint }
$containerName = $importedCert.privateKey.CspKeyContainerInfo.UniqueKeyContainerName
if ($importedCert.privateKey.CspKeyContainerInfo.MachineKeyStore) {
    $keyPath = (resolve-path "${env:ProgramData}\Microsoft\Crypto\RSA\MachineKeys\$containerName").path
}
else {
    $keyPath = (resolve-path "${env:AppData}\Microsoft\Crypto\RSA\$containerName").path
}

# ... and use normal filesystem ACLs to give "Everyone" permission to use the private key
$acl = get-acl $keyPath
$newAR = new-object System.Security.AccessControl.FileSystemAccessRule "Everyone","Read","Allow"
$acl.AddAccessRule($newAR)
set-acl $keyPath $acl


