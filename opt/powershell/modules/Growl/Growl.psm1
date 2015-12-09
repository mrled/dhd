Set-StrictMode -Version 2

$GrowlConnectorDllPath = "$(Split-Path (gp HKCU:\Software\Growl).'(default)')\Growl.Connector.dll"
[Reflection.Assembly]::LoadFrom($GrowlConnectorDllPath) | Out-Null

$script:PowerGrowler = New-Object "Growl.Connector.GrowlConnector" 
$script:PowerGrowlerNotices = @{}
$script:DefaultAppName = "PowerGrowler"
$script:DefaultGrowlIcon = "${PSScriptRoot}\growl.png"
$script:DefaultTypeName = "Default"

<#
.Synopsis
Register a new Type name for growl notices from PowerGrowl
.Description
Creates a new type name that can be used for sending growl notices
.Parameter AppName
The name of the application you want to register as
.Parameter Name
The type name to be used sending growls
.Parameter DisplayName
The test to use for display (defaults to use the same value as the type name)
.Parameter Icon
Overrides the default icon of the message (accepts .ico, .png, .bmp, .jpg, .gif etc)
.Parameter MachineName
The name of a remote machine to register remotely instead of locally.
.Parameter Priority
Overrides the default priority of the message (use sparingly)
.Example
Register-GrowlType "PoshTwitter" "Command Completed"
  
Registers the type "Command Completed," using the default icon, for sending notifications to the local PC
#>
function Register-GrowlType {
    param(
        [Parameter(Mandatory=$true)] [String] $AppName,
        [Parameter(Mandatory=$true)] [ValidateScript( { 
                !$script:PowerGrowlerNotices.Contains($AppName) -OR 
                !$script:PowerGrowlerNotices.$AppName.Notices.ContainsKey($_)} )]
            [String] $Name,
        [String] $DisplayName = $Name,
        [String] $Icon = $script:DefaultGrowlIcon,
        [String] $MachineName,
        [String] $AppIcon
    )
  
    [Growl.Connector.NotificationType]$Notice = $Name
    $Notice.DisplayName = $DisplayName
    $Notice.Icon = Convert-Path (Resolve-Path $Icon)

    if($MachineName) {
        $Notice.MachineName = $MachineName
    }
    if(!$script:PowerGrowlerNotices.Contains($AppName)) {
       $script:PowerGrowlerNotices.Add( $AppName, ([Growl.Connector.Application]$AppName) )

       $script:PowerGrowlerNotices.$AppName = Add-Member -input $script:PowerGrowlerNotices.$AppName -Name Notices -Type NoteProperty -Value (New-Object hashtable) -Passthru
        $script:PowerGrowlerNotices.$AppName.Icon = Convert-Path (Resolve-Path $AppIcon)
    }
   
    $script:PowerGrowlerNotices.$AppName.Notices.Add( $Name, $Notice )

    $script:PowerGrowler.Register( 
        $script:PowerGrowlerNotices.$AppName, 
        [Growl.Connector.NotificationType[]]@($script:PowerGrowlerNotices.$AppName.Notices.Values) )
}

## Register the "PowerGrowler" "Default" notice so everything works out of the box
Register-GrowlType -AppName $script:DefaultAppname -Name $script:DefaultTypeName -appIcon $script:DefaultGrowlIcon

<#
.Synopsis
Set the Growl password
.Description
Set the password and optionally, the encryption algorithm, for communicating with Growl
.Parameter Password
The password for Growl
.Parameter Encryption
The algorithm to be used for encryption (defaults to AES)
.Parameter KeyHash
The algorithm to be used for key hashing (defaults to SHA1)
#>
function Set-GrowlPassword { 
    PARAM( 
        [Parameter(Mandatory=$true)] [String]$Password,
        [ValidateSet( "AES", "DES", "RC2", "TripleDES", "PlainText" )]
            [String]$Encryption = "AES",
        [ValidateSet( "MD5", "SHA1", "SHA256", "SHA384", "SHA512" )]
            [String]$KeyHash = "SHA1"
    )   
    $script:PowerGrowler.EncryptionAlgorithm = [Growl.Connector.Cryptography+SymmetricAlgorithmType]::"$Encryption"
    $script:PowerGrowler.KeyHashAlgorithm = [Growl.Connector.Cryptography+SymmetricAlgorithmType]::"$KeyHash"
    $script:PowerGrowler.Password = $Password
}

<#
.Synopsis
Register a script to be called when each notice is finished. 
.Description
Registers a scriptblock as a handler for the NotificationCallback event. You should accept two parameters, a Growl.Connector.Response and a Growl.Connector.CallbackData object.

The NotificationCallback only happens when a callback is requested, which in this Growl library only happens if you pass both CallbackData and CallbackType to the Send-Growl function.
.Example
$handler = { 
    PARAM( $response, $context )
    Write-Host "Response $($response|out-string)" -fore Cyan
    Write-Host "Context $($context|fl|out-string)" -fore Green
    Write-Host $("Response Type: {0}`nNotification ID: {1}`nCallback Data: {2}`nCallback Data Type: {3}`n" -f $context.Result, $context.NotificationID, $context.Data, $context.Type) -fore Yellow
}
Register-GrowlCallback -Handler $handler

Registers an informational debugging-style handler. 
#>
function Register-GrowlCallback {
    PARAM(
        [Parameter(Mandatory=$true)] [Scriptblock]$Handler
    )
    Register-ObjectEvent $script:PowerGrowler NotificationCallback -Action $Handler
}

<#
.Synopsis
Send a growl notice
.Description
Send a growl notice with the scpecified values
.Parameter Caption
The short caption to display
.Parameter Message
The message to send (most displays will resize to accomodate)
.Parameter AppName
The Application Name to send. This MUST be previously registered. (See also -NoticeType.)
.Parameter NoticeType
The type of notice to send. This MUST be the name of one of the registered types, and senders should bear in mind that each registered type has user-specified settings, so you should not abuse the types, but create your own for messages that will recur.
For example, the user settings allow certain messages to be disabled, set to a different "Display", or to have their Duration and Stickyness changed, as well as have them be Forwarded to another device, have Sounds play, and set different priorities.
.Parameter CallbackUrl 
The URL to use as callback
.Parameter CallbackData
Data to use as callback
.Parameter CallbackType
Type of data callback
.Parameter NoCallback
No callback (clicking on the Growl notification does nothing)
.Parameter Icon
Overrides the default icon of the message (accepts .ico, .png, .bmp, .jpg, .gif etc)
.Parameter Priority
Overrides the default priority of the message (use sparingly)
.Example
Send-Growl "Greetings" "Hello World!"

The Hello World of Growl.
.Example
Register-GrowlType -AppName "MailGrowlerExample" -Name "NewMailArrival" -appIcon ~\Icons\NewMailIcon.png
Send-Growl "You've got Mail!" "Message for you sir!" -AppName "MailGrowlerExample" -NoticeType "NewMailArrival" 

Displays a message with a couple of movie quotes and a mail icon. Uses custom AppName and NoticeType parameters so that users can then provide customized sounds/appearance of mail notifications from within the Growl GUI. 
#>
function Send-Growl {
    [CmdletBinding(DefaultParameterSetName="NoCallback")]
    PARAM (
        [Parameter(Mandatory=$true, Position=0)] [string] $Caption,
        [Parameter(Mandatory=$true, Position=1)] [string] $Message,
        [Parameter(Position=2)]
            [ValidateScript( {$script:PowerGrowlerNotices.Contains($AppName)} )]
            [string] $AppName = $script:DefaultAppName,
        [Parameter(Position=3)]    
            [ValidateScript( {$script:PowerGrowlerNotices.$AppName.Notices.ContainsKey($_)} )]   
            [string] [Alias("Type")] $NoticeType = $script:DefaultTypeName,
        [Parameter(ParameterSetName="UrlCallback")] 
            [Uri] $CallbackUrl = "",
        [Parameter(Mandatory=$true, ParameterSetName="DataCallback")] [string] $CallbackData,
        [Parameter(Mandatory=$true, ParameterSetName="DataCallback")] [string] $CallbackType,
        [Parameter(ParameterSetName="NoCallback")] [switch] $NoCallback, 
        [string] $Icon = $script:DefaultGrowlIcon,
        [Growl.Connector.Priority] $Priority = "Normal" 
    )

    $datetime = (Get-Date).Ticks.ToString()
    $notice = New-Object Growl.Connector.Notification $appName, $NoticeType, $datetime, $caption, $Message
   
    if ($Icon) { $notice.Icon = Convert-Path (Resolve-Path $Icon) }
    if ($Priority) { $notice.Priority = $Priority }
    if ($DebugPreference -gt "SilentlyContinue") { Write-Output $notice }

    switch ($PsCmdlet.ParameterSetName) {
        "UrlCallback" {
            $context = new-object Growl.Connector.CallbackContext
            $context.Data = $CallbackUrl
            $context.Type = "${NoticeType}+Url"
            Write-Verbose $CallbackUrl
            $urlCb = new-object Growl.Connector.UrlCallbackTarget
            $urlCb.Url = $CallbackUrl
            $context.SetUrlCallbackTarget($urlcb)
            $script:PowerGrowler.Notify($notice, $context)
        }
        "DataCallback" {
            $context = new-object Growl.Connector.CallbackContext
            $context.Data = $CallbackData
            $context.Type = $CallbackType
            Write-Verbose $context.GetUrlCallbackTarget()
            $script:PowerGrowler.Notify($notice, $context)
        }
        "NoCallback" {
            $script:PowerGrowler.Notify($notice)
        }
        default {
            throw "Unhandled parameter set"
        }
    }
}

Export-ModuleMember -Function Send-Growl, Set-GrowlPassword, Register-GrowlCallback, Register-GrowlType
