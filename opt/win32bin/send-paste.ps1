## Send-Paste.ps1 (aka sprunge for Pastebin)
##############################################################################################################
## Uploads code to any pastebin.com based pastebin site and returns the url for you.
##############################################################################################################
## Usage:
##    get-content myscript.ps1 | Send-Paste "An example for you" "This is just to show how to do it"
##       would send the script with the specified title and description
##    ls *.ps1 | Send-Paste -Keep Forever
##       would flood the pastebin site with all your scripts, using filename as the title
##       and a generic description, and mark them for storing indefinitely
##    get-history -count 5 | % { $_.CommandLine } | Send-Paste
##       would paste the last 5 commands in your history!
##############################################################################################################
## History:
## v 2.0 - works with "pastebin" (including http://posh.jaykul.com/p/ and http://PowerShellCentral.com/Scripts/)
## v 1.0 - Worked with a special pastebin
##############################################################################################################
#function Send-Paste {
param(
   $Title,
   $Description="Automated paste from PowerShell console",
   $KeepFor="forever",
   $Language="posh",
#   $Author = $(Read-Host "Your name"),
   $Author = "Micah R Ledbetter",
   $url="http://pastebin.com"
)
   
BEGIN {
   $null = [Reflection.Assembly]::LoadWithPartialName("System.Web")
   [string]$data = $null;
   [string]$meta = $null;

   if($language) {
      $meta = "format=" + [System.Web.HttpUtility]::UrlEncode($language)
      # $url = $url + "?" +$lang
   } else {
      $meta = "format=text"
   }
 
   do {
      switch -regex ($KeepFor) {
         "^d.*" { $meta += "&amp;amp;expiry=d" }
         "^m.*" { $meta += "&amp;amp;expiry=m" }
         "^f.*" { $meta += "&amp;amp;expiry=f" }
         default {
            $KeepFor = Read-Host "Invalid value for 'KeepFor' parameter. Please specify 'day', 'month', or 'forever'"
         }
      }
   } until ( $meta -like "*&amp;amp;expiry*" )

   if($Description) {
      $meta += "&amp;amp;descrip=" + [System.Web.HttpUtility]::UrlEncode($Description)
   } else {
      $meta += "&amp;amp;descrip="
   }  
   $meta += "&amp;amp;poster=" + [System.Web.HttpUtility]::UrlEncode($Author)
   
   function PasteBin-Text ($meta, $title, $data) {
      $meta += "&amp;amp;paste=Send&amp;amp;posttitle=" + [System.Web.HttpUtility]::UrlEncode($Title)
      $data = $meta + "&amp;amp;code2=" + [System.Web.HttpUtility]::UrlEncode($data)
     
      # Write-Host $data -fore yellow
     
      $request = [System.Net.WebRequest]::Create($url)
      $request.ContentType = "application/x-www-form-urlencoded"
      $request.ContentLength = $data.Length
      $request.Method = "POST"

      $post = new-object IO.StreamWriter $request.GetRequestStream()
      $post.Write([char[]]$data,[int32]0,[int32]$data.Length)
      $post.Flush()
      $post.Close()

      # $reader = new-object IO.StreamReader $request.GetResponse().GetResponseStream() ##,[Text.Encoding]::UTF8
      # write-output $reader.ReadToEnd()
      # $reader.Close()
      write-output $request.GetResponse().ResponseUri.AbsoluteUri
      $request.Abort()      
   }
}

PROCESS {
   switch($_) {
      {$_ -is [System.IO.FileInfo]} {
         $Title = $_.Name
         Write-Output $_.FullName
         Write-Output $(PasteBin-Text $meta $Title $([string]::join("`n",(Get-Content $_.FullName))))
      }
      {$_ -is [String]} {
         if(!$data -and !$Title){
            $Title = Read-Host "Give us a title for your post"
         }
         $data += "`n" + $_
      }
      ## Todo, handle folders?
      default {
         Write-Error "Unable to process $_"
      }
   }
}
END {
   if($data) {
      Write-Output $(PasteBin-Text $meta $Title $data)
   }
}
#}