<#
.SYNOPSIS
Prett-print XML
.PARAMETER Xml
A string containing XML to pretty-print
#>
function Format-XML {
    Param (
        [Parameter(ValueFromPipeline=$true, Mandatory=$true, Position=0)] [Array] $Xml
    )
    foreach ($xmlItem in $Xml) {
        $stringWriter = New-Object -TypeName IO.StringWriter
        $xmlWriter = New-Object -TypeName Xml.XmlTextWriter -ArgumentList @($stringWriter)
        $xmlWriter.Formatting = [Xml.Formatting]::Indented
        $xmlItem.WriteContentTo($xmlWriter)
        $stringWriter.ToString()
    }
}
