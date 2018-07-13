using module MrlPsParsing

<#
.SYNOPSIS
Test generating a graph of function calls in an extracted Ed-Fi ODS database deployment package
#>
[CmdletBinding()] Param(
    [string] $DotPath = "$Home\Downloads\FunctionGraph.dot",
    [string] $RootScriptPath = (
        Resolve-Path -Path "$Home\Downloads\edfidb\EdFi.RestApi.Databases.0.0.49-bps" |
            Select-Object -ExpandProperty Path
    ),
    [string] $RootScriptPsDriveLetter = "V",
    $EdgeList,
    [switch] $VeryVerbose,
    [string[]] $ExcludeSourceFilePattern = @(

        # Stuff I'll probably never need / obsolete stuff / etc
        "\\Database\\CreateDbScript\.ps1$"
        "\\DeployDatabasesToAzure\.ps1$"
        "\\PsakeTabExpansion\.ps1$"
        "\\TransformSchoolYearTypeInserts\.ps1$"
        "\\TransformXSDtoTypeInserts\.ps1$"
        "\\EntityFramework\\"
        ".*azure.*"
        "\\reset\-shared\-sandbox\.ps1$"

        # External modules I won't care about
        '\\sqlps\\'
        "\\psake\.psm1$"

        # Stuff from database projects themselves
        "\\EdFi\.Ods\.Db\.Ods\.Extension\\deploy\.ps1$"

        # Activities, which are all obsolete anyway
        "\\activities\\build\\"
        "\\activities\\deployment\\"
        "\\activities\\adhoc\-processes\\"
        "\\activities\\data\-extraction\\"
        ".*psake\.ps1$"

        # Ed-Fi Modules I can probably ignore
        "\\7z\.psm1$"
        "\\build\-utility\.ps1$"
        "\\build\-utility\.psm1$"
        "\\certificate\-management\.psm1$"
        "\\common\.ps1$"
        "\\common\.psm1$"
        "\\common\-objects\.psm1$"
        "\\config\-encryption\.psm1$"
        "\\config\-transform\.psm1$"
        "\\credential\-management\.psm1$"
        "\\deployment\.psm1$"
        "\\gac\-utility\.psm1$"
        "\\packaging\.psm1$"
        "\\path\-resolver\.psm1$"
        "\\permissions\.psm1$"
        "\\zip\.psm1$"

        # Credentials, tests, vars, etc
        ".*\.credentials\.ps1$"
        ".*\-credentials\.ps1$"
        ".*\.Tests\.ps1$"
        ".*\.vars\.ps1$"
        ".*\-vars\.ps1$"
    ),

    [Parameter(ParameterSetName='Graph', Mandatory)] [switch] $Graph,
    [Parameter(ParameterSetName='Graph')] $ImagePath = "$Home\Downloads\FunctionGraph.pdf",
    [Parameter(ParameterSetName='Graph')] $ImageFormat = "pdf",

    [Parameter(ParameterSetName='List', Mandatory)] [switch] $List,
    [Parameter(ParameterSetName='List')] $ListFilename = "$Home\Downloads\FunctionList.txt",

    [Parameter(ParameterSetName='FromRoot', Mandatory)] [switch] $FromRoot,
    [Parameter(ParameterSetName='FromRoot')] $RootScriptName = "DLPDeployment.ps1"
)

$Error.Clear()
$ErrorActionPreference = "Stop"

Import-Module -Name PSGraph
Import-Module -Name MrlPsParsing

<#
.SYNOPSIS
Generate a function call graph from its list of edges
.NOTES
We use a bidirectional graph which is according to Rafferty the correct type to use when graphing dependencies
#>
function New-FunctionCallGraph {
    [CmdletBinding()] Param(
        [Parameter(Mandatory, ValueFromPipeline=$true)]
        [FunctionCallEdge[]]
        $EdgeList
    )

    $excludeTargets = @(
        'powershell.exe'
        'psake.psm1'
    )

    $allNodes = $EdgeList |
        Foreach-Object -Process { @($_.Source, $_.Target) } |
        Select-Object -Unique

    $allFiles = $allNodes |
        Select-Object -ExpandProperty File -Unique

    $functionsByFile = @{}
    foreach ($uniqueFile in $allFiles) {
        $functionsByFile[$uniqueFile.FullName] = $allNodes |
            Where-Object -FilterScript { $_.File.FullName -eq $uniqueFile.FullName }
    }
    function LabelMaker([QualifiedCommand[]] $Commands) {
        $output = @(
            "<{0}>{1}" -f $Commands[0].FileIdentity(), $Commands[0].FileLabel()
        )
        foreach ($cmd in $Commands) {
            $output += "<{0}>{1}" -f $cmd.CommandIdentity(), $cmd.CommandLabel()
        }
        return $output -Join "|"
    }

    $graphParams = @{
        Name = "FunctionCallGraph"
        Attributes = @{
            label = "Function Call Graph"
            overlap = $False
            splines = $True
            rankdir = 'LR'
            concentrate = $True
        }
        ScriptBlock = {
            foreach ($filePath in $functionsByFile.Keys) {
                $fileInfo = Get-Item -Path $filePath
                if ($fileInfo.Name -In $excludeTargets) {
                    continue
                }
                $fileId = $functionsByFile[$filePath][0].FileIdentity()
                Node -Name $fileId -Attributes @{
                    shape = 'record'
                    label = LabelMaker -Commands $functionsByFile[$filePath]
                }
            }
            foreach ($edge in $EdgeList) {
                if ($edge.Target.File.Name -in $excludeTargets) {
                    continue
                }
                Edge -From $edge.Source.Identity() -To $edge.Target.Identity()
            }
        }
    }
    $graph = Graph @graphParams

    return $graph
}

<#
.SYNOPSIS
Select all edges relating to $RootScript from a list of edges,
including all edges representing a command called from any nested command.
.PARAMETER EdgeList
A list of all edges
.PARAMETER RootScript
The root script to use
#>
function Select-EdgesFromRootScript {
    [CmdletBinding()] Param(
        [Parameter(Mandatory)] [FunctionCallEdge[]] $EdgeList,
        [Parameter(Mandatory)] [System.IO.FileInfo] $RootScript,
        [switch] $VeryVerbose
    )

    function Write-VeryVerbose {
        [CmdletBinding()] Param(
            [string] $Message
        )
        if ($VeryVerbose) {
            Write-Host -ForegroundColor Magenta -Object $Message
        }
}

    function RecurseOverEdges {
        Param(
            [Parameter(Mandatory)] [QualifiedCommand] $RootCommand,
            [int] $RecurseDepth = 0,
            [FunctionCallEdge[]] $Edges = @(),
            [string[]] $SeenCommands = @()
        )

        $result = New-Object -TypeName PSObject -Property @{
            Edges = $Edges
            SeenCommands = $SeenCommands
        }

        if ($RootCommand.Identity() -In $result.SeenCommands) {
            Write-VeryVerbose -Message "Found '$($RootCommand.Identity())' but we have already encountered it"
            return $result
        } else {
            Write-VeryVerbose -Message "Encountered '$($RootCommand.Identity())' for the first time"
        }

        $result.SeenCommands += $RootCommand.Identity()
        $rootEdges = $EdgeList |
            Where-Object -FilterScript {
                $_.Source.Identity() -EQ $RootCommand.Identity() -And
                $_ -NotIn $result.Edges
            }
        Write-VeryVerbose -Message "Adding $($rootEdges.Count) to result's Edges property"
        $result.Edges += $rootEdges

        foreach ($edge in $rootEdges) {
            $subResult = RecurseOverEdges -RootCommand $edge.Target -Edges $result.Edges -SeenCommands $result.SeenCommands -RecurseDepth $($RecurseDepth + 1)
            $uniqEdges = Select-UniqueFunctionCallEdge -FunctionCallEdge (@($result.Edges) + @($subResult.Edges))
            # $uniqSeenCommands = Select-UniqueQualifiedCommand -QualifiedCommand (@($result.SeenCommands) + @($subResult.SeenCommands))
            $uniqSeenCommands = @($result.SeenCommands) + @($subResult.SeenCommands) | Select-Object -Unique
            Write-VeryVerbose -Message (@(
                "[Depth $RecurseDepth]"
                "[Target $($edge.Target.CommandIdentity())]"
                "[Edges $($result.Edges.Count)->$($uniqEdges.Count)]"
                "[SeenCommands $($result.SeenCommands.Count)->$($uniqSeenCommands.Count)]"
            ) -join " ")
            $result.Edges = $uniqEdges
            $result.SeenCommands = $uniqSeenCommands
        }

        Write-VeryVerbose -Message (@(
            "[Depth $RecurseDepth]"
            "[Command $($RootCommand.Identity())]"
            "[Returning $($result.Edges.Count) edges, $($result.SeenCommands.Count) commands]"
        ) -join " ")

        return $result
    }

    $rootCommand = New-Object -TypeName QualifiedCommand -ArgumentList @($RootScript, '', $true)
    $recurseResult = RecurseOverEdges -RootCommand $rootCommand
    return $recurseResult
}

if (-Not (Get-PSDrive | Where-Object -Property Name -EQ -Value $RootScriptPsDrive)) {
    $RootScriptPath = Resolve-Path -Path $RootScriptPath | Select-Object -ExpandProperty Path
    $RootScriptPath = $RootScriptPath -Replace "\\$", ""
    subst.exe "${RootScriptPsDriveLetter}:" /D
    subst.exe "${RootScriptPsDriveLetter}:" "$RootScriptPath"
    Get-PSDrive | Out-Null
}

Write-Host -ForegroundColor Green -Object "Retrieving sample Powershell file list... " -NoNewLine
$allPsUnfiltered = Get-ChildItem -Recurse -Include "*.ps1","*.psm1" -Path "${RootScriptPsDriveLetter}:"
$allPs = @()
foreach ($psFile in $allPsUnfiltered) {
    if (-Not (Test-MatchesAnyPattern -String $psFile.FullName -PatternList $ExcludeSourceFilePattern)) {
        $allPs += $psFile
    }
}
Write-Host -ForegroundColor Green -Object "Done"

if (-Not $EdgeList) {
    Write-Host -ForegroundColor Green -Object "Building function call / AST map... " -NoNewLine
    $functionAstMap = New-FunctionCallAstMap -Path $allPs
    Write-Host -ForegroundColor Green -Object "Done"

    Write-Host -ForegroundColor Green -Object "Building list of edges... " -NoNewLine
    $EdgeList = Get-FunctionCallEdgeList -FunctionAstMap $functionAstMap
    Write-Host -ForegroundColor Green -Object "Done"

    $commandList = $functionAstMap.Keys
} else {
    $commandList = Select-UniqueQualifiedCommand -QualifiedCommand @($EdgeList.Source + $EdgeList.Target)
}

switch ($PsCmdlet.ParameterSetName) {
    "Graph" {
        Write-Host -ForegroundColor Green -Object "Building graph... " -NoNewLine
        $graph = New-FunctionCallGraph -EdgeList $EdgeList
        Out-File -FilePath $DotPath -Encoding ASCII -Force -InputObject $graph
        Write-Host -ForegroundColor Green -Object "Done"

        Write-Host -ForegroundColor Green -Object "Building graph -ical representation... " -NoNewLine
        Export-PSGraph -Source $DotPath -DestinationPath $ImagePath -ShowGraph -OutputFormat $ImageFormat
        Write-Host -ForegroundColor Green -Object "Done"

        Start-Process -FilePath $ImagePath
    }
    "List" {
        $outStr = ""
        foreach ($psFile in $allPs) {
            $fileEdges = $EdgeList | Where-Object -FilterScript { $_.Target.File.FullName -eq $psFile.FullName }
            if ($fileEdges.Count -gt 0) {
                $outStr += "`r`n -  $($psFile.FullName) references $($fileEdges.Count) function edges:"
                foreach ($qualCmd in $commandList) {
                    # $fileFunctionEdges = $fileEdges | Where-Object -FilterScript { $_.Target.Command -eq $qualCmd.Command }
                    $fileFunctionEdges = $fileEdges | Where-Object -FilterScript { $_.Target.Command -eq $qualCmd.Command }
                    if ($fileFunctionEdges.Count -gt 0) {
                        $outStr += "`r`n     -  $($qualCmd.Command) is called by $($fileFunctionEdges.Count) functions:"
                        foreach ($ffe in $fileFunctionEdges) {
                            $outStr += "`r`n         -  $($ffe.Source)"
                        }
                    }
                }
            }
        }
        $global:TestOutStr = $outStr
        Out-File -FilePath $ListFilename -Encoding utf8 -Force -Width 9999 -InputObject $outStr
        Start-Process -FilePath $ListFilename
    }
    'FromRoot' {
        $sefrsParams = @{
            EdgeList = $EdgeList
            RootScript = Get-Item -Path "${RootScriptPsDriveLetter}:\$RootScriptName"
            VeryVerbose = $VeryVerbose
        }
        $edgesFromRoot = Select-EdgesFromRootScript @sefrsParams
        return $edgesFromRoot
    }
    default {
        throw "Unknown parameter set '$($PsCmdlet.ParameterSetName)'"
    }
}
