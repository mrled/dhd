@{

    Alias = @(

        #### Aliases for built-in commands
        @{ Name = "getmo";  Value = "Get-Module"; }
        @{ Name = "rmmo";   Value = "Remove-Module"; }

        #### Aliases for external binaries
        @{ Name = "l"; Value = "less.exe"; }

        #### Aliases for MrlInteractive functions
        @{ Name = "..";             Value = "MrlInteractive\Set-LocationParent"; }
        @{ Name = "...";            Value = "MrlInteractive\Set-LocationGrantParent"; }
        @{ Name = "....";           Value = "MrlInteractive\Set-LocationGreatGrandParent"; }
        @{ Name = "bat";            Value = "MrlInteractive\Start-BatchFile"; }
        @{ Name = "cd";             Value = "MrlInteractive\Set-MrlLocation";                       Option = "AllScope"; }
        @{ Name = "fex";            Value = "MrlInteractive\Expand-FuckingArchive"; }
        @{ Name = "llm";            Value = "MrlInteractive\Get-ChildItemSortedLastWriteTime"; }
        @{ Name = "man";            Value = "MrlInteractive\Get-MrlHelp";                           Option = "AllScope"; }
        @{ Name = "omg";            Value = "MrlInteractive\Show-InputCommandline"; }
        @{ Name = "pwgen";          Value = "MrlInteractive\New-Password"; }
        @{ Name = "reimport";       Value = "MrlInteractive\Import-ModuleIdempotently"; }
        @{ Name = "Rename-Tab";     Value = "MrlInteractive\Set-ConEmuTabTitle"; }
        @{ Name = "syntax";         Value = "MrlInteractive\Get-Syntax"; }
        @{ Name = "touch";          Value = "MrlInteractive\Set-MrlFile"; }
        @{ Name = "vl";             Value = "MrlInteractive\Invoke-VimLessMacro"; }
        @{ Name = "wh";             Value = "MrlInteractive\Show-AllCommands"; }

        #### Aliases for my other modules
        @{ Name = "err";    Value = "MrlDebugging\Show-ErrorReport"; }
        @{ Name = "clerr";  Value = "MrlDebugging\Clear-Error"; }

    )

}