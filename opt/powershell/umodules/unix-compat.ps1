function whoami {
    $me.identity.name
}
function id {
    $output = "" + $me.identity.name + "(" + $me.identity.user.value + ")"
    $output
}

# Make output of get-command better (more like Unix) for interactive use. 
# NOTE: For aliases, the processing function calls the show function again - this is recursive!
# it's so if you have an alias chain like x->y->z->, where x and y are aliases
# and z is a function, you'll get the whole relationship + the function definition as well. 
function Display-AllCommands {
    param(
        [alias("r","a","all")] [switch]$recurse,
        [int]$recursionlevel=0,
        # weird syntax means that if the $recursionlevel isn't specified, 
        # $args[0] doesn't become $recursionlevel:
        [parameter(Position=0, ValueFromRemainingArguments=$true)] $args 
    )
    if ($args.Count -le 0) {return}
    foreach ($a in $args) {
        $level = $recursionlevel
        # This next line helps keep track if there is lots of output, but also clutters everything. Hmm. 
        #if ($level -eq 0) {write-host ($a) -foregroundcolor Green}
        if ($level -gt 20) { 
            $errstr  = "Recursion is greater than 20 levels deep. Probably a circular set of aliases? "
            write-error $errstr
            return
        }
        $levelprefix = ""
        for ($i=0; $i -le $level; $i++) {
            if ($i -eq $level) { $levelprefix += "-> " }
            else { $levelprefix += "   " }
        }

        $cmdobjs = @()
        $gcmoutput = get-command -all $a
        if ($gcmoutput.count) { $cmdobjs = $gcmoutput } #there was an array of results; use it
        else { $cmdobjs += $gcmoutput } #there was just one result; make a one-item array

        foreach ($c in $cmdobjs) {
            if ($c.CommandType) { #sometime get-command passes us an empty object! awesome!!
                switch ($c.CommandType) {
                    "Alias" {
                        write-output ($levelprefix + $c.Name + ": Aliased to " + $c.Definition) #-nonewline
                        if ($recurse.ispresent) {
                            $level = $level +1
                            Display-AllCommands $c.Definition -recurse -recursionlevel $level
                        }
                    }
                    "Application" { 
                        write-output ($levelprefix + $c.Name + ": Executable at " + $c.Definition) 
                    }
                    "Function" {
                        # TODO: don't display function definition unless I do -recurse
                        # Can I still show just the parameters though? Hmm. 
                        write-output ($levelprefix + $c.Name + ": " + $c.CommandType)
                        $defstr = $c.Definition
                        # $c.Definition is a string. 
                        # - SOMETIMES, it begins w/ a new line. if so, chomp.
                        # - SOMETIMES it ends w/ a new line too; chomp that. 
                        # - Then, add the $levelprefix to the beginning of every line 
                        #   AND to the beginning of the whole string
                        # ending with a newline (chomp that too because write-host inserts one).
                        # additionally, insert the $functionprefix at the beginning of every line
                        # AND at the beginning of the whole string
                        # I try to match both \n and \r\n because I've had it give me BOTH (lol)

                        $regex = [system.text.regularexpressions.regex]
                        $reml = [System.Text.RegularExpressions.RegexOptions]::MultiLine 
                        $re_firstnewline = new-object $regex ('\A\r?\n', $reml)
                        $re_lastnewline = new-object $regex ('\Z\r?\n', $reml)
                        $re_newline = new-object $regex ('\r?\n', $reml)
                        $re_stringbegin = new-object $regex ('\A', $reml)

                        $functionprefix = $levelprefix + "   " #indent the funct definitions a bit further
                        $defstr = $re_firstnewline.replace($defstr, '')
                        $defstr = $re_lastnewline.replace($defstr, '')
                        $defstr = $re_newline.replace($defstr, [environment]::NewLine + $functionprefix)
                        $defstr = $re_stringbegin.replace($defstr, $functionprefix)

                        write-output ($defstr) 
                    }
                    default { write-output ($levelprefix + $c.Name + ": " + $c.CommandType) }
                }
            }
        }
    }
}
set-alias wh display-allcommands

# demonstration:
# - a much-too-complex string of aliases to aliases to aliases...
# - what happens when there's more than one command for a given string.
# - recursion.
# remember that you have to dot-source this bitch
# function Setup-TestForWh {
#     set-alias ttt___ttt uuu___uuu 
#     set-alias uuu___uuu vvv___vvv
#     set-alias vvv___vvv WSManHTTPConfig #an exe file in system32 on x64 win7
#     set-alias WSManHTTPConfig xxx___xxx
#     set-alias xxx___xxx get-authenticodesignature #existing cmdlet
#     set-alias get-authenticodesignature zzz___zzz
#     set-alias zzz___zzz create-shortcut
#     function ttt___ttt { echo "functiontest" }
#     function xxx___xxx { echo "functiontest" }
#     function ttt___ttt { echo "functiontest" }
#     function ttt___ttt { echo "functiontest" }
#     function WSManHTTPConfig { echo "hurr's a function"; cmd.exe }
#     function get-authenticodesignature { echo "functest"; get-content C:\boot.ini; echo "ZUHH" }
#     set-alias aaa___aaa bbb___bbb #recursive
#     set-alias bbb___bbb aaa___aaa #recursive
# }


function head {
    param(
        [int]$n=10,
        [parameter(Position=0, ValueFromRemainingArguments=$true)] $args 
    )
    foreach ($file in $args) {
        get-content $file | select-object -first $n
    }
}
function tail {
    param(
        [int]$n=10,
        [parameter(Position=0, ValueFromRemainingArguments=$true)] $args 
    )
    foreach ($file in $args) {
        get-content $file | select-object -last $n
    }
}




# 'out-host -paging' is the Powershell pager, but it fucking sucks. it litters the screen with 
# '<SPACE> next page; <CR> next line; Q quit' when you hit enter. It doesn't calculate
# screen height properly. It's done those things since at least PS v2, and continues in v3. 
# Plus, in v3, it throws an exception when you hit 'q' to end the paging, and tells you that
# you hit 'q' to end the paging. what the fuck is that about.
# more.com is "the official workaround". I guess nobody fucking using out-host -paging. lol. 
# 
# OTOH, out-host -paging is the only way to get a pager that behaves like 'less', where it 
# displays information as it becomes available, rather than like 'more', which requires that the
# whole command finishes before it will display any information. UGH.
# 
# Old versions of less.exe from GnuWin32 (this means the one shipping with both pscx and git) 
# won't work either.
# If you're running less.exe from inside console2/conemu and GnuWin32's sh.exe, it works fine.
# If you're running less.exe from inside cmd.exe and powershell.exe or cmd.exe, it works fine.
# But if you're running it from console2/conemu and powershell.exe, it fucking crashes.
# http://sourceforge.net/projects/console/forums/forum/143117/topic/4629708
# 2 data points: v394 has the problem, v436 does NOT. 
# Recent msys (as of 2012 sometime I think) has 436.
# Edit: Ooh sweet, recent (as of 20130822) Git installs 444. 
#
# This tries to solve the problem, but doesn't work for piped data lolwut: 
# http://mow001.blogspot.com/2005/11/enhanced-more-function-for-msh.html
# 
# Lots of stuff (including some of my functions) assume that 'more' is the pager.
#
# One more thing. After installing (at least) Strawberry Perl, it changes the TERM environment
# variable to "dumb". This should be unset, or set to "MSYS", otherwise, less will report:
# "WARNING: terminal is not fully functional" every time you launch it. UGH. 
if (test-path alias:more) { del alias:more }
if (test-path function:more) { del function:more }
if (test-path alias:l) { del alias:l }
$possibless = @(
    "$home\Documents\WindowsPowerShell\Modules\PSCX\Apps\less.exe"
    "C:\opt\MinGW\msys\1.0\bin\less.exe",
    "${env:ProgramFiles(x86)}\Git\bin\less.exe",
    "$env:windir\system32\more.com"
)
foreach ($pl in $possibless) {
    if (test-path $pl) {
        set-alias less "$pl"
        set-alias l less
        set-alias more less
        set-alias m less
        break
    }
}

# this will only work with a decent less.exe as described above
# it's intended so you can do something like 
#     gci 'c:\program files' -include *.txt | lessall
# like I do with my subl() function 
# TODO: this is using PSCX's less.exe by calling 'less.exe' directly. 
#       it won't work with the one from Git. Ugh. 
function lessall {
    $files = @()
    foreach ($f in $input) { if (-not [string]::IsNullOrEmpty($f)) { $files += @("`"$f`"") } }
    foreach ($f in $args)  { if (-not [string]::IsNullOrEmpty($f)) { $files += @("`"$f`"") } }
    $allfiles = $files -join " "
    less.exe $allfiles
}

if (test-path "${env:ProgramFiles(x86)}\Git\bin\diff.exe") {
    set-alias unixdiff "${env:ProgramFiles(x86)}\Git\bin\diff.exe"
}

# by defaul, touch is aliased to set-filetime, which doesn't create new empty files. 
if (test-path alias:touch) {del alias:touch}
function touch {
    param([parameter(mandatory=$true)] $file)
    if (test-path $file) {
        set-filetime $file
    }
    else {
        new-item -ItemType file $file
    }

}


if (test-path alias:man) { del alias:man }
function man {
    foreach ($a in $args) {
        get-help $a -full | more
    }
}

if (test-path alias:cd) { del alias:cd }
# You can pipe a path to this 
# This is particularly useful for something like `mkdir asdf | cd` to mkdir/cd in the same command, nice

# Set-LocationEx from PSCX let's you move back/forward in your location stack with these:
#     Set-LocationEx -
#     Set-LocationEx +
# Calling just Set-Location displays the stack
$setloc = "set-location"
if (get-module pscx) {
    $setloc = "Set-LocationEx"
    if (test-path alias:pwd) { del alias:pwd }
    set-alias pwd $setloc
}

function cd {
    param(
        [parameter(position=0, valuefrompipeline=$true)] $location = $home
    )
    iex "$setloc `"$location`""
}

