<#
.description
Move the mouse programatically to stop or delay the system's screen blanking, locking, sleep, or hibernation timeout.
.parameter delayMinutes
The number of minutes to delay. Note that this is *additive* with the system's sleep timeout. That is, if you have your machine set to lock the screen after 10 minutes, and pass delayMinutes as 5, then this function will prevent the screen from locking for 5 minutes, at which point your system's timer starts, and after 10 minutes the screen is locked. Note that the countdown is reset whenver the cursor is moved by the user.
.parameter delayForever
Delay system screen lock forever. Be careful, this could damage your screen.
.parameter intervalSeconds
Number of seconds to sleep between each cursor movement
#>

[CmdletBinding(DefaultParameterSetName="delayMinutes")] Param(
    [Parameter(ParameterSetName="delayMinutes", Mandatory=$false)] [Int] [ValidateScript({$_ -gt 0})] $delayMinutes = 20,
    [Parameter(ParameterSetName="delayForever", Mandatory=$true)] [Switch] $delayForever,
    [Int] [ValidateScript({$_ -gt 0})] $intervalSeconds = 60
)

[System.Reflection.Assembly]::LoadWithPartialName("System.Windows.Forms") | Out-Null

<#
.description
Move the cursor one pixel in all directions, to make sure the move actually happens even if the cursor starts in a corner
#>
function Move-CursorAround {
    [CmdletBinding()] Param(
        [System.Drawing.Point] $startPosition = [System.Windows.Forms.Cursor]::Position,
        [int] $delayBetween = 0,
        [int] $pixels = 1
    )
    $movePositions = @(
        New-Object System.Drawing.Point(($position.X + $pixels), $position.Y)
        New-Object System.Drawing.Point(($position.X - $pixels), $position.Y)
        New-Object System.Drawing.Point($position.X, ($position.Y + $pixels))
        New-Object System.Drawing.Point($position.X, ($position.Y - $pixels))
        $startPosition
    )
    foreach ($position in $movePositions) {
        [System.Windows.Forms.Cursor]::Position = $position
        Start-Sleep -Seconds $delayBetween
    }
}

$timer = [System.Diagnostics.Stopwatch]::StartNew()

if ($PsCmdlet.ParameterSetName -eq "delayMinutes") {
    Write-Verbose "Delaying screen lock for $delayMinutes minutes"
    $conditionalSb = {$timer.Elapsed.Minutes -lt $delayMinutes}
}
elseif ($PsCmdlet.ParameterSetName -eq "delayForever") {
    Write-Verbose "Delaying screen lock FOREVER"
    $conditionalSb = {$true}
}

$startPosition = [System.Windows.Forms.Cursor]::Position

do {
    Start-Sleep -Seconds $intervalSeconds
    if ($startPosition -ne [System.Windows.Forms.Cursor]::Position) {
        Write-Verbose "The cursor has moved after $($timer.Elapsed.Seconds) seconds; reset the timer"
        $startPosition = [System.Windows.Forms.Cursor]::Position
        $timer.Reset()
        $timer.Start()
    }
    else {
        Write-Verbose "The cursor has not moved in $($timer.Elapsed.Seconds) seconds; moving cursor"
        Move-CursorAround
    }
} while (Invoke-Command -ScriptBlock $conditionalSb)

Write-Verbose "Relinquishing cursor - screen will now lock after standard system inactivity timeout"
