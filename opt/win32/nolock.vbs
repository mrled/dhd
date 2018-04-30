Dim objResult
Set objShell = WScript.CreateObject("WScript.Shell")
Do While True
    objResult = objShell.sendKeys("{NUMLOCK}{NUMLOCK}")
    WScript.Sleep(60000)
Loop
