# I've had the best luck doing it this way - NOT doing it in a single batch script
# Sometimes one of these commands will stop further execution in a batch script, but when I 
# call cmd.exe over and over like this, that problem goes away. 
cmd.exe /c 'winrm quickconfig -q'
cmd.exe /c 'winrm quickconfig -transport:http'
cmd.exe /c 'winrm set winrm/config @{MaxTimeoutms="1800000"}'
cmd.exe /c 'winrm set winrm/config/winrs @{MaxMemoryPerShellMB="2048"}'
cmd.exe /c 'winrm set winrm/config/service @{AllowUnencrypted="true"}'
cmd.exe /c 'winrm set winrm/config/client @{AllowUnencrypted="true"}'
cmd.exe /c 'winrm set winrm/config/service/auth @{Basic="true"}'
cmd.exe /c 'winrm set winrm/config/client/auth @{Basic="true"}'
cmd.exe /c 'winrm set winrm/config/service/auth @{CredSSP="true"}'
cmd.exe /c 'winrm set winrm/config/listener?Address=*+Transport=HTTP @{Port="5985"}'
cmd.exe /c 'netsh advfirewall firewall set rule group="remote administration" new enable=yes'
cmd.exe /c 'netsh firewall add portopening TCP 5985 "Port 5985"'
cmd.exe /c 'net stop winrm'
cmd.exe /c 'sc.exe config winrm start= auto'
cmd.exe /c 'net start winrm'
