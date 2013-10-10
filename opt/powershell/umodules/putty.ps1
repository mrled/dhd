# a hack but it is a pain to remember how to do this every time ugh. 
function Initialize-PuttyRsaKey {
    param ([switch]$NoKeygen)
 
    $sshdir = "$home\.ssh"
    $ppk = "$sshdir\id_rsa.ppk"
    $putty_pub = "$sshdir\id_rsa.ppub"
    $pub = "$sshdir\id_rsa.pub"
    $privkey = "$sshdir\id_rsa"

    write-host "NOTE: This will only work with RSA keys: `n-$privkey, `n-$putty_pub, `n-$ppk, `n-$pub"

    mkdir -f $sshdir > $null
    if (-not $NoKeygen.IsPresent) {
        if ((test-path $ppk) -or (test-path $putty_pub) -or (test-path $ppk) -or (test-path $privkey)) {
            write-error ("None of these files may exist: $ppk, $putty_pub, $pub, and $privkey .")
            return
        }
        
        write-host ("Make sure to save your key as $ppk and $putty_pub .")
        write-host ("Note that you should also export your key as an openssh key to $privkey .")
        start-process puttygen -wait
    }
    if (-not (test-path $ppk) -or -not (test-path $putty_pub) -or -not (test-path $privkey)) {
        write-error ("You must save your files as $ppk, $putty_pub, and $privkey .")
        return
    }

    $pageantlnk_path = "$startmenu\Programs\Startup\pageant.lnk"
    $pageantlnk = create-shortcut($pageantlnk_path)
    $pageantlnk.TargetPath = (get-command pageant).Definition
    $pageantlnk.Arguments = $ppk
    $pageantlnk.Save()
    & (gcm $pageantlnk_path).definition

    $newcontent = Convert-PuttyRsaPubKey($putty_pub)
    add-content -path $pub -value $newcontent
    write-host ("Your pubkey has been saved in openssh format to $pub.")
    # note: i don't echo it at the end because copy/pasting from Win terminals
    # gives you linebreaks which don't work. C/O $pub from your editor instead. 
}

# By default, putty saves pub key files with linebreaks everywhere. Convert them to openssh format. 
function Convert-PuttyRsaPubKey {
    param ([string]$ppbfile)
    $pcontent = get-content $ppbfile
    $newcontent = "ssh-rsa "
    for ($i=2; $i -lt $pcontent.count -1; $i++) {
        $newcontent += $pcontent[$i]
    }
    $comment = "$env:username@$hostname-" + $pcontent[1].split("`"")[1]
    $newcontent += " $comment"
    return $newcontent
}
# convert a key that putty uselessly put out in a bullshit format into the format expected by authorized_keys
# expects an rsa2 key. not sure what happens if this is wrong. probably won't work. 
function Convert-PuttyPublicKey {
    param(
        [parameter(mandatory=$True)]  [string]  $keyfile
    )
    $keydata = get-content $keyfile
    if (-not (($keydata[0].StartsWith("---- BEGIN")) -and ($keydata[-1].StartsWith("---- END"))) ) {
        write-error "Invalid Putty public key file"
        return
    }
    $comment = $keydata[1]
    $newcomment = $comment -replace "Comment: `"","" -replace "`"",""
    $xdata = $keydata[2..($keydata.count-2)]  # get only the key data, no comments or header shit
    $newdata = "ssh-rsa "
    foreach ($l in $xdata) { $newdata += $l } # get rid of linebreaks
    $newdata += " $newcomment"
    return $newdata
}

function uploadid {
    param(
        [parameter(mandatory=$True)]  [alias("host")]  [string]  $hostname,
        [string]  $keyfile="$home\.ssh\id_rsa.pub" 
    )
    $keydata = get-content $keyfile
    write-host "using keyfile $keyfile" -color green
    write-host "key data: $keydata" -color green

    # if its in the putty format, fix it first. 
    if ($keydata.startswith("---- BEGIN")) { 
        $keydata = convert-puttypublickey $keyfile
    }

    $akeys = "~/.ssh/authorized_keys"
    "",$keydata | plink $hostname "mkdir -p ~/.ssh && cat - >> $akeys && chmod 700 ~/.ssh && chmod 600 $akeys"
}

