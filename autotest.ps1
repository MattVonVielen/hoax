function Reference-Assembly($Name) {
    [void] [System.Reflection.Assembly]::LoadWithPartialName($Name)
}

Reference-Assembly System.Windows.Forms
Reference-Assembly System.Drawing

function Decode-IconGraphic {
    $encodedIcon = "
        AAABAAEAEBAAAAEAIABoBAAAFgAAACgAAAAQAAAAIAAAAAEAIAAAAAAAAAgAAAAA
        AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABwoyTDIPVYN0D16Pkg9c
        jZIKU390BTJMMgAAAAcAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACDUBhNyVx
        oLhLk8T/ZqPS/2un2f9uqNj/Y57L/0ePwv8baZu4CTxcNwAAAAIAAAAAAAAAAAAA
        AAAAAAACEkdpREqSv+1wqdX/bqfZ/3Oq2v9uqtv/dqvZ/3mr1f9yqtj/b6nZ/y58
        te0HQGJDAAAAAgAAAAAAAAAAEkVlN1GXxO1/td3/eq7Z/3yw2/+BsNn/dKze/3at
        2f9+s97/e7Lf/3ux3v9uq+D/OYK07Qk8XDcAAAAAAAAABzuDrbiHuOD/gbPb/4a2
        2P+SvNv/l8Hk/12bx/92psP/ncDX/6/Q7f+Ft9//cq3f/3Wt2v8eaZy4AAAABxQ9
        UTJ6s9f/gLPb/3+z3P9insb/XJa+/6bH3v9koc3/OH2q/ylum/9VjrH/vNbr/3mx
        4f9/teH/TZbJ/wouSDEgaZN0n8nk/4a43P9xqtX/NICx/1GQuv9PkLr/jr7k/4u8
        5P+Bstj/JG6d/4Ctxv+LvOP/gbfg/3Sw2f8PVoJzKHWjkbrX6/+LvOH/ksHk/3iw
        1v80f7H/udbt/4i75f+NveP/kL7i/02Twf9blbn/nMTo/4u+4/+Mv+T/E2GRkCp4
        ppDD3fD/msXl/5PB5P95sdj/J3eu/8je8f+OwOf/lcHl/5jD5f9locj/W5a9/7nY
        8P+ex+f/kcDj/xVjko8nbphzu9ru/6DJ6P+WwuH/m8Xi/yp8tP+AsdT/wt7y/63S
        7f9+sdb/NX+w/1mawv9Zl7//mMTm/4K43v8UW4hyGj5YMZXF4v+qzef/msTk/6XL
        6P93sdn/J3ew/0KJu/9vqM7/uNTq/0qQvv86gbH/kL/i/5jG6P9ipdH/CjVPMAAA
        AAdIj7e3zuTz/6PM6f+r0ev/oczq/5rG5/9lpdL/Y6PQ/5PA4f+dyej/jbzd/5rI
        6P+dyOf/MHqotwAAAAcAAAAAHFBsNney1e3P5PL/tNTq/6TM6v+mzOn/osrq/6jM
        5/+myub/pc7r/6fN6f+izez/VpvF7BJGYzYAAAAAAAAAAAAAAAIiV3lDeLPW7dXo
        9P+z0+z/q87q/6TM6/+mzer/q87q/6nO6v+y1O3/XqLL7RZPbkMAAAACAAAAAAAA
        AAAAAAAAAAAAAiFQcTZJkLm3lsbk/7va7v/F3vH/x+Dy/7HT6v+GvN7/QYmzthdL
        ZzYAAAACAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABxo/WjAqcppyLnqnjix4
        p44mbpdxFTtWLwAAAAcAAAAAAAAAAAAAAAAAAAAA/n8AAPAPAADgBwAAwAMAAIAB
        AACAAQAAgAEAAAAAAAAAAAAAgAEAAIABAACAAQAAwAMAAOAHAADwDwAA/n8AAA==
    "
    $iconBytes = [System.Convert]::FromBase64String($encodedIcon)
    $stream = New-Object System.IO.MemoryStream (,$iconBytes)
    $icon = New-Object System.Drawing.Icon $stream
    $stream.Close()
    return $icon
}

function Create-NotifyIcon() {
    $trayIcon = New-Object -TypeName System.Windows.Forms.NotifyIcon `
                           -Property @{ Icon = Decode-IconGraphic;
                                        Text = "No tests run yet!";
                                        Visible = $true }
    $trayIcon | Add-Member ScriptMethod Notify {
        param($NotificationType, $TitleText, $BodyText)

        $this.Text = $BodyText
        $this.BalloonTipIcon = $NotificationType
        $this.BalloonTipText = $BodyText
        $this.BalloonTipTitle = $TitleText
        $this.ShowBalloonTip(10000)
    }
    return $trayIcon
}

function Add-EventHandler($Obj, $Event, $Name) {
    $action = {
        try {
            Handle-Event $Event
        } catch {
            Write-Host "Error in event handler: " $_
        }
    }
    Register-ObjectEvent -InputObject $Obj `
                         -EventName $Event `
                         -SourceIdentifier "$Name$Event" `
                         -Action $action | Out-Null
}

function Create-FileSystemWatcher($Path) {
    $fsw = New-Object -TypeName System.IO.FileSystemWatcher `
                      -Property @{ Path = $(Get-Location).Path;
                                   NotifyFilter = 'LastWrite';
                                   IncludeSubdirectories = $true;
                                   Filter = "*.erl" }

    Add-EventHandler $fsw Changed $Path
    Add-EventHandler $fsw Created $Path
    return $fsw
}

function global:Handle-Event($Event) {
    $file = $Event.SourceEventArgs.Name

    if($file -match '^(.*\\)?(?:src|test)\\([^.].*?)(?:_tests?)?.erl$') {
        $suite = $matches[2]

        $test_module = Join-Path test "${suite}_test*.erl"
        if($matches[1]) {$test_module = Join-Path $prefix $test_module}

        $currentdate = Get-Date -Format g

        if(Test-Path $test_module) {
            $cmd = ".\rebar eunit skip_deps=true suite=$suite"
            Write-Host $cmd
            $output = Invoke-Expression $cmd
            if($LastExitCode -ne 0) {
                Write-Host -Separator "`r`n" -ForegroundColor Red $output
                $trayIcon.Notify("Error",
                                 "$suite - eunit failed",
                                 "$currentdate - eunit failed for $suite")
            } else {
                Write-Host -Separator "`r`n" -ForegroundColor Green $output
                $trayIcon.Notify("Info",
                                 "$suite - eunit passed",
                                 "$currentdate - eunit passed for $suite")
            }
        } else {
            $output = "No tests for $suite"
            Write-Host -Separator "`r`n" -ForegroundColor Yellow $output
            $trayIcon.Notify("Warning",
                             "$suite - no eunit tests",
                             "$currentdate - no eunit tests for $suite")
        }
    }
}

# Main
try {
    $trayIcon = Create-NotifyIcon
    $watcher = Create-FileSystemWatcher

    Write-Host "Watching $pwd - <Ctrl>-C to stop"
    while ($true) {
        if ([System.Console]::KeyAvailable) {
            $key = [System.Console]::ReadKey($true)
        }
    }
} finally {
    Write-Host "Cleaning up!"
    Get-EventSubscriber | Unregister-Event
    $trayIcon.Dispose()
    $watcher.Dispose()
}
