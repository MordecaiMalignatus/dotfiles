function toggle-wifi
        networksetup -setairportpower en0 off
        sleep 2
        networksetup -setairportpower en0 on
end
