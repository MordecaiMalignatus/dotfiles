function get-scutil-nameserver
        scutil --dns | awk '/nameserver\[/ {print $3}' | tail -n1 | pbcopy
end
