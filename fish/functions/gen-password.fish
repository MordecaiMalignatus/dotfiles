function gen-password
        pwgen -c1 32 | tr -d \n | pbcopy
end
