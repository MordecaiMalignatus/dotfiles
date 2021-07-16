function install-crontab
        echo "0 12 * * * fish -c 'sync-grimoire'
0 10 * * * fish -c 'upgrade'" | crontab
end
