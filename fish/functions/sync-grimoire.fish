function sync-grimoire
        echo "Pushing grimoire..."
        pushd ~/grimoire
        git add . 
        git commit -m "Daily dump ["(iso-date)"]"
        git pull
        git push
        popd
end
