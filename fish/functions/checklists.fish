function checklists
        pushd ~/Sync/Checklists
        ls | fzf | xargs cls -f 
        popd
end
