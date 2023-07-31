function start-day
    if test -e ~/.work-machine
         cls -f "$HOME/grimoire/checklists/day-start.txt"
    else
         cls -f "$HOME/Sync/Checklists/morning.md"
    end
end
