function start-day
    if test -e "~/.work-machine"
         cls -f "~/grimoire/checklists/day-start.txt"
    else
         cls -f "~/Sync/Checklists/morning.md"
    end
end
