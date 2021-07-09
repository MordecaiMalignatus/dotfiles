-- Reload config any time. 
hs.hotkey.bind({"shift", "ctrl"}, "R", function()
    hs.reload()
end)

-- Terminal automation
hs.hotkey.bind({"shift", "ctrl"}, "D", function()
    local spawnNewTerminal = [[
        tell application "Terminal"
          do script ""
          activate
        end tell
    ]]
    local success, object, description = hs.osascript.applescript(spawnNewTerminal)
    if not success then
        hs.notify.new({title="Spawning new Terminal not successful :(",
            informativeText="Error message: " .. description}):send()
    end
end)
