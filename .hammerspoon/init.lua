-- Reload config any time.
hs.hotkey.bind({"shift", "ctrl"}, "R", function()
    hs.reload()
    hs.sound.getByName("Ping"):play()
end)

-- Terminal automation
hs.hotkey.bind({"shift", "ctrl"}, "D", function()
    local spawnNewTerminal = [[
      tell application "Ghostty"
        activate
      end tell
    ]]
    local success, object, description = hs.osascript.applescript(spawnNewTerminal)
    if not success then
        hs.notify.new({title="Spawning new Terminal not successful :(",
            informativeText="Error message: " .. description}):send()
    end
end)

-- Bike automation
hs.hotkey.bind({"shift", "ctrl"}, "B", function()
    local spawnNewTerminal = [[
      tell application "Bike"
        activate
      end tell
    ]]
    local success, object, description = hs.osascript.applescript(spawnNewTerminal)
    if not success then
        hs.notify.new({title="Activating Bike not successful :(",
            informativeText="Error message: " .. description}):send()
    end
end)


hs.hotkey.bind({"ctrl", "alt", "shift"}, "W", function()
    hs.execute("pass -c wf", true)
    hs.sound.getByName("Ping"):play()
end)
