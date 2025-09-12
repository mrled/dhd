local f11main = {
    {
        spoon.GridCraft.Action.new { key = "q", empty = true },
        spoon.GridCraft.Action.new { key = "w", empty = true },
        spoon.GridCraft.Action.new { key = "e", empty = true },
        spoon.GridCraft.Action.new { key = "r", empty = true },
    },
    {
        spoon.GridCraft.Action.new {
            key = "a",
            handler = function()
                -- Not sure why this works with hs.execute osascript but not hs.applescript.exec
                hs.execute([[
                    osascript -e 'tell application "Finder" to make new Finder window'
                ]])
                -- hs.applescript.exec([[
                --     tell application "Finder" to make new Finder window
                -- ]])
            end,
            description = "Finder",
            icon = spoon.GridCraft.Icon.fromMacIcon("/System/Library/CoreServices/Finder.app"),
        },
        spoon.GridCraft.Action.new {
            key = "s",
            handler = function()
                hs.execute([[
                    open -na "Firefox" --args --new-window
                ]])
            end,
            description = "Firefox",
            icon = spoon.GridCraft.Icon.fromMacIcon("/Applications/Firefox.app"),
        },
        spoon.GridCraft.Action.new {
            key = "d",
            handler = function()
                hs.execute([[
                    osascript -e 'tell application "Terminal"
                        do script
                        activate
                    end tell'
                ]])
                -- hs.applescript.exec([[
                --     tell application "Terminal"
                --         do script
                --         activate
                --     end tell'
                -- ]])
            end,
            description = "Terminal",
            icon = spoon.GridCraft.Icon.fromMacIcon("/System/Applications/Utilities/Terminal.app"),
        },
        spoon.GridCraft.Action.new {
            key = "f",
            handler = function()
                hs.execute([[
                    open -na "Visual Studio Code" --args --new-window
                ]])
            end,
            description = "VS Code",
            icon = spoon.GridCraft.Icon.fromMacIcon("/Applications/Visual Studio Code.app"),
        },
    },
    {
        spoon.GridCraft.Action.new {
            key = "z",
            handler = function()
                hs.caffeinate.lockScreen()
            end,
            description = "Lock screen",
            icon = spoon.GridCraft.Icon.phosphor("lock", "regular")
        },
        spoon.GridCraft.Action.new {
            key = "x",
            handler = hs.reload,
            description = "hs.reload",
            icon = spoon.GridCraft.Icon.phosphor("arrows-clockwise", "regular")
        },
        spoon.GridCraft.Action.new {
            key = "c",
            handler = hs.openConsole,
            description = "Console",
            icon = spoon.GridCraft.Icon.phosphor("terminal-window", "regular")
        },
        spoon.GridCraft.Action.new {
            key = "v",
            handler = function()
                -- Copy the rich data of the current pasteboard to a temp pasteboard
                local richPb = hs.pasteboard.uniquePasteboard()
                hs.pasteboard.writeAllData(richPb, hs.pasteboard.readAllData(nil))

                -- Overwrite the main pasteboard with the plain text data only
                local plainData = hs.pasteboard.readString()
                hs.pasteboard.writeObjects(plainData)

                -- Wait briefly to ensure the modal is dismissed,
                -- then simulate cmd-v to paste.

                -- We have to wait briefly before simulating cmd-v,
                -- because we want to make sure the modal is dismissed first.
                hs.timer.doAfter(0.2, function()
                    hs.eventtap.keyStroke({ 'cmd' }, 'v')

                    -- Restore the original rich clipboard data
                    hs.pasteboard.writeAllData(nil, hs.pasteboard.readAllData(richPb))
                    hs.pasteboard.deletePasteboard(richPb)
                end)
            end,
            description = "Plain Paste",
            icon = spoon.GridCraft.Icon.phosphor("clipboard", "regular")
        },
    }
}

local gridKeysDisplayScreen = "mouse"

local f11config = spoon.GridCraft.Configuration.new()
f11config.animationMs = 50
f11config.displayScreen = gridKeysDisplayScreen

local f11grid = spoon.GridCraft.Grid.new(
    { "ctrl" },
    "f11",
    f11main,
    "F11 Grid",
    f11config
)

--[[
Empty submenu for copy/pasting:
  {
    {
      spoon.GridCraft.Action.new { key = "=", empty = true },
      spoon.GridCraft.Action.new { key = "1", empty = true },
      spoon.GridCraft.Action.new { key = "2", empty = true },
      spoon.GridCraft.Action.new { key = "3", empty = true },
      spoon.GridCraft.Action.new { key = "4", empty = true },
      spoon.GridCraft.Action.new { key = "5", empty = true },
    },
    {
      spoon.GridCraft.Action.new { key = "`", empty = true },
      spoon.GridCraft.Action.new { key = "q", empty = true },
      spoon.GridCraft.Action.new { key = "w", empty = true },
      spoon.GridCraft.Action.new { key = "e", empty = true },
      spoon.GridCraft.Action.new { key = "r", empty = true },
      spoon.GridCraft.Action.new { key = "t", empty = true },
    },
    {
      spoon.GridCraft.Action.new { key = nil, empty = true },
      spoon.GridCraft.Action.new { key = "a", empty = true },
      spoon.GridCraft.Action.new { key = "s", empty = true },
      spoon.GridCraft.Action.new { key = "d", empty = true },
      spoon.GridCraft.Action.new { key = "f", empty = true },
      spoon.GridCraft.Action.new { key = "g", empty = true },
    },
    {
      spoon.GridCraft.Action.new { key = "[", empty = true },
      spoon.GridCraft.Action.new { key = "z", empty = true },
      spoon.GridCraft.Action.new { key = "x", empty = true },
      spoon.GridCraft.Action.new { key = "c", empty = true },
      spoon.GridCraft.Action.new { key = "v", empty = true },
      spoon.GridCraft.Action.new { key = "b", empty = true },
    }
  }
]]


-- Load GridCraft example config files.
-- For testing the GridCraft examples.
-- dofile("/Volumes/DataDisk/mrldata/Repositories/GridCraft/site/content/docs/examples/simple-submenu/example.lua")
