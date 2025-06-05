local f11sub5 = {
    {
        spoon.GridCraft.Action.new { key = "=", empty = true },
        spoon.GridCraft.Action.new { key = "1", application = "Day One" },
        spoon.GridCraft.Action.new { key = "2", application = "Photos" },
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
        spoon.GridCraft.Action.new { key = "v", empty = true },
        spoon.GridCraft.Action.new { key = "b", empty = true },
    }
}

local f11main = {
    {
        spoon.GridCraft.Action.new { key = "=", application = "The Archive" },
        spoon.GridCraft.Action.new { key = "1", application = "1Password" },
        spoon.GridCraft.Action.new { key = "2", application = "Marked 2", description = "Marked" },
        spoon.GridCraft.Action.new { key = "3", empty = true },
        spoon.GridCraft.Action.new { key = "4", application = "Fantastical" },
        spoon.GridCraft.Action.new {
            key = "5",
            description = "Special",
            icon = spoon.GridCraft.Icon.phosphor("star", "regular"),
            submenu = f11sub5
        },
    },
    {
        spoon.GridCraft.Action.new { key = "`", empty = true },
        spoon.GridCraft.Action.new { key = "q", application = "Messages" },
        spoon.GridCraft.Action.new { key = "w", application = "Mattermost" },
        spoon.GridCraft.Action.new { key = "e", application = "Visual Studio Code", description = "VS Code" },
        spoon.GridCraft.Action.new { key = "r", application = "Bear" },
        spoon.GridCraft.Action.new { key = "t", application = "Terminal" },
    },

    {
        spoon.GridCraft.Action.new { key = nil, empty = true },
        spoon.GridCraft.Action.new { key = "a", application = "Slack" },
        spoon.GridCraft.Action.new { key = "s", application = "Discord" },
        spoon.GridCraft.Action.new { key = "d", application = "OmniFocus" },
        spoon.GridCraft.Action.new { key = "f", application = "Finder" },
        spoon.GridCraft.Action.new { key = "g", application = "ChatGPT" },
    },
    {
        spoon.GridCraft.Action.new { key = "[", application = "Claude" },
        spoon.GridCraft.Action.new { key = "z", application = "Mail" },
        spoon.GridCraft.Action.new { key = "x", application = "Firefox" },
        spoon.GridCraft.Action.new { key = "c", application = "Google Chrome" },
        spoon.GridCraft.Action.new { key = "v", application = "Safari" },
        spoon.GridCraft.Action.new { key = "b", application = "BBEdit" },
    },
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

local f10config = spoon.GridCraft.Configuration.new()
f10config.animationMs = 0
f10config.displayScreen = gridKeysDisplayScreen

local f10main = {
    {
        spoon.GridCraft.Action.new { key = "q", empty = true },
        spoon.GridCraft.Action.new { key = "w", empty = true },
        spoon.GridCraft.Action.new { key = "e", empty = true },
        spoon.GridCraft.Action.new { key = "r", empty = true },
        spoon.GridCraft.Action.new { key = "t", empty = true },
    },
    {
        spoon.GridCraft.Action.new { key = "a", empty = true },
        spoon.GridCraft.Action.new { key = "s", empty = true },
        spoon.GridCraft.Action.new { key = "d", empty = true },
        spoon.GridCraft.Action.new { key = "f", empty = true },

        -- A key to hide applications from the left hand
        spoon.GridCraft.Action.new {
            key = "g",
            handler = function()
                hs.eventtap.keyStroke({ "cmd" }, "h")
            end,
            description = "Hide",
            icon = spoon.GridCraft.Icon.phosphor("eye-slash", "regular")
        },
    },
    {
        spoon.GridCraft.Action.new { key = "z", empty = true },
        spoon.GridCraft.Action.new { key = "x", empty = true },
        spoon.GridCraft.Action.new { key = "c", empty = true },
        spoon.GridCraft.Action.new { key = "v", empty = true },
        spoon.GridCraft.Action.new { key = "b", empty = true },
    }
}

local f10grid = spoon.GridCraft.Grid.new(
    { "ctrl" },
    "f10",
    f10main,
    "F10 Grid",
    f10config
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
