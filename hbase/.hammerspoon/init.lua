local hammerSpoonEmoji = "🔨🥄"

if not hs then
  print(hammerSpoonEmoji .. " Hammerspoon not running, exiting...")
  return
end

--[[
  Reload Hammerspoon configuration when files change.

  Ignore certain changes:
  - .git directories, to avoid reloading on git operations.
    Without this, any git operation in a watched directory would trigger a reload, even 'git status'.
]]
local function reloadConfig(files)
  local doReload = false
  for _, file in ipairs(files or {}) do
    if string.find(file, "/%.git/") == nil then
      doReload = true
    end
  end
  if doReload then
    hs.reload()
  end
end

-- Reloads cannot have any path components that are symlinks.
DhdWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.dhd/hbase/.hammerspoon/", hs.reload):start()
local gcWatcher = hs.pathwatcher.new("/Volumes/DataDisk/mrldata/Repositories/GridCraft", reloadConfig):start()

package.path = package.path .. ";" .. os.getenv("HOME") .. "/.dhd/hbase/.hammerspoon/?.lua"

hs.printf("======== config file reloaded ========")
hs.alert.show(hammerSpoonEmoji .. " Config Loaded")

-- Required for hs command-line tool
local function hsCliInstall()
  local hsIpc = require("hs.ipc") -- Required per hs.ipc docs
  local homeOpt = os.getenv("HOME") .. "/opt"
  hs.fs.mkdir(homeOpt)
  hs.fs.mkdir(homeOpt .. "/bin")
  hs.fs.mkdir(homeOpt .. "/share")
  hs.fs.mkdir(homeOpt .. "/share/man")
  hs.fs.mkdir(homeOpt .. "/share/man/man1")

  -- Attempt to install and hide errors
  local ok = hs.ipc.cliInstall(homeOpt, false)
  if not ok then
    -- If we got an error, uninstall first (as suggested by the docs)
    hs.ipc.cliUninstall(homeOpt, false)
    -- This time, try to install but print errors to the console
    ok = hs.ipc.cliInstall(homeOpt, false)
    if not ok then
      print("hs tool install FAILED")
      return
    end
  end
end
hsCliInstall()

local shiftIt = require('shiftIt')
shiftIt.config.animationDuration = 0

hs.loadSpoon("GridCraft")

local fiveMenu = {
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

local mainMenu = {
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
      submenu = fiveMenu
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

local leader = spoon.GridCraft.Grid.new(
  { "ctrl" },
  "f11",
  mainMenu,
  "Leader Grid"
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
