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
local dhdWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.dhd/hbase/.hammerspoon/", hs.reload):start()
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
    spoon.GridCraft.action { key = "=", empty = true },
    spoon.GridCraft.action { key = "1", empty = true },
    spoon.GridCraft.action { key = "2", empty = true },
    spoon.GridCraft.action { key = "3", empty = true },
    spoon.GridCraft.action { key = "4", empty = true },
    spoon.GridCraft.action { key = "5", empty = true },
  },
  {
    spoon.GridCraft.action { key = "`", empty = true },
    spoon.GridCraft.action { key = "q", empty = true },
    spoon.GridCraft.action { key = "w", empty = true },
    spoon.GridCraft.action { key = "e", empty = true },
    spoon.GridCraft.action { key = "r", empty = true },
    spoon.GridCraft.action { key = "t", empty = true },
  },
  {
    spoon.GridCraft.action { key = nil, empty = true },
    spoon.GridCraft.action { key = "a", empty = true },
    -- spoon.GridCraft.action { key = "s", empty = true },
    spoon.GridCraft.action { key = "s", application = "Slack" },
    spoon.GridCraft.action { key = "d", empty = true },
    spoon.GridCraft.action { key = "f", empty = true },
    spoon.GridCraft.action { key = "g", empty = true },
  },
  {
    spoon.GridCraft.action { key = "[", empty = true },
    spoon.GridCraft.action {
      key = "z",
      handler = function()
        hs.caffeinate.lockScreen()
      end,
      description = "Lock screen",
      icon = spoon.GridCraft.iconPhosphor("lock", "regular")
    },
    spoon.GridCraft.action {
      key = "x",
      handler = hs.reload,
      description = "hs.reload",
      icon = spoon.GridCraft.iconPhosphor("arrows-clockwise", "regular")
    },
    spoon.GridCraft.action {
      key = "c",
      handler = hs.openConsole,
      description = "Console",
      icon = spoon.GridCraft.iconPhosphor("terminal-window", "regular")
    },
    spoon.GridCraft.action { key = "v", empty = true },
    spoon.GridCraft.action { key = "b", empty = true },
  }
}

local mainMenu = {
  {
    spoon.GridCraft.action {
      key = "=",
      empty = true,
    },
    spoon.GridCraft.action { key = "1", application = "1Password" },
    spoon.GridCraft.action { key = "2", application = "Day One" },
    spoon.GridCraft.action { key = "3", application = "Photos" },
    spoon.GridCraft.action { key = "4", application = "Fantastical" },
    spoon.GridCraft.action {
      key = "5",
      description = "Special",
      icon = spoon.GridCraft.iconPhosphor("star", "regular"),
      submenu = fiveMenu
    },
  },
  {
    spoon.GridCraft.action {
      key = "`",
      handler = function()
        print("ASDFASDFASDFAS"); hs.reload()
      end,
      description = "hs.reload",
      icon = spoon.GridCraft.iconPhosphor("arrows-clockwise", "regular")
    },
    spoon.GridCraft.action { key = "q", application = "Messages" },
    spoon.GridCraft.action { key = "w", application = "Mattermost" },
    spoon.GridCraft.action { key = "e", application = "Visual Studio Code", description = "VS Code" },
    spoon.GridCraft.action { key = "r", application = "Bear" },
    spoon.GridCraft.action { key = "t", application = "Terminal" },
  },

  {
    spoon.GridCraft.action { key = nil, empty = true },
    spoon.GridCraft.action { key = "a", application = "Slack" },
    spoon.GridCraft.action { key = "s", application = "Discord" },
    spoon.GridCraft.action { key = "d", application = "OmniFocus" },
    spoon.GridCraft.action { key = "f", application = "Finder" },
    spoon.GridCraft.action { key = "g", application = "ChatGPT" },
  },
  {
    spoon.GridCraft.action { key = "[", application = "Claude", icon = [[<span class="icon">💬</span>]] },
    spoon.GridCraft.action { key = "z", application = "Mail", icon = [[<span class="icon">M</span>]] },
    spoon.GridCraft.action { key = "x", application = "Firefox" },
    spoon.GridCraft.action { key = "c", application = "Google Chrome" },
    spoon.GridCraft.action { key = "v", application = "Safari" },
    spoon.GridCraft.action { key = "b", application = "BBEdit" },
  },
}

local leader = spoon.GridCraft.grid(
  { "ctrl" },
  "f11",
  mainMenu,
  "Leader Grid"
)

--[[
Empty submenu for copy/pasting:

spoon.GridCraft.action {
  key = "5",
  description = "Special",
  icon = spoon.GridCraft.iconPhosphor("star", "regular"),
  submenu = {
    {
      spoon.GridCraft.action { key = "=", empty = true },
      spoon.GridCraft.action { key = "1", empty = true },
      spoon.GridCraft.action { key = "2", empty = true },
      spoon.GridCraft.action { key = "3", empty = true },
      spoon.GridCraft.action { key = "4", empty = true },
      spoon.GridCraft.action { key = "5", empty = true },
    },
    {
      spoon.GridCraft.action { key = "`", empty = true },
      spoon.GridCraft.action { key = "q", empty = true },
      spoon.GridCraft.action { key = "w", empty = true },
      spoon.GridCraft.action { key = "e", empty = true },
      spoon.GridCraft.action { key = "r", empty = true },
      spoon.GridCraft.action { key = "t", empty = true },
    },
    {
      spoon.GridCraft.action { key = nil, empty = true },
      spoon.GridCraft.action { key = "a", empty = true },
      spoon.GridCraft.action { key = "s", empty = true },
      spoon.GridCraft.action { key = "d", empty = true },
      spoon.GridCraft.action { key = "f", empty = true },
      spoon.GridCraft.action { key = "g", empty = true },
    },
    {
      spoon.GridCraft.action { key = "[", empty = true },
      spoon.GridCraft.action { key = "z", empty = true },
      spoon.GridCraft.action { key = "x", empty = true },
      spoon.GridCraft.action { key = "c", empty = true },
      spoon.GridCraft.action { key = "v", empty = true },
      spoon.GridCraft.action { key = "b", empty = true },
    }
  }
},
]]
