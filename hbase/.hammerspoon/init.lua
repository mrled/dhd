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

local shiftIt = require('shiftIt')
shiftIt.config.animationDuration = 0

---
-- Swapp, my SWitcher of APPs.
---

local modalHotKey = require("modalHotKey")
local appModal = modalHotKey.new(
  hs.hotkey.modal.new({ "cmd", "ctrl" }, "-"),
  {
    modalHotKey.shortcutKey { shortcutKey = 'b', appName = 'Boop', },
    modalHotKey.shortcutKey { shortcutKey = 'c', appName = 'Google Chrome', },
    modalHotKey.shortcutKey { shortcutKey = 'f', appName = 'Finder', },
    modalHotKey.shortcutKey { shortcutKey = 'k', appName = 'Keychain Access', },
    modalHotKey.shortcutKey { shortcutKey = 'n', appName = 'nvAlt', },
    modalHotKey.shortcutKey { shortcutKey = 's', appName = 'Safari', },
    modalHotKey.shortcutKey { shortcutKey = 't', appName = 'Terminal', },
    modalHotKey.shortcutKey { shortcutKey = 'u', appName = 'Soulver 3', },
    modalHotKey.shortcutKey { shortcutKey = 'v', appName = 'Visual Studio Code', },
    modalHotKey.shortcutKey { shortcutKey = 'w', appName = 'Brave Browser', },
    modalHotKey.shortcutKey { shortcutKey = 'x', appName = 'Firefox', },
  },
  hammerSpoonEmoji .. " Swapp"
)

appModal = modalHotKey.new(
  hs.hotkey.modal.new({ "cmd", "ctrl" }, "="),
  {
    modalHotKey.shortcutKey { shortcutKey = 'l', actionDesc = "Lock screen", action = hs.caffeinate.lockScreen },
    modalHotKey.shortcutKey { shortcutKey = 'r', actionDesc = "Reload Hammerspoon configuration", action = hs.reload },
  },
  hammerSpoonEmoji .. " Special functions"
)

-- local leader = hs.hotkey.modal.new("", "f11")
-- leader.entered = function() print 'Entered leader mode' end
-- leader.exited = function() print 'Exited leader mode' end
-- leader:bind('', 'escape', function() leader:exit() end)
-- leader:bind('', 'f11', function() leader:exit() end)

local GridCraft = require("GridCraft")

local leader = GridCraft.modal(
  { "ctrl" },
  "f11",
  {
    {
      GridCraft.action {
        key = "=",
        action = hs.caffeinate.lockScreen,
        actionDesc = "Lock screen",
        icon = GridCraft.iconPhosphor("lock", "regular")
      },
      GridCraft.action { key = "1", appName = "1Password" },
      GridCraft.action { key = "2", appName = "Day One" },
      GridCraft.action { key = "3", appName = "Photos" },
      GridCraft.action { key = "4", appName = "Fantastical" },
      GridCraft.action { key = "5", empty = true },
    },
    {
      GridCraft.action {
        key = "`",
        action = hs.reload,
        actionDesc = "hs.reload",
        icon = GridCraft.iconPhosphor("arrows-clockwise", "regular")
      },
      GridCraft.action { key = "q", appName = "Messages" },
      GridCraft.action { key = "w", appName = "Mattermost" },
      GridCraft.action { key = "e", appName = "Visual Studio Code", actionDesc = "VS Code" },
      GridCraft.action { key = "r", appName = "Bear" },
      GridCraft.action { key = "t", appName = "Terminal" },
    },

    {
      GridCraft.action { key = nil, empty = true },
      GridCraft.action { key = "a", appName = "Slack" },
      GridCraft.action { key = "s", appName = "Discord" },
      GridCraft.action { key = "d", appName = "OmniFocus" },
      GridCraft.action { key = "f", appName = "Finder" },
      GridCraft.action { key = "g", appName = "ChatGPT" },
    },
    {
      GridCraft.action { key = "[", appName = "Claude" },
      GridCraft.action { key = "z", appName = "Mail" },
      GridCraft.action { key = "x", appName = "Firefox" },
      GridCraft.action { key = "c", appName = "Google Chrome" },
      GridCraft.action { key = "v", appName = "Safari" },
      GridCraft.action { key = "b", appName = "BBEdit" },
    },
  },
  "Leader Grid"
)
