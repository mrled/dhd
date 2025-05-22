local hammerSpoonEmoji = "🔨🥄"

if not hs then
  print(hammerSpoonEmoji .. " Hammerspoon not running, exiting...")
  return
end

-- Enabling both of these seems to break automatic reloading...
-- maybe because .hammerspoon/ is a symlink to .dhd/hbase/.hammerspoon/ ?
-- local hsWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
local dhdWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.dhd/hbase/.hammerspoon/", hs.reload):start()

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

local WarcraftGrid = require("WarcraftGrid")

local leader = WarcraftGrid.modal(
  { "ctrl" },
  "f11",
  {
    {
      WarcraftGrid.action { key = "=", empty = true },
      WarcraftGrid.action { key = "1", appName = "1Password" },
      WarcraftGrid.action { key = "2", appName = "Day One" },
      WarcraftGrid.action { key = "3", appName = "Photos" },
      WarcraftGrid.action { key = "4", appName = "Fantastical" },
      WarcraftGrid.action { key = "5", empty = true },
    },
    {
      WarcraftGrid.action { key = "`", empty = true },
      WarcraftGrid.action { key = "q", appName = "Messages" },
      WarcraftGrid.action { key = "w", appName = "Mattermost" },
      WarcraftGrid.action { key = "e", appName = "Visual Studio Code" },
      WarcraftGrid.action { key = "r", appName = "Bear" },
      WarcraftGrid.action { key = "t", appName = "Terminal" },
    },

    {
      WarcraftGrid.action { key = nil, empty = true },
      WarcraftGrid.action { key = "a", appName = "Slack" },
      WarcraftGrid.action { key = "s", appName = "Discord" },
      WarcraftGrid.action { key = "d", appName = "OmniFocus" },
      WarcraftGrid.action { key = "f", appName = "Finder" },
      WarcraftGrid.action { key = "g", appName = "ChatGPT" },
    },
    {
      WarcraftGrid.action { key = "[", appName = "Claude" },
      WarcraftGrid.action { key = "z", appName = "Mail" },
      WarcraftGrid.action { key = "x", appName = "Firefox" },
      WarcraftGrid.action { key = "c", appName = "Google Chrome" },
      WarcraftGrid.action { key = "v", appName = "Safari" },
      WarcraftGrid.action { key = "b", appName = "BBEdit" },
    },
  },
  "Leader Grid"
)
