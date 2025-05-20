local hammerSpoonEmoji = "🔨🥄"

if not hs then
  print(hammerSpoonEmoji .. " Hammerspoon not running, exiting...")
  return
end

-- reload the configs automatically if anything changes in the config dir
local function reloadConfig(files)
  local doReload = false
  for _, file in pairs(files) do
    if file:sub(-4) == ".lua" then
      doReload = true
    end
  end
  if doReload then
    hs.reload()
  end
end

-- Enabling both of these seems to break automatic reloading...
-- maybe because .hammerspoon/ is a symlink to .dhd/hbase/.hammerspoon/ ?
-- local hsWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
local dhdWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.dhd/hbase/.hammerspoon/", reloadConfig):start()

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
