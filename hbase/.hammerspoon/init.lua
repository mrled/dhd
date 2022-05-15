local hammerSpoonEmoji = "🔨🥄"

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

local hsWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
local dhdWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.dhd/hbase/.hammerspoon/", reloadConfig):start()

package.path = package.path .. ";" .. os.getenv("HOME") .. "/.dhd/hbase/.hammerspoon/?.lua"

hs.printf("======== config file reloaded ========")
hs.alert.show(hammerSpoonEmoji .. " Config Loaded")

animationDuration = 0

local shiftIt = require('shiftIt')

---
-- Swapp, my SWitcher of APPs.
---

local modalHotKey = dofile(os.getenv("HOME") .. "/.dhd/hbase/.hammerspoon/modalHotKey.lua")
local appModal = modalHotKey.new(
  hs.hotkey.modal.new({ "cmd", "ctrl" }, "-"),
  {
    modalHotKey.shortcutKey { shortcutKey = 'b', appName = 'Boop', },
    modalHotKey.shortcutKey { shortcutKey = 'c', appName = 'Google Chrome', },
    modalHotKey.shortcutKey { shortcutKey = 'f', appName = 'Finder', },
    modalHotKey.shortcutKey { shortcutKey = 'k', appName = 'Keychain Access', },
    modalHotKey.shortcutKey { shortcutKey = 'n', appName = 'Numi', },
    modalHotKey.shortcutKey { shortcutKey = 's', appName = 'Safari', },
    modalHotKey.shortcutKey { shortcutKey = 't', appName = 'Terminal', },
    modalHotKey.shortcutKey { shortcutKey = 'v', appName = 'Visual Studio Code', },
    modalHotKey.shortcutKey { shortcutKey = 'w', appName = 'Brave Browser', },
    modalHotKey.shortcutKey { shortcutKey = 'x', appName = 'Firefox', },
  },
  {
    fillColor = {
      white = 0.45,
      alpha = 1,
    },
    strokeWidth = 10,
    fadeInDuration = 0,
    fadeOutDuration = 0,
  },
  hammerSpoonEmoji .. " Swapp"
)

local function newStigOlBickies()
  local newSticky = string.format([[
   on run argv
       tell application "Stickies" to activate
       tell application "System Events"
       tell process "Stickies"
               tell window 1
                   delay 0.5
                   keystroke "n" using command down
               end tell
           end tell
       end tell
   end run
 ]])
  hs.osascript.applescript(newSticky)
  local result = hs.osascript.applescript
end

appModal = modalHotKey.new(
  hs.hotkey.modal.new({ "cmd", "ctrl" }, "="),
  {
    modalHotKey.shortcutKey { shortcutKey = 'n', actionDesc = "New stig ol' bickies", action = newStigOlBickies }
  },
  {
    fillColor = {
      white = 0.45,
      alpha = 1,
    },
    strokeWidth = 10,
    fadeInDuration = 0,
    fadeOutDuration = 0,
  },
  hammerSpoonEmoji .. " Special functions"
)

-- TODO: Convert to better module
local termdraw = require('termdraw')
