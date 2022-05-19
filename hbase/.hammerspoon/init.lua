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

-- Enabling both of these seems to break automatic reloading...
-- maybe because .hammerspoon/ is a symlink to .dhd/hbase/.hammerspoon/ ?
-- local hsWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
local dhdWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.dhd/hbase/.hammerspoon/", reloadConfig):start()

package.path = package.path .. ";" .. os.getenv("HOME") .. "/.dhd/hbase/.hammerspoon/?.lua"

hs.printf("======== config file reloaded ========")
hs.alert.show(hammerSpoonEmoji .. " Config Loaded")

animationDuration = 0

local shiftIt = require('shiftIt')


hs.loadSpoon("SpoonInstall")
spoon.SpoonInstall.use_syncinstall = true

spoon.SpoonInstall:andUse("TextClipboardHistory",
  {
    disable = false,
    config = {
      show_in_menubar = false,
    },
    hotkeys = {
      toggle_clipboard = { { "cmd", "shift" }, "v" } },
    start = true,
  }
)

spoon.SpoonInstall:andUse("KSheet",
  {
    hotkeys = {
      toggle = { { "cmd", "alt", "ctrl" }, "/" }
    } })

spoon.SpoonInstall:andUse("Seal",
  {
    hotkeys = { toggle = { { "alt" }, "space" } },
    fn = function(s)
      s:loadPlugins({ "apps", "calc", "useractions" })
      s.plugins.useractions.actions = {
        ["Hammerspoon docs webpage"] = {
          url = "http://hammerspoon.org/docs/",
          icon = hs.image.imageFromName(hs.image.systemImageNames.ApplicationIcon),
        },
      }
      s:refreshAllCommands()
    end,
    start = true,
  }
)

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
end

appModal = modalHotKey.new(
  hs.hotkey.modal.new({ "cmd", "ctrl" }, "="),
  {
    modalHotKey.shortcutKey { shortcutKey = 'l', actionDesc = "Lock screen", action = hs.caffeinate.lockScreen },
    modalHotKey.shortcutKey { shortcutKey = 'n', actionDesc = "New stig ol' bickies", action = newStigOlBickies },
    modalHotKey.shortcutKey { shortcutKey = 'r', actionDesc = "Reload Hammerspoon configuration", action = hs.reload },
  },
  hammerSpoonEmoji .. " Special functions"
)

-- TODO: Convert to better module
local termdraw = require('termdraw')
