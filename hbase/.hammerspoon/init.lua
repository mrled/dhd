hammerSpoonEmoji = "🔨🥄"

-- reload the configs automatically if anything changes in the config dir
function reloadConfig(files)
  doReload = false
  for _, file in pairs(files) do
    if file:sub(-4) == ".lua" then
      doReload = true
    end
  end
  if doReload then
    hs.reload()
  end
end

hsWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
dhdWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.dhd/hbase/.hammerspoon/", reloadConfig):start()

package.path = package.path .. ";" .. os.getenv("HOME") .. "/.dhd/hbase/.hammerspoon/?.lua"

hs.printf("======== config file reloaded ========")
hs.alert.show(hammerSpoonEmoji .. " Config Loaded")

animationDuration = 0

ShiftIt = require('shiftIt')

---
-- Swapp, my SWitcher of APPs.
---

modalHotKey = dofile(os.getenv("HOME") .. "/.dhd/hbase/.hammerspoon/modalHotKey.lua")
appModal = modalHotKey.new(
  hs.hotkey.modal.new({ "cmd", "ctrl" }, "-"),
  {
    modalHotKey.shortcutKey { shortcutKey = 'e', appName = 'Emacs', },
    modalHotKey.shortcutKey { shortcutKey = 'f', appName = 'Finder', },
    modalHotKey.shortcutKey { shortcutKey = 'i', appName = 'iTerm', },
    modalHotKey.shortcutKey { shortcutKey = 'j', appName = 'Cisco Jabber', },
    modalHotKey.shortcutKey { shortcutKey = 'k', appName = 'Keychain Access', },
    modalHotKey.shortcutKey { shortcutKey = 'n', appName = 'nvAlt', },
    modalHotKey.shortcutKey { shortcutKey = 'o', appName = 'Microsoft Outlook', },
    modalHotKey.shortcutKey { shortcutKey = 's', appName = 'Safari', },
    modalHotKey.shortcutKey { shortcutKey = 't', appName = 'Terminal', },
  },
  hammerSpoonEmoji .. " Swapp",
  {
  fillColor = {
    white = 0.45,
    alpha = 1,
  },
  strokeWidth = 10,
  fadeInDuration = 0,
  fadeOutDuration = 0,

}
)

function newStigOlBickies()
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
  result = hs.osascript.applescript
end

appModal = modalHotKey.new(
  hs.hotkey.modal.new({ "cmd", "ctrl" }, "="),
  {
    modalHotKey.shortcutKey { shortcutKey = 'n', actionDesc = "New stig ol' bickies", action = newStigOlBickies }
  },
  hammerSpoonEmoji .. " Special functions",
  {
  fillColor = {
    white = 0.45,
    alpha = 1,
  },
  strokeWidth = 10,
  fadeInDuration = 0,
  fadeOutDuration = 0,

}
)

-- TODO: Convert to better module
termdraw = require('termdraw')
