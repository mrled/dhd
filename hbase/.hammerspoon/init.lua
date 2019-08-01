hammerSpoonEmoji = "🔨🥄"

-- reload the configs automatically if anything changes in the config dir
function reloadConfig(files)
   doReload = false
   for _,file in pairs(files) do
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

--[[
-- One replacement for ShiftIt
-- <https://github.com/fikovnik/ShiftIt/issues/296#issuecomment-438386501>

units = {
 right30       = { x = 0.70, y = 0.00, w = 0.30, h = 1.00 },
 right70       = { x = 0.30, y = 0.00, w = 0.70, h = 1.00 },
 left70        = { x = 0.00, y = 0.00, w = 0.70, h = 1.00 },
 left30        = { x = 0.00, y = 0.00, w = 0.30, h = 1.00 },
 top50         = { x = 0.00, y = 0.00, w = 1.00, h = 0.50 },
 bot50         = { x = 0.00, y = 0.50, w = 1.00, h = 0.50 },
 upright30     = { x = 0.70, y = 0.00, w = 0.30, h = 0.50 },
 botright30    = { x = 0.70, y = 0.50, w = 0.30, h = 0.50 },
 upleft70      = { x = 0.00, y = 0.00, w = 0.70, h = 0.50 },
 botleft70     = { x = 0.00, y = 0.50, w = 0.70, h = 0.50 },
 maximum       = { x = 0.00, y = 0.00, w = 1.00, h = 1.00 }
}

mash = { 'shift', 'ctrl', 'cmd' }
hs.hotkey.bind(mash, 'l', function() hs.window.focusedWindow():move(units.right30,    nil, true) end)
hs.hotkey.bind(mash, 'h', function() hs.window.focusedWindow():move(units.left70,     nil, true) end)
hs.hotkey.bind(mash, 'k', function() hs.window.focusedWindow():move(units.top50,      nil, true) end)
hs.hotkey.bind(mash, 'j', function() hs.window.focusedWindow():move(units.bot50,      nil, true) end)
hs.hotkey.bind(mash, ']', function() hs.window.focusedWindow():move(units.upright30,  nil, true) end)
hs.hotkey.bind(mash, '[', function() hs.window.focusedWindow():move(units.upleft70,   nil, true) end)
hs.hotkey.bind(mash, ';', function() hs.window.focusedWindow():move(units.botleft70,  nil, true) end)
hs.hotkey.bind(mash, "'", function() hs.window.focusedWindow():move(units.botright30, nil, true) end)
hs.hotkey.bind(mash, 'm', function() hs.window.focusedWindow():move(units.maximum,    nil, true) end)

]]--

---
-- Another ShiftIt replacement that is closer to the original
-- https://github.com/fikovnik/ShiftIt/issues/296#issuecomment-476189164
---

shiftItMods = { 'ctrl', 'alt', 'cmd' }
units = {
 bottom = { x = 0.0, y = 0.5, w = 1.0, h = 0.5 },
 left   = { x = 0.0, y = 0.0, w = 0.5, h = 1.0 },
 right  = { x = 0.5, y = 0.0, w = 0.5, h = 1.0 },
 top    = { x = 0.0, y = 0.0, w = 1.0, h = 0.5 },
}
function createMoveWindow(rect)
 return function ()
   hs.window.focusedWindow():move(rect, nil, true, animationDuration)
 end
end
hs.hotkey.bind(shiftItMods, 'down', createMoveWindow(units.bottom))
hs.hotkey.bind(shiftItMods, 'left', createMoveWindow(units.left))
hs.hotkey.bind(shiftItMods, 'right', createMoveWindow(units.right))
hs.hotkey.bind(shiftItMods, 'up', createMoveWindow(units.top))
hs.hotkey.bind(shiftItMods, 'm', function()
 hs.window.focusedWindow():maximize(animationDuration)
end)

---
-- Swapp, my SWitcher of APPs.
---

modalHotKey = dofile(os.getenv("HOME") .. "/.dhd/hbase/.hammerspoon/modalHotKey.lua")
appModal = modalHotKey.new(
  hs.hotkey.modal.new({"cmd", "ctrl"}, "-"),
  {
     modalHotKey.shortcutKey{ shortcutKey='e', appName='Emacs', },
     modalHotKey.shortcutKey{ shortcutKey='f', appName='Finder', },
     modalHotKey.shortcutKey{ shortcutKey='i', appName='iTerm', },
     modalHotKey.shortcutKey{ shortcutKey='j', appName='Cisco Jabber', },
     modalHotKey.shortcutKey{ shortcutKey='k', appName='Keychain Access', },
     modalHotKey.shortcutKey{ shortcutKey='n', appName='nvAlt', },
     modalHotKey.shortcutKey{ shortcutKey='o', appName='Microsoft Outlook', },
     modalHotKey.shortcutKey{ shortcutKey='s', appName='Safari', },
     modalHotKey.shortcutKey{ shortcutKey='t', appName='Terminal', },
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
  hs.hotkey.modal.new({"cmd", "ctrl"}, "="),
  {
     modalHotKey.shortcutKey{ shortcutKey='n', actionDesc="New stig ol' bickies", action=newStigOlBickies }
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

