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
myWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
hs.alert.show("🔨🥄 Config Loaded")


hs.hotkey.bind({"cmd", "alt", "ctrl"}, "W", function()
  hs.alert.show("Hello World!")
end)


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

-- Another ShiftIt replacement that is closer to the original
-- https://github.com/fikovnik/ShiftIt/issues/296#issuecomment-476189164

mods = { 'ctrl', 'alt', 'cmd' }

units = {
  bottom = { x = 0.0, y = 0.5, w = 1.0, h = 0.5 },
  left   = { x = 0.0, y = 0.0, w = 0.5, h = 1.0 },
  right  = { x = 0.5, y = 0.0, w = 0.5, h = 1.0 },
  top    = { x = 0.0, y = 0.0, w = 1.0, h = 0.5 },
}

animationDuration = 0

function createMoveWindow(rect)
  return function ()
    hs.window.focusedWindow():move(rect, nil, true, animationDuration)
  end
end

hs.hotkey.bind(mods, 'down', createMoveWindow(units.bottom))
hs.hotkey.bind(mods, 'left', createMoveWindow(units.left))
hs.hotkey.bind(mods, 'right', createMoveWindow(units.right))
hs.hotkey.bind(mods, 'up', createMoveWindow(units.top))
hs.hotkey.bind(mods, 'm', function()
  hs.window.focusedWindow():maximize(animationDuration)
end)





appCuts = {
  e = 'Emacs',
  f = 'Finder',
  j = 'Cisco Jabber',
  k = 'Keychain Access',
  o = 'Microsoft Outlook',
  s = 'Safari',
  t = 'Terminal',
}

appHotKey = hs.hotkey.modal.new({"cmd", "ctrl"}, "q")
appHotKey:exit() --When reloading config, if we were in the middle of this hotkey, exit it


-- modalSuppression from https://github.com/asmagill/hammerspoon-config/blob/master/_scratch/modalSuppression.lua
--[[
modalSuppression = dofile("modalSuppression.lua")
if modalSuppression.started() then
   hs.alert("Reloading config, exiting modal suppression...")
   modalSuppression.stop()
end

function appHotKey:entered()
   hs.alert'Entered appHotKey mode'
end
function appHotKey:exited()
   modalSuppression.stop()
   hs.alert'Exited appHotKey mode'
end
--]]

--[[
-- This does cmd+x ctrl+t, appCuts key
-- Works ok except that if you don't hit an appCuts key it waits for one which is stupid
modalSuppression = dofile("modalSuppression.lua")
for key, app in pairs(appCuts) do
   -- hs.hotkey.bind({}, key, function ()
   appHotKey:bind({}, key, function()
         hs.application.launchOrFocus(app)
         appHotKey:exit()
   end)
end
--]]

--[[
-- This is an attempt to hide it inside another hotkey which might never work?
appHotKey:bind({}, 'escape', function() appHotKey:exit() end)
appHotKey:bind({"cmd", "ctrl"}, "t", function() appHotKey:exit() end)
appHotKey:bind({}, 'i', 'Select app', function()
  for key, app in pairs(appCuts) do
     hs.hotkey.bind({}, key, function ()
           hs.application.launchOrFocus(app)
          appHotKey:exit()
     end)
  end
  modalSuppression.start()
  -- modalSuppression.stop()
end)

--]]

modalHotKey = dofile("modalHotKey.lua")
appActionTable = {}
for key, app in pairs(appCuts) do
   appActionTable[key] = function() hs.application.launchOrFocus(app) end
end
appModal = modalHotKey.new(hs.hotkey.modal.new({"cmd", "ctrl"}, "t"), appActionTable)
