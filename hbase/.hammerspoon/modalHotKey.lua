--[[ modalHotKey.lua

Adapted from https://github.com/asmagill/hammerspoon-config/blob/master/_scratch/modalSuppression.lua
see https://github.com/Hammerspoon/hammerspoon/issues/1505

save file somewhere then type `modalHotKey = dofile("modalHotKey.lua")` to load it
Here's an example based on my usage:


appCuts = {
  e = 'Emacs',
  f = 'Finder',
  j = 'Cisco Jabber',
  k = 'Keychain Access',
  o = 'Microsoft Outlook',
  s = 'Safari',
  t = 'Terminal',
}
appActionTable = {}
for key, app in pairs(appCuts) do
  appActionTable[key] = function() hs.application.launchOrFocus(app) end
end
modalHotKey = dofile("modalHotKey.lua")
appModal = modalHotKey.new(
  hs.hotkey.modal.new({"cmd", "ctrl"}, "t"),
  appActionTable,
  appCuts,
  "Special Application Switcher"
)


*only* the recognized key sequences will be allowed through -- this means you can't even quit hammerspoon with Cmd-Q
without tapping escape first.
--]]

-- suppressKeysOtherThanOurs(): Create custom eventtap to suppress unwanted keys and pass through the ones we do want
-- modal: An object created from module.new() below (not just a modal hotkey from hs.hotkey.modal.new(); requires our extension methods)
local suppressKeysOtherThanOurs = function(modal)
    local passThroughKeys = {}

    -- this is annoying because the event's raw flag bitmasks differ from the bitmasks used by hotkey, so
    -- we have to convert here for the lookup

    for i,v in ipairs(modal.keys) do
        -- parse for flags, get keycode for each
        local kc, mods = tostring(v._hk):match("keycode: (%d+), mods: (0x[^ ]+)")
        local hkFlags = tonumber(mods)
        local hkOriginal = hkFlags
        local flags = 0
        if (hkFlags &  256) ==  256 then hkFlags, flags = hkFlags -  256, flags | hs.eventtap.event.rawFlagMasks.command   end
        if (hkFlags &  512) ==  512 then hkFlags, flags = hkFlags -  512, flags | hs.eventtap.event.rawFlagMasks.shift     end
        if (hkFlags & 2048) == 2048 then hkFlags, flags = hkFlags - 2048, flags | hs.eventtap.event.rawFlagMasks.alternate end
        if (hkFlags & 4096) == 4096 then hkFlags, flags = hkFlags - 4096, flags | hs.eventtap.event.rawFlagMasks.control   end
        if hkFlags ~= 0 then print("unexpected flag pattern detected for " .. tostring(v._hk)) end
        passThroughKeys[tonumber(kc)] = flags
    end

    return hs.eventtap.new({
        hs.eventtap.event.types.keyDown,
        hs.eventtap.event.types.keyUp,
    }, function(event)
        -- check only the flags we care about and filter the rest
        local flags = event:getRawEventData().CGEventData.flags  & (
                                                  hs.eventtap.event.rawFlagMasks.command   |
                                                  hs.eventtap.event.rawFlagMasks.control   |
                                                  hs.eventtap.event.rawFlagMasks.alternate |
                                                  hs.eventtap.event.rawFlagMasks.shift
                                              )
        if passThroughKeys[event:getKeyCode()] == flags then
            hs.printf("passing:     %3d 0x%08x", event:getKeyCode(), flags)
            return false -- pass it through so hotkey can catch it
        else
            hs.printf("suppressing: %3d 0x%08x", event:getKeyCode(), flags)
            modal:exitWithMessage("Invalid modal key " .. event:getKeyCode() .. " exiting mode")
            return true -- delete it if we got this far -- it's a key that we want suppressed
        end
    end)
end


local module = {}

-- new(): Create a new modal hotkey
-- triggerKey: A table created by invoking hs.hotkey.modal.new(), like hs.hotkey.modal.new({"cmd", "ctrl"}, "t")
-- actionTable: A table of {subKey = action} pairs
-- actionDescTable: A table of {subKey = description} pairs
-- modalMessagePrefix: A message prefix to display when communicating to the user about this hot key
module.new = function(triggerKey, actionTable, actionDescTable, modalMessagePrefix)
    local modality = {}
    modality.activeAlert = nil
    modality.alertMessage = modalMessagePrefix .. "\n========\n"

    -- Create a table of index->subKey mappings
    -- This lets us alphabetize our hotkeys for display
    alphaKeys = {}
    for subKey, _ in pairs(actionDescTable) do
        table.insert(alphaKeys, subKey)
    end
    table.sort(alphaKeys)

    -- Show a menu of allowed subKey presses
    doubleTab = "		"
    for idx, subKey in pairs(alphaKeys) do
        desc = actionDescTable[subKey]
        modality.alertMessage = modality.alertMessage .. "\n" .. subKey .. ":" .. doubleTab .. desc
    end

    triggerKey.exitWithMessage = function(self, message)
        hs.alert(modalMessagePrefix .. "\n========\n" .. message)
        self:exit()
    end

    triggerKey.entered = function(self)
        self._eventtap = suppressKeysOtherThanOurs(self):start()
        modality.activeAlert = hs.alert.show(modality.alertMessage, {}, hs.screen.mainScreen(), "forever")
    end

    triggerKey.exited = function(self)
        self._eventtap:stop()
        self._eventtap = nil
        hs.alert.closeSpecific(modality.activeAlert)
    end

    -- define an explicit way out
    triggerKey:bind({}, "escape", function() triggerKey:exit() end)

    for subKey, action in pairs(actionTable) do
        triggerKey:bind({}, subKey, function()
            action()
            triggerKey:exit()
        end)
    end

    modality.triggerKey = triggerKey

    modality.start = function(self)
       if self.triggerKey._eventtap then
           self.triggerKey:enter()
       end
    end
    modality.stop = function(self)
       if self.triggerKey._eventtap then
           self.triggerKey:exit()
       end
    end
    modality.started = function(self)
        return not not self.triggerKey._eventtap
    end

    return modality
end


return module
