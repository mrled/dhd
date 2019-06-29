-- Adapted from https://github.com/asmagill/hammerspoon-config/blob/master/_scratch/modalSuppression.lua
-- see https://github.com/Hammerspoon/hammerspoon/issues/1505

-- save file somewhere then type `modalSuppression = dofile("modalSuppression.lua")` to load it
-- For this example, since we're trying out varying modifiers, the recognized key sequences will be printed in the console
-- start the modal state by typing `modalSuppression.start()`
-- *only* the recognized key sequences will be allowed through -- this means you can't even quit hammerspoon with Cmd-Q
-- without tapping escape first.

--- this generates the function to create the custom eventtap to suppress unwanted keys and pass through the ones we want

local eventtap = require("hs.eventtap")
local hotkey   = require("hs.hotkey")
local keycodes = require("hs.keycodes")

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
        if (hkFlags &  256) ==  256 then hkFlags, flags = hkFlags -  256, flags | eventtap.event.rawFlagMasks.command   end
        if (hkFlags &  512) ==  512 then hkFlags, flags = hkFlags -  512, flags | eventtap.event.rawFlagMasks.shift     end
        if (hkFlags & 2048) == 2048 then hkFlags, flags = hkFlags - 2048, flags | eventtap.event.rawFlagMasks.alternate end
        if (hkFlags & 4096) == 4096 then hkFlags, flags = hkFlags - 4096, flags | eventtap.event.rawFlagMasks.control   end
        if hkFlags ~= 0 then print("unexpected flag pattern detected for " .. tostring(v._hk)) end
        passThroughKeys[tonumber(kc)] = flags
    end

    return eventtap.new({
        eventtap.event.types.keyDown,
        eventtap.event.types.keyUp,
    }, function(event)
        -- check only the flags we care about and filter the rest
        local flags = event:getRawEventData().CGEventData.flags  & (
                                                  eventtap.event.rawFlagMasks.command   |
                                                  eventtap.event.rawFlagMasks.control   |
                                                  eventtap.event.rawFlagMasks.alternate |
                                                  eventtap.event.rawFlagMasks.shift
                                              )
        if passThroughKeys[event:getKeyCode()] == flags then
            hs.printf("passing:     %3d 0x%08x", event:getKeyCode(), flags)
            return false -- pass it through so hotkey can catch it
        else
            hs.printf("suppressing: %3d 0x%08x", event:getKeyCode(), flags)
            modal:exit() -- If we type the wrong key, just exit the modal - don't make us hit esc
            return true -- delete it if we got this far -- it's a key that we want suppressed
        end
    end)
end


local module = {}

-- new(): Create a new modal hotkey
-- triggerKey: A table created by invoking hs.hotkey.modal.new(), like hs.hotkey.modal.new({"cmd", "ctrl"}, "t")
-- actionTable: A table of {subKey = action} pairs
module.new = function(triggerKey, actionList)
   local modality = {}

   triggerKey.entered = function(self)
      self._eventtap = suppressKeysOtherThanOurs(self):start()
      hs.alert("Entering modal only key mode, tap escape to exit")
   end

   triggerKey.exited = function(self)
      self._eventtap:stop()
      self._eventtap = nil
      hs.alert("Exiting modal only key mode")
   end

   -- make sure there is a way out!
   triggerKey:bind({}, "escape", function() triggerKey:exit() end)
   --triggerKey:bind({"ctrl"}, "g", function() triggerKey:exit() end)

   for subKey, action in pairs(actionList) do
      triggerKey:bind({}, subKey, function()
            action()
            appHotKey:exit()
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
