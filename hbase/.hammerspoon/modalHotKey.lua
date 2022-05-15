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

   for i, v in ipairs(modal.keys) do
      -- parse for flags, get keycode for each
      local kc, mods = tostring(v._hk):match("keycode: (%d+), mods: (0x[^ ]+)")
      local hkFlags = tonumber(mods)
      local hkOriginal = hkFlags
      local flags = 0
      if (hkFlags & 256) == 256 then hkFlags, flags = hkFlags - 256, flags | hs.eventtap.event.rawFlagMasks.command end
      if (hkFlags & 512) == 512 then hkFlags, flags = hkFlags - 512, flags | hs.eventtap.event.rawFlagMasks.shift end
      if (hkFlags & 2048) == 2048 then hkFlags, flags = hkFlags - 2048, flags | hs.eventtap.event.rawFlagMasks.alternate end
      if (hkFlags & 4096) == 4096 then hkFlags, flags = hkFlags - 4096, flags | hs.eventtap.event.rawFlagMasks.control end
      if hkFlags ~= 0 then print("unexpected flag pattern detected for " .. tostring(v._hk)) end
      passThroughKeys[tonumber(kc)] = flags
   end

   return hs.eventtap.new({
      hs.eventtap.event.types.keyDown,
      hs.eventtap.event.types.keyUp,
   }, function(event)
      -- check only the flags we care about and filter the rest
      local flags = event:getRawEventData().CGEventData.flags & (
          hs.eventtap.event.rawFlagMasks.command |
              hs.eventtap.event.rawFlagMasks.control |
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
-- actionList: A list of module.shortcutKey tables
-- modalMessageStyle: a table to pass to hs.alert.show() for alert styling
-- modalMessagePrefix: A message prefix to display when communicating to the user about this hot key
module.new = function(triggerKey, actionList, modalMessageStyle, modalMessagePrefix)
   local modality = {}
   modality.triggerKey = triggerKey
   modality.activeAlert = nil
   modality.modalMenuMessage = modalMessagePrefix .. "\n\n"
   modality.alertStyle = modalMessageStyle

   -- define an explicit way out
   modality.triggerKey:bind({}, "escape", function() triggerKey:exit() end)

   local doubleTab = ""
   for _, action in pairs(actionList) do
      modality.modalMenuMessage = modality.modalMenuMessage .. "\n" .. string.upper(action.shortcutKey) .. ": " .. doubleTab .. action.actionDesc
      modality.triggerKey:bind({}, action.shortcutKey, function()
         action.action()
         modality.triggerKey:exit()
      end)
   end

   modality.triggerKey.exitWithMessage = function(self, message)
      hs.alert.show(modalMessagePrefix .. "\n\n" .. message, modality.alertStyle)
      self:exit()
   end

   modality.triggerKey.entered = function(self)
      self._eventtap = suppressKeysOtherThanOurs(self):start()
      modality.activeAlert = hs.alert.show(modality.modalMenuMessage, modality.alertStyle, hs.screen.mainScreen(), "forever")
   end

   modality.triggerKey.exited = function(self)
      self._eventtap:stop()
      self._eventtap = nil
      hs.alert.closeSpecific(modality.activeAlert)
   end

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


-- shortcutKey(): A key that does something in our modal context
-- arg: a table consisting of:
--- shortcutKey: (string) A single key
--- actionDesc: (string) A description for the action (optional if appName is passed)
--- appName: (string) The name of an application to switch to (mutually exclusive with action)
--- action: (function) An action to take
--- Note: if appName is passed, the action is automatically set to hs.application.launchOrFocus(app)
--- Note: if appName is passed and actionDesc is nil, appName will be used for actionDesc
module.shortcutKey = function(arg)
   local key = {}
   key.shortcutKey = arg.shortcutKey
   if arg.appName then
      key.appName = arg.appName
      key.action = function() hs.application.launchOrFocus(key.appName) end
   else
      key.action = arg.action
   end
   if arg.appName and not arg.actionDesc then
      key.actionDesc = arg.appName
   else
      key.actionDesc = arg.actionDesc
   end
   return key
end


return module
