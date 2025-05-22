--[[
  WarcraftGrid: a Warcraft 3 grid for Hammerspoon actions
]]

if not hs then
  print("Hammerspoon not running, exiting...")
  return
end
local Util = require("WarcraftGrid.Util")
local WebView = require("WarcraftGrid.WebView")


local M = {}


--[[
action(): An action to take when a shortcut key is pressed

Params:
mods: Modifier keys as could be passed to hs.hotkey.modal.new(), like {"cmd", "ctrl"} or {}
key: (string) A key to trigger the action
actionDesc: (string) A description for the action (optional if appName is passed)
appName: (string) The name of an application to switch to (mutually exclusive with action)
action: (function) An action to take

Note: if appName is passed, the action is automatically set to hs.application.launchOrFocus(app)
Note: if appName is passed and actionDesc is nil, appName will be used for actionDesc
]]
M.action = function(arg)
  local action = {}

  action.mods = arg.mods or {}
  action.key = arg.key

  if arg.empty then
    action.action = function() end
    action.actionDesc = "No action"
    -- This is a transparent PNG image, lol
    action.icon =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABAQMAAAAl21bKAAAAA1BMVEUAAACnej3aAAAAAXRSTlMAQObYZgAAAApJREFUCNdjYAAAAAIAAeIhvDMAAAAASUVORK5CYII="

    -- Or use one of the macOS built in icons (just a random one, not a good choice)
    -- action.icon = hs.image.imageFromName(hs.image.systemImageNames.InvalidDataFreestandingTemplate)
    return action
  end

  if arg.appName then
    action.appName = arg.appName
    action.action = function() hs.application.launchOrFocus(action.appName) end
  else
    action.action = arg.action
  end

  if arg.appName and not arg.actionDesc then
    action.actionDesc = arg.appName
  else
    action.actionDesc = arg.actionDesc
  end

  -- action.icon = hs.image.imageFromName(hs.image.systemImageNames.InvalidDataFreestandingTemplate):encodeAsURLString(false, "png")
  action.icon = "data:image/svg+xml;base64," .. Util.base64(Util.moduleFileContents("img/app-window.svg"))
  if arg.icon then
    action.icon = arg.icon
  elseif arg.appName then
    -- Don't use hs.application.find() -- that only works for running apps!
    -- local app = hs.application.find(arg.appName)
    local appPath = Util.findApplicationPath(arg.appName)
    if appPath then
      -- local icon = Util.getApplicationIconDataUri(arg.appName, appPath)
      local icon = hs.image.iconForFile(appPath)
      if icon then
        action.icon = icon:encodeAsURLString()
      else
        print("No icon found for " .. arg.appName)
      end
    else
      print("No app found for " .. arg.appName)
    end
  end

  return action
end


--[[
modal(): Create a new modal hotkey

Parameters:
  mods: Modifier keys as could be4 passed to hs.hotkey.modal.new(), like {"cmd", "ctrl"} or {}
  key: A key to trigger the modal hotkey as could be passed to hs.hotkey.modal.new(), like "t"
  actionTable: (table) A table of rows, each of which is a table of actions.
    e.g. to represent the left half of a qwerty keyboard, you might use:
    Map keys from the keyGrid to action tables, like:
    {
      {
        WarcraftGrid.action { key = "1", appName = "1Password" },
        WarcraftGrid.action { key = "2", appName = "Day One" },
        WarcraftGrid.action { key = "3", appName = "Photos" },
        WarcraftGrid.action { key = "4", empty = true },
        WarcraftGrid.action { key = "5", empty = true },
      },
      {
        WarcraftGrid.action { key = "q", empty = true },
        -- WarcraftGrid.action { key = "q", appName = "Messages"},
        WarcraftGrid.action { key = "w", appName = "Mattermost" },
        WarcraftGrid.action { key = "e", appName = "Visual Studio Code" },
        WarcraftGrid.action { key = "r", appName = "Bear" },
        WarcraftGrid.action { key = "t", appName = "Terminal" },
      },
    }
    Note that we are constrained to using array tables rather than key-value tobles
    so that the order is preserved.
  title: A message prefix to display when communicating to the user about this hot key
]]
M.modal = function(mods, key, actionTable, title)
  local modality = {}
  modality.triggerKey = hs.hotkey.modal.new(mods, key)
  modality.activeWebView = nil

  modality.alertStyle = {
    fillColor = {
      white = 0.45,
      alpha = 1,
    },
    strokeWidth = 10,
    fadeInDuration = 0,
    fadeOutDuration = 0,
  }

  print("modal: " .. title .. " " .. hs.inspect(mods) .. " " .. key)

  -- define explicit ways out: either press escape or the trigger key
  modality.triggerKey:bind({}, "escape", function() modality.triggerKey:exit() end)
  modality.triggerKey:bind(mods, key, function() modality.triggerKey:exit() end)

  for _, keyRow in pairs(actionTable) do
    for _, action in pairs(keyRow) do
      if action ~= nil and action.key ~= nil then
        if action.empty == true then
          -- do nothing
        else
          modality.triggerKey:bind({}, action.key, function()
            action.action()
            modality.triggerKey:exit()
          end)
        end
      end
    end
  end

  -- Create the web view here, and only show/hide it in the callback functions.
  -- This means it is rendered when new() is called, and show() displays it instantly.
  modality.activeWebView = WebView.webView(title, actionTable, 1024, 768)

  modality.triggerKey.exitWithMessage = function(self, message)
    hs.alert.show(title .. "\n\n" .. message, modality.alertStyle)
    self:exit()
  end

  modality.triggerKey.entered = function(self)
    modality.activeWebView:show()
  end

  modality.triggerKey.exited = function(self)
    modality.activeWebView:hide()
  end

  modality.start = function(self)
    self.triggerKey:enter()
  end
  modality.stop = function(self)
    self.triggerKey:exit()
  end

  return modality
end


return M
