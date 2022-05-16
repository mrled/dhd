local hsMouse = require("hs.mouse")
local osascript = require("hs.osascript")

local rectanglePreviewColor = '#81ecec' -- cyan
local rectangleCancelColor = '#f09d9d' -- red
local rectangleMinWidth = 300
local rectangleMinHeight = 150
local rectangleFadeDuration = 0.75
local rectanglePreview = hs.drawing.rectangle(
  hs.geometry.rect(0, 0, 0, 0)
)

local function setRectangle(newFrame)
  rectanglePreview:setStrokeWidth(2)
  rectanglePreview:setRoundedRectRadii(2, 2)
  rectanglePreview:setStroke(true):setFill(true)
  rectanglePreview:setLevel('floating')

  rectanglePreview:setFrame(newFrame)

  local color = rectanglePreviewColor
  if newFrame.w < rectangleMinWidth or newFrame.h < rectangleMinHeight then
    color = rectangleCancelColor
  end
  rectanglePreview:setStrokeColor({ hex = color, alpha = 1 })
  rectanglePreview:setFillColor({ hex = color, alpha = 0.5 })
end

local function openItermOriginal()
  local frame = rectanglePreview:frame()
  local createItermWithBounds = string.format([[
    if application "iTerm" is not running then
      activate application "iTerm"
    end if
    tell application "iTerm"
      set newWindow to (create window with default profile)
      set the bounds of newWindow to {%i, %i, %i, %i}
    end tell
    ]], frame.x, frame.y, frame.x + frame.w, frame.y + frame.h)
  hs.osascript.applescript(createItermWithBounds)
end

local function openIterm(bounds)
  hs.printf("Will open iterm...")
  local openItermTemplate = [[
    if application "iTerm" is not running then
      activate application "iTerm"
    end if
    tell application "iTerm"
      set newWindow to (create window with default profile)
      set the bounds of newWindow to {%i, %i, %i, %i}
    end tell
  ]]
  local openItermScript = string.format(openItermTemplate, bounds.xmin, bounds.ymin, bounds.xmax, bounds.ymax)
  hs.printf("Running AppleScript to open iTerm window:")
  hs.printf(openItermScript)
  local succeeded, resultObj, resultRaw = osascript.applescript(openItermScript)
  hs.printf("Opened iterm...")
end

local fromPoint = nil

local function handleDragEvent(e)
  if fromPoint == nil then
    return nil
  end
  local toPoint = hsMouse.absolutePosition()
  local newFrame = hs.geometry.new({
    ["x1"] = fromPoint.x,
    ["y1"] = fromPoint.y,
    ["x2"] = toPoint.x,
    ["y2"] = toPoint.y,
  })
  setRectangle(newFrame)

  return nil
end

local dragEvent = hs.eventtap.new(
  { hs.eventtap.event.types.mouseMoved },
  handleDragEvent
)


local function handleFlagsEvent(event)
  local flags = event:getFlags()

  if flags.ctrl and flags.shift then
    fromPoint = hsMouse.absolutePosition()
    hs.printf("Starting termdraw drag event from %s,%s", fromPoint.x, fromPoint.y)
    local newFrame = hs.geometry.rect(fromPoint.x, fromPoint.y, 0, 0)
    setRectangle(newFrame)
    rectanglePreview:setFrame(newFrame)
    dragEvent:start()
    rectanglePreview:show()

  elseif fromPoint ~= nil then
    local frame = rectanglePreview:frame()
    hs.printf("Ending termdraw drag event at %s,%s", frame.x, frame.y)
    local bounds = { xmin = frame.x, ymin = frame.y, xmax = frame.x + frame.w, ymax = frame.y + frame.h }
    local framedesc = string.format(
      "{%i, %i, %i, %i} (w: %i, h: %i)",
      frame.x, frame.y, frame.x + frame.w, frame.y + frame.h, frame.w, frame.h)

    fromPoint = nil
    dragEvent:stop()

    if frame.w < rectangleMinWidth or frame.h < rectangleMinHeight then
      -- sometimes you press the keys accidentally and invisible terms get created
      -- this prevents that
      hs.printf("Cowardly refusing to create tiny iTerm with " .. framedesc)
    else
      openIterm(bounds)
      hs.printf("Created new iTerm with " .. framedesc)
    end

    rectanglePreview:hide(rectangleFadeDuration)

  else
    fromPoint = nil
  end

  return nil
end

local flagsEvent = hs.eventtap.new(
  { hs.eventtap.event.types.flagsChanged },
  handleFlagsEvent
)
flagsEvent:start()
