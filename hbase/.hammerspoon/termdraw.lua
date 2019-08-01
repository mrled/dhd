local rectanglePreviewColor = '#81ecec'  -- cyan

local rectanglePreview = hs.drawing.rectangle(
 hs.geometry.rect(0, 0, 0, 0)
)
rectanglePreview:setStrokeWidth(2)
rectanglePreview:setStrokeColor({ hex=rectanglePreviewColor, alpha=1 })
rectanglePreview:setFillColor({ hex=rectanglePreviewColor, alpha=0.5 })
rectanglePreview:setRoundedRectRadii(2, 2)
rectanglePreview:setStroke(true):setFill(true)
rectanglePreview:setLevel('floating')

local function setRectangle(color)
end


local function openIterm()
 local frame = rectanglePreview:frame()
 local framedesc = string.format(
    "{%i, %i, %i, %i} (w: %i, h: %i)",
    frame.x, frame.y, frame.x + frame.w, frame.y + frame.h, frame.w, frame.h)
 if frame.w < 300 or frame.h < 150 then
    -- sometimes you press the keys accidentally and invisible terms get created
    -- this prevents that
    hs.printf("Refusing to create tiny iTerm with " .. framedesc)
 else
   local createItermWithBounds = string.format([[
     if application "iTerm" is not running then
       activate application "iTerm"
     end if
     tell application "iTerm"
       set newWindow to (create window with default profile)
       set the bounds of newWindow to {%i, %i, %i, %i}
     end tell
   ]], frame.x, frame.y, frame.x + frame.w, frame.y + frame.h)
   hs.printf("Created new iTerm with " .. framedesc)
   hs.osascript.applescript(createItermWithBounds)
 end
end


local fromPoint = nil

local drag_event = hs.eventtap.new(
 { hs.eventtap.event.types.mouseMoved },
 function(e)
   toPoint = hs.mouse.getAbsolutePosition()
   local newFrame = hs.geometry.new({
     ["x1"] = fromPoint.x,
     ["y1"] = fromPoint.y,
     ["x2"] = toPoint.x,
     ["y2"] = toPoint.y,
   })
   rectanglePreview:setFrame(newFrame)

   return nil
 end
)


local flags_event = hs.eventtap.new(
 { hs.eventtap.event.types.flagsChanged },
 function(e)
   local flags = e:getFlags()
   if flags.ctrl and flags.shift then
     fromPoint = hs.mouse.getAbsolutePosition()
     local newFrame = hs.geometry.rect(fromPoint.x, fromPoint.y, 0, 0)
     rectanglePreview:setFrame(newFrame)
     drag_event:start()
     rectanglePreview:show()
   elseif fromPoint ~= nil then
     fromPoint = nil
     drag_event:stop()
     rectanglePreview:hide(1)
     openIterm()
   end
   return nil
 end
)
flags_event:start()
