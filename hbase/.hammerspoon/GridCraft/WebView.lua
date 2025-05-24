--[[
   A centered web view for displaying HTML content.
]]

local Util = require "GridCraft.Util"

local M = {}

--[[
  CSS for the view
]]
M.css = Util.moduleFileContents("WebView.css")

local centeredWebView = function(content, width, height)
  -- The screen with the currently focused window
  local mainScreen = hs.screen.mainScreen()

  -- A rect containing coordinates of the entire frame, including dock and menu
  local mainFrame = mainScreen:fullFrame()

  -- Coordinates to center the web view in the main frame
  local wvLeftCoord = ((mainFrame.w - mainFrame.x) / 2) - (width / 2)
  local wvTopCoord = ((mainFrame.h - mainFrame.y) / 2) - (height / 2)
  local wvRect = hs.geometry.rect(wvLeftCoord, wvTopCoord, width, height)

  local wv = hs.webview.new(wvRect)
  wv:windowStyle({ "borderless", "nonactivating" })
  wv:transparent(true)
  wv:bringToFront(true)
  wv:closeOnEscape(true)
  wv:html(content)
  return wv
end

--[[
  Generate an HTML table from a list of items
]]
local itemTableHtml = function(actionTable)
  local tableHtml = ""

  for _, keyRow in pairs(actionTable) do
    local rowHtml = "<tr>"
    for _, action in pairs(keyRow) do
      local hotkeyLabel = ""
      if action.key ~= nil then
        hotkeyLabel = string.upper(action.key)
      end
      rowHtml = rowHtml .. string.format(
        [[
          <td class="key">
            <a href="%s">
              %s
              <br/>
              <span class="hotkey">%s</span>
              <span class="description">%s</span>
            </a>
          </td>
          ]],
        action.url or "", -- href value
        action.icon,
        hotkeyLabel,      -- hotkey
        action.actionDesc -- visible description
      )
    end
    tableHtml = tableHtml .. rowHtml .. "</tr>"
  end
  return tableHtml
end


--[[
  Load Phosphor icons from a local file
]]
local getPhosphorSvg = function()
  local phosphorSvgRaw = Util.moduleFileContents("img/Phosphor.svg")
  if phosphorSvgRaw then
    -- Remove the XML declaration and DOCTYPE if present
    local phosphorSvg = phosphorSvgRaw:gsub("^<%?xml.-%?>%s*\n<!DOCTYPE.-%>\n?", "")
    -- The result should be a single giant SVG element with all icons
    return phosphorSvg
  else
    return ""
  end
end


--[[
  HTML for the entire web view
]]
local webViewHtml = function(title, itemTable)
  local webViewMenuMessageTemplate = [[
    <html>
      <head>
        <title>%s</title>
        <style>%s</style>
        %s
        <!--<script src="https://cdn.jsdelivr.net/npm/@phosphor-icons/web@2.1.2"></script>-->
        <style>
          @font-face {
            font-family: 'Phosphor';
            src: url('data:font/woff2;base64,%s') format('woff2');
          }
        </style>
      </head>
      <body>
        <table>
        %s
        </table>
      </body>
    </html>
   ]]

  return string.format(
    webViewMenuMessageTemplate,
    title,
    M.css,
    getPhosphorSvg(),
    Util.moduleFileContents("img/Phosphor.woff2"), -- Base64-encoded WOFF2 font
    itemTable
  )
end


--[[
   Create a new web view intended for modal messages.

   Works like hs.alert, but with complete control over the layout in HTML.
]]
M.webView = function(title, items, width, height)
  local itemTable = itemTableHtml(items)
  local html = webViewHtml(title, itemTable)
  local wv = centeredWebView(html, width or 1024, height or 768)
  return wv
end


return M
