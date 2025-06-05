if not hs then
  print("Hammerspoon not running, exiting...")
  return
end

--[[
  Reload Hammerspoon configuration when files change.

  Ignore certain changes:
  - .git directories, to avoid reloading on git operations.
    Without this, any git operation in a watched directory would trigger a reload, even 'git status'.
]]
local function reloadConfig(files)
  local doReload = false
  for _, file in ipairs(files or {}) do
    if string.find(file, "/%.git/") == nil then
      doReload = true
    end
  end
  if doReload then
    hs.reload()
  end
end

-- Reloads cannot have any path components that are symlinks.
-- Keep reload objects as global variables to make sure they don't get garbage collected.
DhdWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.dhd/hbase/.hammerspoon/", hs.reload):start()

-- Load host-specific configuration if it exists
HostConfig = {}
local hostname = string.lower(hs.host.localizedName())
local hostConfigFile = string.format("%s/.dhd/hosts/%s/hammerspoon.lua", os.getenv("HOME"), hostname)
if hs.fs.attributes(hostConfigFile) then
  print("Found host-specific configuration: " .. hostConfigFile)
  HostConfig = dofile(hostConfigFile)
end

-- Add path watchers for directories specified in the host configuration.
PathWatchers = {}
for _, dir in ipairs(HostConfig.watchDirs or {}) do
  if hs.fs.attributes(dir) then
    print("Adding path watcher for: " .. dir)
    local watcher = hs.pathwatcher.new(dir, reloadConfig):start()
    table.insert(PathWatchers, watcher)
  end
end

package.path = package.path .. ";" .. os.getenv("HOME") .. "/.dhd/hbase/.hammerspoon/?.lua"

hs.printf("======== config file reloaded ========")
hs.alert.show("HammerSpoon Config Loaded")

-- Required for hs command-line tool
local function hsCliInstall()
  local hsIpc = require("hs.ipc") -- Required per hs.ipc docs
  local homeOpt = os.getenv("HOME") .. "/opt"
  hs.fs.mkdir(homeOpt)
  hs.fs.mkdir(homeOpt .. "/bin")
  hs.fs.mkdir(homeOpt .. "/share")
  hs.fs.mkdir(homeOpt .. "/share/man")
  hs.fs.mkdir(homeOpt .. "/share/man/man1")

  -- Attempt to install and hide errors
  local ok = hs.ipc.cliInstall(homeOpt, false)
  if not ok then
    -- If we got an error, uninstall first (as suggested by the docs)
    hs.ipc.cliUninstall(homeOpt, false)
    -- This time, try to install but print errors to the console
    ok = hs.ipc.cliInstall(homeOpt, false)
    if not ok then
      print("hs tool install FAILED")
      return
    end
  end
end
hsCliInstall()

local shiftIt = require('shiftIt')
shiftIt.config.animationDuration = 0

-- GridCraft configuration
hs.loadSpoon("GridCraft")
local gridConfig = require("gridConfig")
