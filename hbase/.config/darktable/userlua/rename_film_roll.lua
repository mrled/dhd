--[[
    rename_film_roll.lua - rename a film roll directory, keeping darktable's DB in sync

    Strategy:
      1. Move each darktable-managed image to the new directory via dt.database.move_image(),
         which updates the DB atomically per image.
      2. Shell out to move any remaining files (sidecars, non-imported files, etc.).
      3. Remove the now-empty old directory.

    Usage: select any image(s) in the film roll you want to rename, enter a new
    directory name (basename only, sibling of the current directory), click Rename Roll.
]]

local dt = require "darktable"
local df = require "lib/dtutils.file"

local MODULE_NAME = "rename_film_roll"

local rename_roll = {
    widgets = {},
    module_installed = false,
    event_registered = false,
}

local script_data = {}
script_data.metadata = {
    name = "rename film roll",
    purpose = "rename a film roll directory, moving all files and updating the darktable DB",
    author = "micahrl",
}

-- Given a film roll path and a new basename, return the new full path.
-- e.g. /photos/2024-01-vacation, "2024-01-holiday" -> /photos/2024-01-holiday
local function new_roll_path(old_path, new_basename)
    local parent = string.match(old_path, "^(.*)/[^/]+$") or old_path
    return parent .. "/" .. new_basename
end

local function shell_escape(s)
    return "'" .. s:gsub("'", "'\\''") .. "'"
end

local function do_rename_roll()
    local images = dt.gui.action_images
    if #images == 0 then
        dt.print("Please select at least one image in the film roll to rename.")
        return
    end

    local new_basename = rename_roll.widgets.new_name.text
    if not new_basename or #new_basename == 0 then
        dt.print("Please enter a new directory name.")
        return
    end
    -- Reject any path separators — we only accept a bare directory name.
    if new_basename:find("/") then
        dt.print("New name must be a directory basename, not a path.")
        return
    end

    -- Determine the film roll from the first selected image.
    local old_path = images[1].path
    -- Strip trailing slash if present.
    old_path = old_path:gsub("/$", "")
    local new_path = new_roll_path(old_path, new_basename)

    if old_path == new_path then
        dt.print("New name is the same as the current name.")
        return
    end
    if df.check_if_file_exists(new_path) then
        dt.print("Destination already exists: " .. new_path)
        return
    end

    -- Collect ALL images in this film roll, not just the selected ones.
    local roll_images = {}
    for _, img in ipairs(dt.database) do
        if img.path:gsub("/$", "") == old_path then
            table.insert(roll_images, img)
        end
    end

    dt.print("Renaming " .. old_path .. " -> " .. new_path .. " ...")

    -- Step 1: create the new film roll and move each darktable image into it.
    -- dt.films.new() requires the directory to already exist.
    local mkdir_rc = os.execute("mkdir -p " .. shell_escape(new_path))
    if mkdir_rc ~= 0 and mkdir_rc ~= true then
        dt.print("Failed to create directory: " .. new_path)
        return
    end
    local new_film = dt.films.new(new_path)
    if not new_film then
        dt.print("Failed to create film roll at " .. new_path)
        return
    end

    local moved = 0
    for _, img in ipairs(roll_images) do
        local ok, err = pcall(function()
            dt.database.move_image(new_film, img)
        end)
        if ok then
            moved = moved + 1
        else
            dt.print_error("Failed to move image " .. img.filename .. ": " .. tostring(err))
        end
    end

    -- Step 2: move any remaining files (non-darktable-managed) with the shell.
    -- At this point old_path may still contain sidecars, extra jpegs, etc.
    local move_rest_cmd = string.format(
        "find %s -maxdepth 1 -mindepth 1 -exec mv {} %s/ \\;",
        shell_escape(old_path),
        shell_escape(new_path)
    )
    local rc = os.execute(move_rest_cmd)
    if rc ~= 0 and rc ~= true then
        dt.print_error("Some files may not have moved; check " .. old_path)
    end

    -- Step 3: remove the old (now hopefully empty) directory.
    local rmdir_cmd = "rmdir " .. shell_escape(old_path)
    rc = os.execute(rmdir_cmd)
    if rc ~= 0 and rc ~= true then
        dt.print_error("Could not remove old directory (may not be empty): " .. old_path)
    end

    -- Refresh the lighttable collection so the user sees the new paths.
    local rules = dt.gui.libs.collect.filter()
    dt.gui.libs.collect.filter(rules)

    dt.print(string.format(
        "Done. Moved %d darktable image(s) to %s",
        moved, new_path
    ))
end

local function install_module()
    if rename_roll.module_installed then return end

    rename_roll.widgets.new_name = dt.new_widget("entry"){
        tooltip = "New directory basename (e.g. '2024-01-holiday'). Parent directory stays the same.",
        placeholder = "new-directory-name",
        text = "",
    }

    rename_roll.widgets.button = dt.new_widget("button"){
        label = "Rename Roll",
        clicked_callback = function(_)
            do_rename_roll()
        end,
    }

    rename_roll.widgets.label = dt.new_widget("label"){
        label = "Rename film roll directory",
        selectable = false,
        ellipsize = "middle",
        halign = "start",
    }

    dt.register_lib(
        MODULE_NAME,
        "rename film roll",
        true,
        true,
        {[dt.gui.views.lighttable] = {"DT_UI_CONTAINER_PANEL_RIGHT_CENTER", 800}},
        dt.new_widget("box"){
            orientation = "vertical",
            rename_roll.widgets.label,
            rename_roll.widgets.new_name,
            rename_roll.widgets.button,
        },
        nil,
        nil
    )
    rename_roll.module_installed = true
end

local function destroy()
    dt.gui.libs[MODULE_NAME].visible = false
end

local function restart()
    dt.gui.libs[MODULE_NAME].visible = true
end

if dt.gui.current_view().id == "lighttable" then
    install_module()
else
    if not rename_roll.event_registered then
        dt.register_event(
            MODULE_NAME, "view-changed",
            function(event, old_view, new_view)
                if new_view.name == "lighttable" and old_view.name == "darkroom" then
                    install_module()
                end
            end
        )
        rename_roll.event_registered = true
    end
end

script_data.destroy = destroy
script_data.restart = restart
script_data.destroy_method = "hide"
script_data.show = restart

return script_data
