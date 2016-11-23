--[[

     Licensed under GNU General Public License v2
      * (c) 2016, Florent E. <florent.esp@gmail.com>
--]]

local newtimer        = require("lain.helpers").newtimer
local read_pipe       = require("lain.helpers").read_pipe

local wibox           = require("wibox")

local string          = { match  = string.match,
                          format = string.format }

local setmetatable    = setmetatable

-- Brightness
-- widgets.brightness
local brightness = {}

local function worker(args)
    local args     = args or {}
    local timeout  = args.timeout or 5
    local settings = args.settings or function() end

    brightness.cmd     = args.cmd or "xbacklight"

    brightness.widget = wibox.widget.textbox('')

    function brightness.update()
        local lum = read_pipe(brightness.cmd)

        brightness_now = {}

        brightness_now.level = string.match(lum, "([%d]+)")

        if brightness_now.level == nil
        then
            brightness_now.level  = "0"
            brightness_now.status = "off"
        end

        if brightness_now.level == "0"
        then
           brightness_now.status = "off"
        else
           brightness_now.status = "on"
        end


        widget = brightness.widget
        settings()
    end

    timer_id = string.format("brightness-%s", brightness.cmd)

    newtimer(timer_id, timeout, brightness.update)

    return setmetatable(brightness, { __index = brightness.widget })
end

return setmetatable(brightness, { __call = function(_, ...) return worker(...) end })
