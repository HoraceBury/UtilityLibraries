--[[
    Stack Lib

    Creates display groups which keep their content aligned horizontally or vertically upon a refresh rate or demand.

    optionalRefreshRate: (Optional) (default: none)
        "enterFrame", "none" or milliseconds (must be >= 100)
]]

local function validateRefreshRate( refreshRate )
    if (refreshRate == nil or refreshRate == "enterFrame" or refreshRate == "none") then
        return refreshRate
    else
        local number = tonumber( refreshRate )
        if (number < 100) then
            error( "refreshRate must be >= 100", 1 )
        end
        return number
    end
end

local function startRefreshEvent( group )
    if (group.refreshRate == "enterFrame") then
        function group:enterFrame()
            group:refresh()
        end

        Runtime:addEventListener( "enterFrame", group )
    elseif (tonumber(group.refreshRate) ~= nil) then
        function group:timer()
            group:refresh()
        end

        group.refreshTimer = timer.performWithDelay( tonumber(group.refreshRate), group, 0 )
    end
end

local function cancelRefreshEvent( group )
    if (group.refreshRate == "enterFrame") then
        Runtime:removeEventListener( "enterFrame", group )
    else
        if (group.refreshTimer) then
            group.refreshTimer = timer.cancel( group.refreshTimer )
        end
    end
end

local function newStack( refreshRate )
    local group = display.newGroup()
    group.class = "stack"
    group.stacktype = "undefined"

    group.refreshRate = validateRefreshRate( refreshRate )

    group:addEventListener( "finalize", function()
        cancelRefreshEvent( group )
    end )

    return group
end

function display.newHorizontalStack( optionalRefreshRate )
    local group = newStack( optionalRefreshRate )
    group.stacktype = "horizontal"

    function group:refresh()
        local x = 0
        for i=1, group.numChildren do
            group[i].x = x
            x = x + group[i].width
        end
    end
    
    startRefreshEvent( group )

    return group
end

function display.newVerticalStack( optionalRefreshRate )
    local group = newStack( optionalRefreshRate )
    group.stacktype = "vertical"

    function group:refresh()
        local y = 0
        for i=1, group.numChildren do
            group[i].y = y
            y = y + group[i].height
        end
    end
    
    startRefreshEvent( group )

    return group
end
