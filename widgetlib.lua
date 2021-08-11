local widget = require( "widget" )

local eventDispatcher = system.newEventDispatcher()

local lib = {}

local function unpackParams(arg, params)
    if (#arg == 1) then
        return arg[1]
    end

    local out = {}

    for i=1, #params do
        out[params[i]] = arg[i]
    end

    return out
end

--[[
    require("widgetlib").newButton{
        parent=display.currentStage,
        x=0, y=0,
        label="A Button",
        handler=function() print("button pressed") end,
        collection="allbuttons",
        default={1,1,1}, over={0,0,0},
        strokeWidth=15, -- has calculated default
        cornerRadius=25, -- has calculated default
        width=300, height=100, -- have calculated defaults
        autoToggle=true
    }
]]

function lib.newButton( ... )
    local params = { "parent", "x", "y", "label", "handler", "collection", "default", "over", "strokeWidth", "cornerRadius", "width", "height", "autoToggle" }
    params = unpackParams(arg, params)

    local _label = params.label:lower() .. math.random(1,10000)

    local group = display.newGroup()
    if (params.parent) then params.parent:insert(group) end
    group.class = "button"
    group.label = params.label

    group.isButtonOn = false

    local function switchButtonOff()
        group.isButtonOn = false
        group.btn:setFillColor(unpack(params.default))
        group.btn:setStrokeColor(unpack(params.over))
        group.toplabel.isVisible = false
    end

    local function switchButtonOn()
        group.isButtonOn = true
        group.btn:setFillColor(unpack(params.over))
        group.btn:setStrokeColor(unpack(params.default))
        group.toplabel.isVisible = true

        if (params.autoToggle) then
            switchButtonOff()
        end
    end

    function group:isActivated()
        return group.isButtonOn
    end

    function group:deactivate()
        switchButtonOff()
    end

    function group:activate()
        if (not group.isButtonOn) then
            eventDispatcher:dispatchEvent{ name="editButtonPressed", target=_label, collection=params.collection, fire=false }
        end
    end
    
    function group:editButtonPressed(event)
        if (event.collection == params.collection) then
            if (event.target == _label and not group.isButtonOn) then
                switchButtonOn()
                if (event.fire and params.handler) then
                    params.handler(params.label)
                end
            else
                switchButtonOff()
            end
        end
    end
    eventDispatcher:addEventListener( "editButtonPressed", group )

    local function handleButtonEvent(event)
        if (event.phase == "ended") then
            eventDispatcher:dispatchEvent{ name="editButtonPressed", target=_label, collection=params.collection, fire=true }
        end
        return true
    end

    group.toplabel = display.newText{
        parent = group,
        font = native.systemFont,
        text = params.label,
        x = params.x,
        y = params.y,
        fontSize = params.fontSize or 75
    }
    group.toplabel:setFillColor(unpack(params.default))
    group.toplabel.isVisible = false

    params.width = params.width or group.toplabel.width*1.25
    params.height = params.height or group.toplabel.height*1.25

    group.btn = widget.newButton(
        {
            id = _label,
            label = params.label,
            shape = "roundedRect",
            width = params.width,
            height = params.height,
            strokeWidth = params.strokeWidth or params.height*0.075,
            strokeColor = { default=params.over, over=params.default },
            cornerRadius = params.cornerRadius or params.height*.25,
            fillColor = { default=params.default, over=params.over },
            labelColor = { default=params.over, over=params.default },
            fontSize = params.fontSize or 75,
            onEvent = handleButtonEvent
        })
    group:insert(1, group.btn)
    group.btn.x, group.btn.y = params.x, params.y

    group:addEventListener( "finalize", function()
        eventDispatcher:removeEventListener( "editButtonPressed", group )
    end )

    return group
end

return lib
