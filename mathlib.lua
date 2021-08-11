-- mathlib.lua

local function dump(tbl)
	print("====================")
	for k,v in pairs(tbl) do
		print(k,v)
	end
	print("====================")
end

--[[
	Maths extension library for use in Corona SDK by Matthew Webster.
	All work derived from referenced sources.
	Many of these functions are useful for trigonometry and geometry because they have developed for use within graphical user interfaces.
	Much of these are useful when building physics games
	
	twitter: @horacebury
	blog: http://springboardpillow.blogspot.co.uk/2012/04/sample-code.html
	code exchange: http://code.coronalabs.com/search/node/HoraceBury
	github: https://gist.github.com/HoraceBury
]]--
 
--[[
	References:
		http://stackoverflow.com/questions/1073336/circle-line-segment-collision-detection-algorithm
		http://mathworld.wolfram.com/Circle-LineIntersection.html
		http://stackoverflow.com/questions/385305/efficient-maths-algorithm-to-calculate-intersections
		http://stackoverflow.com/questions/4543506/algorithm-for-intersection-of-2-lines
		http://community.topcoder.com/tc?module=Static&d1=tutorials&d2=geometry2#reflection
		http://gmc.yoyogames.com/index.php?showtopic=433577
		http://local.wasp.uwa.edu.au/~pbourke/geometry/
		http://alienryderflex.com/polygon/
		http://alienryderflex.com/polygon_fill/
		http://www.amazon.com/dp/1558607323/?tag=stackoverfl08-20
		http://www.amazon.co.uk/s/ref=nb_sb_noss_1?url=search-alias%3Daps&field-keywords=Real-Time+Collision+Detection
		http://en.wikipedia.org/wiki/Line-line_intersection
		http://developer.coronalabs.com/forum/2010/11/17/math-helper-functions-distancebetween-and-anglebetween
		http://www.mathsisfun.com/algebra/vectors-dot-product.html
		http://www.mathsisfun.com/algebra/vector-calculator.html
		http://lua-users.org/wiki/PointAndComplex
		http://www.math.ntnu.no/~stacey/documents/Codea/Library/Vec3.lua
		http://www.iforce2d.net/forums/viewtopic.php?f=4&t=79&sid=b9ecd62533361594e321de04b3929d4f
		http://rosettacode.org/wiki/Dot_product#Lua
		http://chipmunk-physics.net/forum/viewtopic.php?f=1&t=2215
		http://www.fundza.com/vectors/normalize/index.html
		http://www.mathopenref.com/coordpolygonarea2.html
		http://stackoverflow.com/questions/2705542/returning-the-nearest-multiple-value-of-a-number
		http://members.tripod.com/c_carleton/dotprod.html/
		http://www.1728.org/density.htm
		http://www.wikihow.com/Find-the-Angle-Between-Two-Vectors
		http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
		http://www.euclideanspace.com/maths/algebra/vectors/angleBetween/
		http://stackoverflow.com/questions/9079853/lua-print-integer-as-a-binary
		http://www.mathopenref.com/coordintersection.html
		http://www.wikihow.com/Algebraically-Find-the-Intersection-of-Two-Lines
]]--

--[[
	Deprecated functions (see revisions for code):
		rad = convertDegreesToRadians( degrees )
		deg = convertRadiansToDegrees( radians )
		polygonFill( points, closed, perPixel, width, height, col )
]]--

--[[
	Multiplication & Fractions Functions:
		
]]--

--[[
	Point Functions:
		
]]--

--[[
	Angle Functions:
		
]]--

--[[
	Line Functions:
		
]]--

--[[
	Polygon Functions:
		
]]--

--[[
	Point Functions:
		
]]--

--[[
	Fractions
]]--

-- returns the average of all parameters passed in
function math.avg( ... )
	return math.sum( unpack( arg ) ) / #arg
end

-- returns the sum total of all parameters passed in
function math.sum( ... )
	local total = 0
	for i=1, #arg do
		total = total + arg[i]
	end
	return total
end

-- Rounds to the nearest multiple of the number.
function math.nearest( number, multiple )
	return math.round( number / multiple ) * multiple
end

-- Rounds down to the nearest multiple of the number.
function math.nearestLow( number, multiple )
	return math.floor( number / multiple ) * multiple
end

-- Rounds up to the nearest multiple of the number.
function math.nearestHigh( number, multiple )
	return math.ceil( number / multiple ) * multiple
end

-- Return the column or row index
function math.getColIndex( x, colSize )
	return math.floor( x / colSize ) + 1
end

--[[
	Rounds all numbers up to the next nearest multiple of the first parameter.
]]--
local function roundUpTo( upto, ... )
	local t = {}
	
	for i=1, #arg do
		t[i] = arg[i] + (upto - arg[i] % upto)
	end
	
	return unpack(t)
end
math.roundUpTo = roundUpTo

local function test_Rounding()
	for i=0, 20 do
		print(i,math.nearest(i,4),math.roundUpTo(4,i))
	end
end
--test_Rounding()

-- Returns b represented as a fraction of a.
-- Eg: If a is 1000 and b is 900 the returned value is 0.9
-- Often the returned value would be used in a multiplication of another value, usually a distance value.
local function fractionOf( a, b )
        return b / a
end
math.fractionOf = fractionOf

-- Returns b represented as a percentage of a.
-- Eg: If a is 1000 and b is 900 the returned value is 90
-- Use: This is useful in determining how far something should be moved to complete a certain distance.
-- Often the returned value would be used in a division of another value, usually a distance value.
local function percentageOf( a, b )
        return fractionOf(a, b) * 100
end
math.percentageOf = percentageOf

-- return a value clamped between a range
local function clamp( val, low, high )
	if (val < low) then return low end
	if (val > high) then return high end
	return val
end
math.clamp = clamp

--[[
	Returns the time to use in a transition when setting a transition.to
	
	Parameters:
		totalTime: Time spent transitioning from 0 to the full value
		fullValue: The target value to transition towards
		currentValue: The partial value. 0 if no transition has occurred yet, otherwise the incomplete intermediate value
	
	Returns:
		The value used to set the 'time' property of a transition.to call.
]]--
local function transitionTime( totalTime, fullValue, currentValue )
	return totalTime - (totalTime /( fullValue/currentValue ))
end
math.transitionTime = transitionTime

--[[
	Angles
]]--

-- rotates point around the centre by degrees
-- rounds the returned coordinates using math.round() if round == true
-- returns new coordinates object
local function rotateAboutPoint( point, degrees, centre )
	local pt = { x=point.x - centre.x, y=point.y - centre.y }
	pt = math.rotateTo( pt, degrees )
	pt.x, pt.y = pt.x + centre.x, pt.y + centre.y
	return pt
end
math.rotateAboutPoint = rotateAboutPoint

-- rotates a point around the (0,0) point by degrees
-- returns new point object
-- center: optional
local function rotateTo( point, degrees, center )
	if (center ~= nil) then
		return rotateAboutPoint( point, degrees, center )
	else
		local x, y = point.x, point.y

		local theta = math.rad( degrees )

		local pt = {
			x = x * math.cos(theta) - y * math.sin(theta),
			y = x * math.sin(theta) + y * math.cos(theta)
		}
		
		return pt
	end
end
math.rotateTo = rotateTo

local function rotate( x, y, angleInDegrees )
	local angleInRadians = math.rad( angleInDegrees )
	local cosRadians, sinRadians = math.cos( angleInRadians ), math.sin( angleInRadians )
	return x * cosRadians - y * sinRadians, x * sinRadians + y * cosRadians
end
math.rotate = rotate

--[[
	Rotates a point clockwise or anticlockwise 90 degrees only.
	Fast implementation.
	
	Parameters option 1:
		xCenter: X of point to rotate around - optional
		yCenter: Y of point to rotate around - optional
		xPoint: X of point to rotate
		yPoint: Y of point to rotate
		angle: Amount to rotate -90 or false, 90 or true - optional: default is 90
	
	Parameters option 2:
		{x,y}: Point to rotate around - optional
		{x,y}: Point to rotate
		angle: Amount to rotate -90 or false, 90 or true - optional: default is 90
	
	Returns - Parameters option 1:
		xPoint: X of rotated point
		yPoint: Y of rotated point
	
	Returns - Parameters option 2:
		{x,y}: Rotated point
]]--
local function rotateBy90( ... )
--print(arg[1],arg[2])
	local xCenter, yCenter = 0, 0
	local xPt, yPt = 0, 0
	local angle = 90
	
	if (type(arg[1]) == "table") then
		if (#arg == 1) then
			xPt, yPt = arg[1].x, arg[1].y
		elseif (#arg == 2 and type(arg[2]) == "table") then
			xCenter, yCenter = arg[1].x, arg[1].y
			xPt, yPt = arg[2].x, arg[2].y
		elseif (#arg == 2 and type(arg[2]) == "number") then
			xPt, yPt = arg[1].x, arg[1].y
			angle = arg[2]
		else
			xCenter, yCenter = arg[1].x, arg[1].y
			xPt, yPt = arg[2].x, arg[2].y
			angle = arg[3]
		end
	else
		if (#arg == 2) then
			xPt, yPt = unpack( arg )
		elseif (#arg == 3) then
			xPt, yPt, angle = unpack( arg )
		elseif (#arg == 4) then
			xCenter, yCenter, xPt, yPt = unpack( arg )
		else
			xCenter, yCenter, xPt, yPt, angle = unpack( arg )
		end
	end
	
	if (type(angle) == "boolean") then
		if (angle) then
			angle = 90
		else
			angle = -90
		end
	end
	
	local dx, dy = xPt-xCenter, yPt-yCenter
	
	if (angle == -90) then
		xPt, yPt = dy, -dx
	else
		xPt, yPt = -dy, dx
	end
	
	if (type(arg[1]) == "table") then
		return {x=xCenter+xPt,y=yCenter+yPt}
	else
		return xCenter+xPt, yCenter+yPt
	end
end
math.rotateBy90 = rotateBy90

local function test_rotateBy90()
	local center = display.newCircle( display.actualCenterX, display.actualCenterY, 25 )
	local rotated = display.newCircle( display.actualCenterX, display.actualCenterY-250, 25 )
	local rotatedforward = display.newCircle( display.actualCenterX, display.actualCenterY-250, 25 )
	local dot = display.newCircle( display.actualCenterX, display.actualCenterY-250, 25 )
	
	dot.fill = {0,0,1}
	rotated.fill = {1,0,0}
	rotatedforward.fill = {0,1,0}
	
	dot:addEventListener( "touch", function(e)
		if (e.phase == "began") then
			display.currentStage:setFocus( e.target )
		elseif (e.phase == "ended") then
			display.currentStage:setFocus( nil )
			return false
		end
		
		dot.x, dot.y = e.x, e.y
		
		local ptB = math.rotateBy90( center, dot, true )
		local ptF = math.rotateBy90( center, dot, -90 )
		
		rotated.x, rotated.y = ptB.x, ptB.y
		rotatedforward.x, rotatedforward.y = ptF.x, ptF.y
		
		return true
	end )
end
--test_rotateBy90()

--[[ Support values for angles ]]--
local PI = (4*math.atan(1))
local quickPI = 180 / PI
math.PI, math.quickPI = PI, quickPI

--[[
	Converts an integer to a string of bits, least significant first.
	
	Ref: http://stackoverflow.com/questions/9079853/lua-print-integer-as-a-binary
]]--
function math.toBits(num)
    local t={} -- will contain the bits
    while num>0 do
        rest=math.fmod(num,2)
        t[#t+1]=rest
        num=(num-rest)/2
    end
    return table.concat(t)
end

--[[
	Returns the angle.
	
	Params:
		a : Returns the angle of the point at a relative to (0,0) (east is the virtual base)
		a, b Params: Returns the angle of b relative to a
		a, b, c Params: Returns the angle found at a for between b and c
]]--
local function angleOf( ... )
	local a, b, c, d = arg[1], arg[2], arg[3], arg[4]
	
	if (#arg == 1 and arg[1].xStart == nil) then
		-- angle of a relative to (0,0)
		return math.atan2( a.y, a.x ) * quickPI -- 180 / PI -- math.pi
	elseif (#arg == 1 and arg[1].xStart ~= nil) then
		-- angle of x,y relative to xStart,yStart
		return math.atan2( a.y - a.yStart, a.x - a.xStart ) * quickPI -- 180 / PI -- math.pi
	elseif (#arg == 2 and type(arg[1]) == "number") then
		-- angle of b relative to a
		return math.atan2( b, a ) * quickPI -- 180 / PI -- math.pi
	elseif (#arg == 2) then
		-- angle of b relative to a
		return math.atan2( b.y - a.y, b.x - a.x ) * quickPI -- 180 / PI -- math.pi
	elseif (#arg == 3) then
		-- angle between b and c found at a
		local deg = angleOf( a, b ) - angleOf( a, c ) -- target - source
		
		if (deg > 180) then
			deg = deg - 360
		elseif (deg < -180) then
			deg = deg + 360
		end
		
		return deg
	elseif (#arg == 4) then
		-- angle of x,y,x,y at second x,y relative to first x,y
		return math.atan2( d - b, c - a ) * quickPI -- 180 / PI -- math.pi
	end
	
	-- wrong set of parameters
	return nil
end
math.angleOf = angleOf

-- Brent Sorrentino
-- Returns the angle between the objects
local function angleBetween( srcObj, dstObj )
	local xDist = dstObj.x - srcObj.x
	local yDist = dstObj.y - srcObj.y
	local angleBetween = math.deg( math.atan( yDist / xDist ) )
	if ( srcObj.x < dstObj.x ) then
		angleBetween = angleBetween + 90
	else
		angleBetween = angleBetween - 90
	end
	return angleBetween
end
math.angleBetween = angleBetween

--[[
	Calculate the angle between two lines.
	
	Params:
		lineA - The first line { a={x,y}, b={x,y} }
		lineA - The first line { a={x,y}, b={x,y} }
]]--
local function angleBetweenLines( lineA, lineB )
	local angle1 = math.atan2( lineA.a.y - lineA.b.y, lineA.a.x - lineA.b.x )
	local angle2 = math.atan2( lineB.a.y - lineB.b.y, lineB.a.x - lineB.b.x )
	return math.deg( angle1 - angle2 )
end
math.angleBetweenLines = angleBetweenLines

-- returns the smallest angle between the two angles
-- ie: the difference between the two angles via the shortest distance
-- returned value is signed: clockwise is negative, anticlockwise is positve
-- returned value wraps at +/-180
-- Example code to rotate a display object by touch:
--[[
	-- called in the "moved" phase of touch event handler
	local a = mathlib.angleBetween( target, target.prevevent )
	local b = mathlib.angleBetween( target, event )
	local d = mathlib.smallestAngleDiff( a, b )
	target.prev = event
	target.rotation = target.rotation - d
]]--
local function smallestAngleDiff( target, source )
	local a = target - source

	if (a > 180) then
		a = a - 360
	elseif (a < -180) then
		a = a + 360
	end

	return a
end
math.smallestAngleDiff = smallestAngleDiff

-- Returns the angle in degrees between the first and second points, measured at the centre
-- Always a positive value
local function angleAt( centre, first, second )
	local a, b, c = centre, first, second
	local ab = math.lengthOf( a, b )
	local bc = math.lengthOf( b, c )
	local ac = math.lengthOf( a, c )
	local angle = math.deg( math.acos( (ab*ab + ac*ac - bc*bc) / (2 * ab * ac) ) )
	return angle
end
math.angleAt = angleAt

-- Returns true if the point is within the angle at centre measured between first and second
local function isPointInAngle( centre, first, second, point )
	local range = math.angleAt( centre, first, second )
	local a = math.angleAt( centre, first, point )
	local b = math.angleAt( centre, second, point )
	-- print(range,a+b)
	return math.round(range) >= math.round(a + b)
end
math.isPointInAngle = isPointInAngle

-- Forces to apply based on total force and desired angle
-- http://developer.anscamobile.com/code/virtual-dpadjoystick-template
local function forcesByAngle(totalForce, angle)
	local forces = {}
	local radians = -math.rad(angle)

	forces.x = math.cos(radians) * totalForce
	forces.y = math.sin(radians) * totalForce

	return forces
end
math.forcesByAngle = forcesByAngle

--[[
	Lines and Vectors
]]--

--[[
	Returns the length of a line.
	
	Takes either:
		x,y - two parameters with the end location of a line as ( x, y ) and (0,0) as the start
		{x,y} - one parameter with the end location of a line as {x,y} and {x=0,y=0} as the start
		{x,y},{x,y} - two parameters as the start and end of the line as {x,y} and {x,y}
		x,y,x,y - four parameters as the start and end of the line as ( x, y, x, y )
	
	Returns:
		The length of the line.
]]--
local function lengthOf( ... )
	local a, b
	
	if (#arg == 4) then
		-- four parameters spelling out x, y, x, y
		-- print("four parameters spelling out x, y, x, y")
		a = { x=arg[1], y=arg[2] }
		b = { x=arg[3], y=arg[4] }
		
	elseif (#arg == 2 and type(arg[1]) == "number") then
		-- two parameters spelling out x and y of one end
		-- print("two parameters spelling out x and y of one end")
		a = { x=arg[1], y=arg[2] }
		b = { x=0, y=0 }
		
	elseif (#arg == 1 and arg[1].a ~= nil and arg[1].b ~= nil) then
		-- one parameter containing a and b as the ends of the line
		-- print("one parameter containing a and b as the ends of the line")
		a = arg[1].a
		b = arg[1].b
		
	elseif (#arg == 1 and arg[1].xStart ~= nil) then
		-- touch event as the start and end
		-- print("touch event as the start and end")
		a = {x=arg[1].xStart, y=arg[1].yStart}
		b = arg[1]
		
	elseif (#arg == 1 and arg[1].x ~= nil) then
		-- one parameter as the x,y end
		-- print("one parameter as the x,y end")
		a = arg[1]
		b = { x=0, y=0 }
		
	else
		-- two parameters as {x,y} for each end
		-- print("two parameters as {x,y} for each end")
		a = arg[1]
		b = arg[2]
	end
	
	local width, height = b.x-a.x, b.y-a.y
	return (width*width + height*height)^0.5 -- math.sqrt(width*width + height*height)
	-- nothing wrong with math.sqrt, but I believe the ^.5 is faster
end
math.lengthOf = lengthOf

--[[
	Description:
		Extends the point away from or towards the origin to the length of len.
	
	Params:
		origin = Point to extrude away from, pass nil to default to {x=0,y=0}
		max =
			If param max is nil then the lenOrMin value is the distance to calculate the point's location
			If param max is not nil then the lenOrMin value is the minimum clamping distance to extrude to
		lenOrMin = the length or the minimum length to extrude the point's distance to
		max = the maximum length to extrude to
		aspoint = true to return as a table containing {x,y}, default: false
	
	Returns:
		x, y = extruded point in real space (this is probably the one you want)
		x, y = extruded point relative to origin point
]]--
local function extrudeToLen( origin, point, lenOrMin, max, aspoint )
	origin = origin or {x=0,y=0}
	
	local length = lengthOf( origin, point )
	if (length == 0) then
		return origin.x, origin.y
	end
	local len = lenOrMin
	if (max ~= nil) then
		if (length < lenOrMin) then
			len = lenOrMin
		elseif (length > max) then
			len = max
		else -- the point is within the min/max clamping range
			return point.x, point.y
		end
	end
	local factor = len / length
	local x, y = (point.x - origin.x) * factor, (point.y - origin.y) * factor
	if (aspoint) then
		return { x=x + origin.x, y=y + origin.y }, { x=x, y=y }
	else
		return x + origin.x, y + origin.y, x, y
	end
end
math.extrudeToLen = extrudeToLen

-- returns true when the point is on the right of the line formed by the north/south points
local function isOnRight( north, south, point )
	local a, b, c = north, south, point
	local factor = (b.x - a.x)*(c.y - a.y) - (b.y - a.y)*(c.x - a.x)
	return factor > 0, factor
end
math.isOnRight = isOnRight

--[[
	Reflect point across line.
	
	Parameters:
		line: Line with ends {a,b} each end in the form {x,y}
		point: {x,y} point to be reflected
	
	Parameters:
		a, b: Line ends of form {x,y}
		point: Point to reflect across the a-b line, also form of {x,y}
	
	Returns:
		{x,y} point on other side of the line, as if reflected in a mirror.
]]--
local function reflectPointAcrossLine( ... )
	local north, south, point
	
	if (#arg == 2) then -- line and point
		north = arg[1].a
		south = arg[1].b
		point = arg[2]
	elseif (#arg == 3) then -- line end a, line end b, point
		north = arg[1]
		south = arg[2]
		point = arg[3]
	elseif (#arg == 6) then -- line a x, line a y, line b x, line b y, pt x, pt y
		north = { x=arg[1], y=arg[2] }
		south = { x=arg[3], y=arg[4] }
		point = { x=arg[5], y=arg[6] }
	else
		return nil
	end
	
	local x1, y1, x2, y2 = north.x, north.y, south.x, south.y
	local x3, y3 = point.x, point.y
	local x4, y4 = 0, 0 -- reflected point
	local dx, dy, t, d

	dx = y2 - y1
	dy = x1 - x2
	t = dx * (x3 - x1) + dy * (y3 - y1)
	t = t / (dx * dx  +  dy * dy)

	x = x3 - 2 * dx * t
	y = y3 - 2 * dy * t

	return { x=x, y=y }
end
math.reflectPointAcrossLine = reflectPointAcrossLine

--[[
	Bounces a point against a line. The point must have a velocity property of form {x,y}
	If the line between pt and pt+pt.velocity does not intersect with the line parameter only nil is returned
	Three points are returned. First, the point where pt would be if it's velocity property were applied is reflected across the line.
	Second, using a line at a right angle drawn out from the intersection point the pt location is reflected.
	Third, the point of the bounce on the line.
	
	Parameters:
		line: The line to bounce the point against, of form { a={x,y}, b={x,y} }
		pt: The point to bounce against the line, of form {x,y}
	
	Returns:
		Success = true if the point bounced off the line, otherwise false
		Bounced point using the line (includes new .velocity)
		Reflected pt using the line at the intersection's right angle
		Point of intersection where the pt will cross the line
]]--
local function bouncePointAgainstLine( line, pt )
	local nextPt = { x=pt.x+pt.velocity.x, y=pt.y+pt.velocity.y }
	local lineB = { a=pt, b=nextPt }
	
	local success, inter = math.doLinesIntersect( line.a, line.b, lineB.a, lineB.b )
	
	if (not success) then
		return success, nil -- the velocity of pt does not cause it to make contact with the line
	end
	
	local len = math.lengthOf( pt, inter )
	
	local bounce = math.reflectPointAcrossLine( line, nextPt )
	local x, y = math.extrudeToLen( inter, bounce, len )
	bounce.velocity = { x=bounce.x-inter.x, y=bounce.y-inter.y }
	
	return success, bounce, {x=x,y=y}, inter -- bounced point, reflected point, intersection
end
math.bouncePointAgainstLine = bouncePointAgainstLine

--[[
	Reflects a point off a polygon, either keeping it within or without by checking intersections.
	Multiple bounces may be processed with only the last resulting location being returned, this is because the
	distance travelled by the 'pt' point is properly calculated.
	The original velocity of the 'pt' point will also be maintained and will not decrement because of the last bounce distance.
	
	Parameters:
		points: List of points comprising the polygon, assumes closed (auto-joins the first and last points)
		pt: The point being fired at/in the polygon. Must contain {x,y,velocity={x,y}} 
	
	Returns:
		pt: A point matching the definition of the 'pt' parameter, with {x,y,velocity={x,y}} for the resulting position. Nil, if there was no collision detected.
		list: List of locations the point bounced from
]]--
local function bouncePointAgainstPolygon( points, pt )
	local wp = math.wrapIndex
	
	local function comparePoints( a, b )
		return math.round( math.lengthOf( a, b ) ) == 0
	end
	
	local function getLine( index )
		return { a=points[index], b=points[ wp( index+1, points ) ] }
	end
	
	-- get velocity speed
	local initspeed = math.lengthOf( {x=0,y=0}, pt.velocity )
	
	-- initialise output list
	local intersections = {}
	
	-- perform first iteration...
	
	while (true) do

		local a = pt

		local b = { x=a.x+a.velocity.x, y=a.y+a.velocity.y }

		local found = math.polygonLineIntersection( points, a, b, true ) -- polygonLineIntersection( polygon, a, b, sort, notWrapped )

		if (#found > 0 and comparePoints( found[1], a )) then table.remove( found, 1 ) end

		if (#found == 0) then print(#intersections); return a, intersections end

		local success, bounce, reflect, inter = math.bouncePointAgainstLine( getLine( found[1].lineIndex ), a ) -- success, bounced point, reflected point, intersection

		inter.velocity = bounce.velocity

		intersections[ #intersections+1 ] = inter

		pt = inter
	end
end
math.bouncePointAgainstPolygon = bouncePointAgainstPolygon

--[[
	Shows that the lines intersect.
	
	Parameters:
		a, b: Lines with a and b ends, each end with x,y coords.
		a, b, c, d: Line ends for a-b and c-d, each end having x,y coords.
		ax, ay, bx, by, cx, cy, dx, dy: Full points of two lines
	
	Returns:
		true, x, y: If the lines intersect
		false: If the lines do not intersect
	
	Ref:
		http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
]]--
local function getLineIntersection( ... ) -- , *i_x, *i_y)
	local p0_x, p0_y, p1_x, p1_y, p2_x, p2_y, p3_x, p3_y
	
	-- separate parameters
	if (#arg == 2) then
		p0_x, p0_y, p1_x, p1_y = arg[1].a.x, arg[1].a.y, arg[1].b.x, arg[1].b.y
		p2_x, p2_y, p3_x, p3_y = arg[2].a.x, arg[2].a.y, arg[2].b.x, arg[2].b.y
	elseif (#arg == 4) then
		p0_x, p0_y, p1_x, p1_y = arg[1].x, arg[1].y, arg[2].x, arg[2].y
		p2_x, p2_y, p3_x, p3_y = arg[3].x, arg[3].y, arg[4].x, arg[4].y
	elseif (#arg == 8) then
		p0_x, p0_y, p1_x, p1_y, p2_x, p2_y, p3_x, p3_y = unpack( arg )
	end
	
	local i_x, i_y -- output
	
    local s1_x, s1_y, s2_x, s2_y
    s1_x = p1_x - p0_x
    s1_y = p1_y - p0_y
    
    s2_x = p3_x - p2_x
    s2_y = p3_y - p2_y

    local s, t
    s = (-s1_y * (p0_x - p2_x) + s1_x * (p0_y - p2_y)) / (-s2_x * s1_y + s1_x * s2_y)
    t = ( s2_x * (p0_y - p2_y) - s2_y * (p0_x - p2_x)) / (-s2_x * s1_y + s1_x * s2_y)

    if (s >= 0 and s <= 1 and t >= 0 and t <= 1) then
        -- Collision detected
    	i_x = p0_x + (t * s1_x)
    	i_y = p0_y + (t * s1_y)
        return true, i_x, i_y
    end

    return false -- ; // No collision
end
math.getLineIntersection = getLineIntersection

--[[ Used when the tablelib library is not provided. ]]--
local hasTableLib = pcall(function() require("tablelib") end)
if (not hasTableLib and table.range == nil) then
	print("tablelib not provided: Adding table.range()")
	table.range = function( tbl, index, size )
		if (index == nil or index < 1) then return nil end
		size = size or #tbl-index+1
		local output = {}
		for i=index, index+size-1 do
			output[#output+1] = tbl[i]
		end
		return output
	end
end

--[[
	Finds a list of intersection points between two paths of { x,y,x,y, ... } format.
	Paths must be even-numbered, have more than four values and consist of only x,y values in that order.
	The path will be checked for self intersections if the isSelf parameter is true or the path tables are the same table.
	
	Parameters:
		aPath: First path
		bPath: Second path
		isSelf: true if the two path tables are the same path, leave nil otherwise
	
	Returns:
		{ {aIndex,bIndex,x,y,aPath,bPath}, ... }
		Table of tables containing the a path and b path indices and x,y coordinates of intersection.
]]--
local function getIntersectionsBetween( aPath, bPath, isSelf )
	local intersections = {}
	
	local isSelf = isSelf or (aPath == bPath)
	
	for aIndex=1, #aPath-3, 2 do
		for bIndex=1, #bPath-3, 2 do
			if (not isSelf or (isSelf and bIndex < aIndex-2 or bIndex > aIndex+2)) then
				local pts = table.copy( table.range( aPath, aIndex, 4 ), table.range( bPath, bIndex, 4 ) )
				local does, x, y = math.getLineIntersection( unpack( pts ) )
				
				if (does) then
					local intersection = {
						x=x, y=y,
						target={ index=aIndex, path=aPath },
						other={ index=bIndex, path=bPath },
					}
					intersections[#intersections+1] = intersection
				end
			end
		end
	end
	
	return intersections
end
math.getIntersectionsBetween = getIntersectionsBetween

local function test_getIntersectionsBetween()
	local a = {652,116,646,118,639,119,632,121,624,123,615,125,606,127,596,129,586,131,575,133,564,135,553,137,541,139,528,142,516,144,503,147,490,149,476,152,462,155,449,157,435,160,421,163,406,166,392,169,378,172,363,175,349,179,335,182,321,185,307,189,293,192,279,196,266,199,252,203,239,207,227,210,214,214,202,218,190,222,179,226,168,230,158,234,148,239,139,243,130,247,122,252,114,256,107,261,101,265,93,272,86,278,79,285,73,291,67,298,62,306,57,313,52,320,48,328,44,336,40,344,37,352,34,360,31,368,29,377,27,385,26,393,25,402,24,411,24,419,24,428,24,436,25,445,26,453,28,462,30,470,32,479,35,487,38,495,42,503,46,511,50,519,55,527,60,535,66,542,72,549,78,556,85,563,102,577,121,589,144,601,170,612,198,624,229,635,261,646,294,656,329,667,363,677,398,687,433,697,466,707,498,717,529,727,557,737,583,747,606,757,626,767,640,776,647,781,654,786,660,791,666,796,671,801,677,805,682,810,686,815,690,819,694,824,698,828,701,833,704,837,706,842,709,846,710,851,712,855,713,860,714,864,714,869,714,878,713,882,712,887,711,892,709,896,707,901,704,906,702,911,699,916,695,921,691,926,687,931,682,937,677,942,672,948,666,953,660,959,654,965,650,968,643,974,635,980,626,986,616,993,606,1000,595,1007,583,1014,570,1021,557,1029,543,1036,529,1044,514,1052,499,1060,484,1068,468,1076,452,1084,435,1092,419,1100,402,1108,386,1116,369,1124,352,1132,336,1140,319,1148,303,1155,287,1163,271,1170,255,1177,240,1184,225,1191,211,1198,197,1204,183,1210,171,1216,158,1222,147,1227,136,1232,126,1237,117,1242,109,1246,101,1249}
	local b = {98,66,103,69,110,73,116,76,124,80,132,84,140,88,150,92,159,96,170,101,180,105,191,110,203,115,215,120,227,125,239,131,252,136,265,142,279,147,292,153,306,159,319,165,333,171,347,177,361,184,375,190,389,197,403,203,417,210,431,217,444,223,458,230,471,237,484,244,496,251,509,259,521,266,533,273,544,280,555,288,565,295,575,302,585,310,594,317,602,325,610,332,617,340,623,347,629,355,634,362,639,370,642,377,645,385,647,392,648,400,647,414,645,423,641,433,635,444,628,454,620,464,610,475,599,486,587,497,574,508,560,519,545,530,530,541,513,553,497,564,479,576,461,587,443,599,424,611,406,622,387,634,368,646,349,658,330,670,312,682,293,693,275,705,258,717,241,729,225,740,209,752,195,763,181,775,168,786,156,798,145,809,136,820,127,831,120,842,115,852,111,863,109,873,108,884,109,894,111,903,115,913,119,923,125,933,131,943,139,952,148,963,157,973,168,983,179,993,191,1003,204,1013,217,1023,231,1033,245,1043,260,1053,275,1063,291,1073,307,1082,324,1092,340,1102,357,1111,374,1121,391,1130,408,1139,425,1148,442,1157,458,1165,475,1174,491,1182,507,1190,523,1198,539,1206,554,1214,568,1221,582,1228,595,1235,608,1241,620,1248,631,1254,642,1259,652,1265,660,1270,668,1275,675,1279}
	
	local aline = display.newLine( unpack( a ) )
	aline.strokeWidth = 6
	
	local bline = display.newLine( unpack( b ) )
	bline.strokeWidth = 6
	
	local intersections = math.getIntersectionsBetween( a, b )
	
	for i=1, #intersections do
		local pt = intersections[i]
		local dot = display.newCircle( pt.x, pt.y, 6 )
		dot:setFillColor( 1,0,0 )
	end
end
--test_getIntersectionsBetween()

local function test_getSelfIntersections( path )
	local a = {100,100,107,102,113,103,119,103,125,104,132,104,140,104,147,104,155,104,164,104,172,104,181,104,191,104,200,104,210,104,220,105,225,106,230,106,235,107,240,107,245,108,250,109,255,110,260,111,266,112,271,113,276,115,281,116,287,118,292,120,297,121,302,123,308,126,313,128,318,130,323,133,328,136,333,139,339,142,344,146,349,149,354,153,359,157,364,161,369,165,374,170,378,175,383,180,388,185,392,190,397,196,415,221,432,246,451,274,470,306,490,340,511,376,533,414,554,453,575,493,596,534,615,575,634,616,651,655,666,694,679,731,690,767,699,799,704,829,706,856,705,879,701,899,696,907,692,913,686,920,679,926,672,932,663,937,654,942,643,948,632,952,620,957,608,961,595,965,581,969,567,973,552,976,537,979,521,982,505,984,489,986,472,988,456,990,439,991,422,992,405,993,388,994,371,994,354,994,337,993,321,993,304,992,288,990,272,989,257,987,242,985,228,982,214,980,200,976,188,973,176,969,164,965,154,961,144,956,135,951,127,945,120,940,114,934,109,927,105,921,102,913,100,906,100,900,100,894,101,887,103,880,105,872,108,864,111,855,115,846,120,836,125,826,131,816,137,805,143,794,150,782,158,770,166,758,174,746,183,733,192,720,201,707,211,694,221,680,231,666,242,652,252,638,263,623,275,609,286,594,298,580,310,565,322,550,334,535,346,520,358,505,371,490,383,475,396,460,408,445,421,430,433,415,446,401,458,386,470,372,483,357,495,343,507,329,518,315,530,302,542,289,553,275,564,263,575,250,586,238,596,226,606,214,616,202,625,191,634,181,643,171,651,161,659,151,666,142,673,134,680,125,686,118,692,111,697,104}
	
	local aline = display.newLine( unpack( a ) )
	aline.strokeWidth = 6
	
	local intersections = math.getIntersectionsBetween( a, a )
	
	for i=1, #intersections do
		local pt = intersections[i]
		local dot = display.newCircle( pt.x, pt.y, 4 )
		dot:setFillColor( 1,0,0 )
	end
end
--test_getSelfIntersections()

-- This is based off an explanation and expanded math presented by Paul Bourke:
-- It takes two lines as inputs and returns true if they intersect, false if they don't.
-- If they do, ptIntersection returns the point where the two lines intersect.
-- params a, b = first line
-- params c, d = second line
-- param ptIntersection: The point where both lines intersect (if they do)
-- http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/
-- http://paulbourke.net/geometry/pointlineplane/
local function doLinesIntersect( a, b, c, d )
	-- parameter conversion
	local L1 = {X1=a.x,Y1=a.y,X2=b.x,Y2=b.y}
	local L2 = {X1=c.x,Y1=c.y,X2=d.x,Y2=d.y}

	-- Denominator for ua and ub are the same, so store this calculation
	local _d = (L2.Y2 - L2.Y1) * (L1.X2 - L1.X1) - (L2.X2 - L2.X1) * (L1.Y2 - L1.Y1)

	-- Make sure there is not a division by zero - this also indicates that the lines are parallel.
	-- If n_a and n_b were both equal to zero the lines would be on top of each
	-- other (coincidental).  This check is not done because it is not
	-- necessary for this implementation (the parallel check accounts for this).
	if (_d == 0) then
		return false
	end

	-- n_a and n_b are calculated as seperate values for readability
	local n_a = (L2.X2 - L2.X1) * (L1.Y1 - L2.Y1) - (L2.Y2 - L2.Y1) * (L1.X1 - L2.X1)
	local n_b = (L1.X2 - L1.X1) * (L1.Y1 - L2.Y1) - (L1.Y2 - L1.Y1) * (L1.X1 - L2.X1)

	-- Calculate the intermediate fractional point that the lines potentially intersect.
	local ua = n_a / _d
	local ub = n_b / _d

	-- The fractional point will be between 0 and 1 inclusive if the lines
	-- intersect.  If the fractional calculation is larger than 1 or smaller
	-- than 0 the lines would need to be longer to intersect.
	if (ua >= 0 and ua <= 1 and ub >= 0 and ub <= 1) then
		local x = L1.X1 + (ua * (L1.X2 - L1.X1))
		local y = L1.Y1 + (ua * (L1.Y2 - L1.Y1))
		return {x=x, y=y}
	end

	return false
end
math.doLinesIntersect = doLinesIntersect

function math.doesThickLineIntersect( a, b, abw, c, d, cdw )
	w = w or 10
	
	-- get rects for the stroked lines
	local ab = display.newRect( math.avg(a.x,b.x), math.avg(a.y,b.y), math.lengthOf(a,b), abw )
	ab.rotation = math.angleOf( a,b )
	
	local cd = display.newRect( math.avg(c.x,d.x), math.avg(c.y,d.y), math.lengthOf(c,d), cdw )
	cd.rotation = math.angleOf( c,d )
	
	-- get path points of the rects
	local function getOutline( rect )
		local pathA = {}
		local x, y = rect:localToContent( -rect.width/2, -rect.height/2 )
		pathA[#pathA+1], pathA[#pathA+2] = x, y
		x, y = rect:localToContent( rect.width/2, -rect.height/2 )
		pathA[#pathA+1], pathA[#pathA+2] = x, y
		x, y = rect:localToContent( rect.width/2, rect.height/2 )
		pathA[#pathA+1], pathA[#pathA+2] = x, y
		x, y = rect:localToContent( -rect.width/2, rect.height/2 )
		pathA[#pathA+1], pathA[#pathA+2] = x, y
		return pathA
	end
	
	local pathA = getOutline( ab )
	local pathB = getOutline( cd )
	
	-- remove rects
	ab:removeSelf()
	cd:removeSelf()
	
	-- get polygon intersection
	local p, intersects = math.getPolygonIntersection( pathA, pathB )
	
	if (intersects) then
		-- if intersection is returned, return avg x and y values of returned polygon
		local x, y = 0, 0
		for i=1, #p do
			x, y = x+p[i].x, y+p[i].y
		end
		return { x=x/#p, y=y/#p }
	else
		-- if intersection is not returned, return false
		return false
	end
end
local function testDoesThickLineIntersect()
	timer.performWithDelay( 100, function()
		local pt = math.doesThickLineIntersect(
			{x=100,y=100},{x=300,y=100},1,
			{x=200,y=100},{x=500,y=100},1
		)
	
		if (pt) then
			display.newCircle( pt.x, pt.y, 10 ).fill = {0,0,1}
		end
	end, 1 )
end
--testDoesThickLineIntersect()

--[[
	Points defined must define overlapping lines.
	Returns the line segment ends where the two lines overlapp.
	
	Parameters:
		ax, ay, bx, by, cx, cy, dx, dy: Line ends defined individually
		{a={x,y},b={x,y}}, {a={x,y},b={x,y}}: Lines defined individually
	
	Returns:
		nil if no parallel lines
		{a={x,y},b={x,y}}, true: If the lines are parallel on the x axis
		{a={x,y},b={x,y}}, false: If the lines are parallel on the y axis
]]--
function math.getParallelLineOverlap( ... )
	local ax, ay, bx, by, cx, cy, dx, dy
	
	if (#arg == 2) then
		ax, ay, bx, by = arg[1].a.x, arg[1].a.y, arg[1].b.x, arg[1].b.y
		cx, cy, dx, dy = arg[2].a.x, arg[2].a.y, arg[2].b.x, arg[2].b.y
	elseif (#arg == 8) then
		ax, ay, bx, by, cx, cy, dx, dy = unpack( arg )
	end
	
	local a, b, c, d
	local xSame
	
	if (ax == bx and ax == cx and ax == dx) then
		xSame = true
		a, b, c, d = ay, by, cy, dy
	elseif (ay == by and ay == cy and ay == dy) then
		xSame = false
		a, b, c, d = ax, bx, cx, dx
	else
		return nil
	end
	
	local function sortAB( a, b )
		return math.min( a, b ), math.max( a, b )
	end
	
	a, b = sortAB( a, b )
	c, d = sortAB( c, d )
	
	local OverlapInterval = nil
	
	if (b - c >= 0 and d - a >=0 ) then
		if (xSame) then
			OverlapInterval = { a={ x=ax, y=math.max(a, c) }, b={ x=ax, y=math.min(b, d) } }
		else
			OverlapInterval = { a={ x=math.max(a, c), y=ay }, b={ x=math.min(b, d), y=ay } }
		end
	end
	
	return OverlapInterval, xSame
end
local function testGetParallelLineOverlap()
	local inner = display.newGroup()
	
	for i=0, 355, 10 do
		local pt = math.rotateTo( {x=0,y=-100}, i )
		local line = display.newLine( inner, pt.x+display.actualCenterX, pt.y+display.actualCenterY, pt.x*2+display.actualCenterX, pt.y*2+display.actualCenterY )
		line.strokeWidth = 5
		line.stroke = {0,0,0}
		line.points = { pt.x+display.actualCenterX, pt.y+display.actualCenterY, pt.x*2+display.actualCenterX, pt.y*2+display.actualCenterY }
	end
	
	local outer = display.newGroup()
	outer.x, outer.y = display.actualCenterX, display.actualCenterY
	
	local line = display.newLine( outer, 0, -150, 0, -250 )
	line.strokeWidth = 5
	line.stroke = {1,0,0}
	
	transition.to( outer, { time=30000, rotation=360 } )
	
	local r = 10
	
	Runtime:addEventListener( "enterFrame", function()
		for i=1, inner.numChildren do
			local ax, ay, bx, by = math.nearest(inner[i].points[1],r), math.nearest(inner[i].points[2],r), math.nearest(inner[i].points[3],r), math.nearest(inner[i].points[4],r)
			
			local cx, cy = outer:localToContent( 0, -150 )
			local dx, dy = outer:localToContent( 0, -250 )
			
			cx, cy, dx, dy = math.nearest(cx,r), math.nearest(cy,r), math.nearest(dx,r), math.nearest(dy,r)
			
			local overlap = math.getParallelLineOverlap( ax, ay, bx, by, cx, cy, dx, dy )
			
			if (overlap) then
				local line = display.newLine( overlap.a.x, overlap.a.y, overlap.b.x, overlap.b.y )
				line.stroke = {0,.4,1}
				line.strokeWidth = 10
			end
		end
	end )
end
--testGetParallelLineOverlap()

-- returns the closest point on the line between A and B from point P
local function GetClosestPoint( A,  B,  P, segmentClamp )
    local AP = { x=P.x - A.x, y=P.y - A.y }
    local AB = { x=B.x - A.x, y=B.y - A.y }
    local ab2 = AB.x*AB.x + AB.y*AB.y
    local ap_ab = AP.x*AB.x + AP.y*AB.y
    local t = ap_ab / ab2
    
    if (segmentClamp ~= false) then
         if (t < 0.0) then
                t = 0.0
         elseif (t > 1.0) then
                t = 1.0
         end
    end
 
    local closest = { x=A.x + AB.x * t, y=A.y + AB.y * t }
 
    return closest
end
math.GetClosestPoint = GetClosestPoint

--[[
	Returns the closest line and the nearest point on that line.
	
	Parameters:
		pt: The point to check againt the polygon.
		points: The points of a polygon.
		isClosed: True if the first point is the same as the last point (passed in as a closed polygon) otherwise the function will behave as though the last line connects the first and last points.
	
	Returns:
		table: A table of the closest lines and the points on those lines which is closest to the 'pt' parameter. Table contains { {pt,len,index}, ... }
]]--
local function closestLineAndPoint( pt, points, isClosed )
	local wp = math.wrapIndex
	
	local distances = {}
	
	local function compare( a, b )
		return (a.len < b.len)
	end
	
	local total = points.numChildren or #points
	local adjust = 0
	if (isClosed) then
		adjust = -1
	end
	
	for i=1, total+adjust do
		local p = math.GetClosestPoint( points[wp(i,points)], points[wp(i+1,points)], pt )
		local len = math.lengthOf( p, pt )
		distances[#distances+1] = { pt=p, len=len, index=i }
	end
	
	table.sort( distances, compare )
	
	local output = {}
	local smallestLen = distances[1].len
	local i = 1
	
	while (distances[i].len == smallestLen) do
		output[#output+1] = distances[i]
		i = i + 1
	end
	
	return output
end
math.closestLineAndPoint = closestLineAndPoint

--[[
	Circle Line Intersection
	
	Description:
		Determines if a line (defined by Ax,Ay-Bx,By) intersects with a circle (defined by Cx,Cy with radius R).
		Always returns two points if the intersection is secant (see third output).
	
	Params:
		Ax, Ay: Start of the line
		Bx, By: End of the line
		Cx, Cy: Centre of the circle
		R: Radius of the circle
	
	Returns:
		"none": The line does not touch the circle.
		"tangent", x, y: The line meets the edge of the circle but does not cut into it.
			The x,y is the location of the intersection.
		"secant", ax, ay, bx, by: The line cuts through the circle.
			ax, ay is the first intersection.
			bx, by is the second intersection.
	
	Example:
		local Ax, Ay, Bx, By, Cx, Cy, R = 100,100 , 200,100 , 200,200 , 110
		
		display.newLine( Ax, Ay, Bx, By ).strokeWidth = 4
		display.newCircle( Cx, Cy, R ):setFillColor( 0,0,1,.5 )
		display.newCircle( Cx, Cy, 4 ):setFillColor( 1,0,0 )
		
		local intersectType, ax, ay, bx, by, t = math.circleLineIntersection( Ax, Ay, Bx, By, Cx, Cy, R )
		
		if (t == 0) then
			print("Intersection at first end")
		elseif (t == 1) then
			print("Intersection at last end")
		elseif (t > 0 and t < 1) then
			print("Intersection between line ends")
		elseif (t < 0) then
			print("Intersection before first end")
		elseif (t > 1) then
			print("Intersection after last end")
		end
		
		if (intersectType == math.none) then
			print("none")
		elseif (intersectType == math.tangent) then
			print("tanget")
			display.newCircle( ax, ay, 5 ):setFillColor(0,.5,0)
		elseif (intersectType == math.secant) then
			print("secant")
			display.newCircle( ax, ay, 3 ):setFillColor( 1,.4,.4 )
			display.newCircle( bx, by, 3 ):setFillColor( .4,.4,1 )
		end
	
	Ref:
		http://stackoverflow.com/questions/1073336/circle-line-segment-collision-detection-algorithm
		http://mathworld.wolfram.com/Circle-LineIntersection.html
]]--
local function circleLineIntersection( Ax, Ay, Bx, By, Cx, Cy, R )
	-- compute the euclidean distance between A and B
	local LAB = math.sqrt( (Bx-Ax)*(Bx-Ax)+(By-Ay)*(By-Ay) )

	-- compute the direction vector D from A to B
	local Dx = (Bx-Ax)/LAB
	local Dy = (By-Ay)/LAB

	-- Now the line equation is x = Dx*t + Ax, y = Dy*t + Ay with 0 <= t <= 1.

	-- compute the value t of the closest point to the circle center (Cx, Cy)
	local t = ( Dx*(Cx-Ax) + Dy*(Cy-Ay) )
	
	-- This is the projection of C on the line from A to B.

	-- compute the coordinates of the point E on line and closest to C
	local Ex = t*Dx+Ax
	local Ey = t*Dy+Ay

	-- compute the euclidean distance from E to C
	local LEC = math.sqrt( (Ex-Cx)*(Ex-Cx)+(Ey-Cy)*(Ey-Cy) )

	-- test if the line intersects the circle
	if ( LEC < R ) then
		-- compute distance from t to circle intersection point
		dt = math.sqrt( R*R - LEC*LEC )
	
		-- compute first intersection point
		Fx = (t-dt)*Dx + Ax
		Fy = (t-dt)*Dy + Ay
	
		-- compute second intersection point
		Gx = (t+dt)*Dx + Ax
		Gy = (t+dt)*Dy + Ay
		
		return "secant", Fx, Fy, Gx, Gy, t/LAB
		
	-- else test if the line is tangent to circle
	elseif ( LEC == R ) then
		-- tangent point to circle is E
		return "tangent", Ex, Ey, t/LAB
	else
		-- line doesn't touch circle
		return "none"
	end
end
math.circleLineIntersection = circleLineIntersection

--[[
	Performs unit normalisation of a vector.
	
	Description:
		Unit normalising is basically converting the length of a line to be a fraction of 1.0
		This function modified the vector value passed in and returns the length as returned by lengthOf()
	
	Note:
		Can also be performed like this:
		function Normalise(vector)
			local x,y = 	x/(x^2 + y^2)^(1/2), y/(x^2 + y^2)^(1/2)
			local unitVector = {x=x,y=y}
			return unitVector
		end
	
	Ref:
		http://www.fundza.com/vectors/normalize/index.html
]]--
local function normalise( vector )
	local len = math.lengthOf( vector )
	vector.x = vector.x / len
	vector.y = vector.y / len
	return len
end
math.normalise = normalise

--[[
	Polygons
]]--

--[[
	Calculates the area of a polygon.
	Will not calculate area for self-intersecting polygons (where vertices cross each other)
	
	Parameters:
		points: table of {x,y} points or list of {x,y,x,y...} coords
	
	Ref:
		http://www.mathopenref.com/coordpolygonarea2.html
]]--
local function polygonArea( points )
	if (type(points[1]) == "number") then
		points = math.tableToPoints( points )
	end
	
	local count = #points
	if (points.numChildren) then
		count = points.numChildren
	end
	
	local area = 0 -- Accumulates area in the loop
	local j = count -- The last vertex is the 'previous' one to the first
	
	for i=1, count do
		area = area +  (points[j].x + points[i].x) * (points[j].y - points[i].y)
		j = i -- j is previous vertex to i
	end
	
	return math.abs(area/2)
end
math.polygonArea = polygonArea

--[[
	Calculates the area of a table of polygons and also returns the sum of the areas.
	Overlapping intersecting areas are not accounted for.
	
	Parameters:
		polygons: table of polygons - see polygonArea()
	
	Returns:
		Table of { polygon, area } tables, sum of areas.
]]--
local function polygonAreas( polygons )
	local tbl = {}
	local sum = 0
	
	for i=1, #tbl do
		local polygon = polygons[i]
		local entry = { polygon=polygon, area=polygonArea( polygon ) }
		tbl[ #tbl+1 ] = entry
		sum = sum + entry.area
	end
	
	return tbl, sum
end
math.polygonAreas = polygonAreas

--[[
	Returns true if the dot {x,y} is within the polygon defined by points table { {x,y},{x,y},{x,y},... }
	Accepts coordinates list {x,y,x,y,...} or points {x,y} table or display group.
	
	Parameters:
		points: table of points or list of coordinates of polygon
		dot: point to check for being inside or outside the bounds of the polygon
	
	Return:
		true if the dot is inside the polygon
]]--
local function isPointInPolygon( points, dot )
	local count = points.numChildren
	
	if (count == nil) then
		points = math.ensurePointsTable( points )
		count = #points
	end
	
	local i, j = count, count
	local oddNodes = false
	
	for i=1, count do
		if ((points[i].y < dot.y and points[j].y>=dot.y
			or points[j].y< dot.y and points[i].y>=dot.y) and (points[i].x<=dot.x
			or points[j].x<=dot.x)) then
			if (points[i].x+(dot.y-points[i].y)/(points[j].y-points[i].y)*(points[j].x-points[i].x)<dot.x) then
				oddNodes = not oddNodes
			end
		end
		j = i
	end
	
	return oddNodes
end
math.isPointInPolygon = isPointInPolygon

--[[
	Return true if the dot { x,y } is within any of the polygons in the list.
	
	Parameters:
		polygons: table of polygons
		dot: point to check for being inside the polygons
	
	Return:
		true if the point is inside any of the polygons, the polygon containing the point
]]--
local function isPointInPolygons( polygons, dot )
	for i=1, #polygons do
		if (isPointInPolygon( polygons[i], dot )) then
			return true, polygons[i]
		end
	end
	return false
end
math.isPointInPolygons = isPointInPolygons
 
-- Returns true if the points in the polygon wind clockwise
-- Does not consider that the vertices may intersect (lines between points might cross over)
local function isPolygonClockwise( pointList )
	local area = 0
	
	if (type(pointList[1]) == "number") then
		pointList = math.pointsToTable( pointList )
		print("#pointList",#pointList)
	end
	
	for i = 1, #pointList-1 do
		local pointStart = { x=pointList[i].x - pointList[1].x, y=pointList[i].y - pointList[1].y }
		local pointEnd = { x=pointList[i + 1].x - pointList[1].x, y=pointList[i + 1].y - pointList[1].y }
		area = area + (pointStart.x * -pointEnd.y) - (pointEnd.x * -pointStart.y)
	end
	
	return (area < 0)
end
math.isPolygonClockwise = isPolygonClockwise

local function doPointsWindClockwise( ... )
	local area = 0
	
	for i=1, #arg-3, 2 do
		local pointStart = { x=arg[i] - arg[1], y=arg[i+1] - arg[2] }
		local pointEnd = { x=arg[i + 2] - arg[1], y=arg[i + 3] - arg[2] }
		area = area + (pointStart.x * -pointEnd.y) - (pointEnd.x * -pointStart.y)
	end
	
	return (area < 0)
end
math.doPointsWindClockwise = doPointsWindClockwise

--[[
local function isWindingClockwise( ... )
	local signedArea = 0
	
	for i=1, #arg, 2 do
		x1 = point[1]
		y1 = point[2]
		if point is last point
			x2 = firstPoint[1]
			y2 = firstPoint[2]
		else
			x2 = nextPoint[1]
			y2 = nextPoint[2]
		end

		signedArea += (x1 * y2 - x2 * y1)
	end
	
	return signedArea / 2
end
math.isWindingClockwise = isWindingClockwise
]]--

--[[
	Returns true if the point has less than 180 degrees between the neighbouring points.
	
	Parameters:
		point: the {x,y} point to check the angle at
		b: the {x,y} point preceding the angle point
		c: the {x,y} point following the angle point
	
	Returns:
		true if the point's angle is less than 180 degrees.
]]--
local function isPointConcave( a, b, c )
	local small = smallestAngleDiff( math.angleOf(b,a), math.angleOf(b,c) )

	if (small < 0) then
		return false
	else
		return true
	end
end
math.isPointConcave = isPointConcave

--[[
	Returns true if the polygon is concave.
	Returns nil if there are not enough points ( < 3 )
	Can accept a display group.
	
	Parameters:
		points: table of {x,y} points or list of {x,y,x,y,...} coords
	
	Returns:
		true if the polygon is not convex.
]]--
local function isPolygonConcave( points )
	-- is points a display group?
	local count = points.numChildren
	if (count == nil) then
		-- points is not a display group...
		-- ensure table of points
		points = math.ensurePointsTable( points )
		count = #points
	end
	
	-- cannot check if input is not a polygon
	if (count < 3) then
		return nil
	end
	
	local isConcave = true
	
	for i=1, count do
		if (i == 1) then
			isConcave = isPointConcave( points[count],points[1],points[2] )
		elseif (i == count) then
			isConcave = isPointConcave( points[count-1], points[count],points[1] )
		else
			isConcave = isPointConcave( points[i-1], points[i], points[i+1] )
		end
		
		if (not isConcave) then
			return false
		end
	end
	
	return true
end
math.isPolygonConcave = isPolygonConcave

--[[
	Returns list of points where a polygon intersects with the line a,b, sorted by closest first if necessary.
	Assumes polygon is standard display format: { x,y,x,y,x,y,x,y, ... }
	
	Parameters:
		polygon: table of points in either {x,y,x,y,...} or { {x,y}, ... } format
		a, b: ends of the line to check for intersection with the polygon format {x,y}
		sort: true to sort the found intersections into order from a to b
		notWrapped: True if the first and last points are not the same.
	
	Returns:
		Table of intersection points with the polygon line's index {x,y,lineIndex,len}
]]--
local function polygonLineIntersection( polygon, a, b, sort, notWrapped )
	polygon = math.ensurePointsTable( polygon )
	
	local wp = math.wrapIndex
	
	local len = polygon.numChildren or #polygon
	
	local wrapAdjust = 0
	if (notWrapped == true) then
		wrapAdjust = -1
	end
	
	local points = {}
	local idx, good = wp(len+wrapAdjust+1,polygon)
	
	for i=1, len+wrapAdjust do
		local idx, good = wp(i+1,polygon)
		local success, pt = math.doLinesIntersect( a, b, polygon[i], polygon[idx] )
		
		if (success) then
			pt.lineIndex = i
			pt.len = math.lengthOf( a, pt )
			points[ #points+1 ] = pt
		end
	end
	
	if (sort and #points > 1) then
		table.sort( points, function(f,g) return f.len < g.len end )
	end
	
	return points
end
math.polygonLineIntersection = polygonLineIntersection

--[[
	Description:
		Calculates the average of all the x's and all the y's and returns the average centre of all points.
		Works with a display group or table proceeding { {x,y}, {x,y}, ... }
	
	Params:
		pts = list of {x,y} points to get the average middle point from
	
	Returns:
		x, y = average centre location of all the points
]]--
local function avgMidPoint( ... )
	local pts = arg
	
	local x, y, c = 0, 0, #pts
	if (pts.numChildren and pts.numChildren > 0) then c = pts.numChildren end
	
	for i=1, c do
		x = x + pts[i].x
		y = y + pts[i].y
	end
	return x/c, y/c
end
math.avgMidPoint = avgMidPoint

--[[
	Calculates the middle of a polygon's bounding box - as if drawing a square around the polygon and finding the middle.
	Also calculates the width and height of the bounding box.
	
	Parameters:
		Polygon coordinates as a table of points, display group or list of coordinates.
	
	Returns:
		Centroid (centre) x, y
		Bounding box width, height
	
	Notes:
		Does not centre the polygon. To do this use: math.centrePolygon
]]--
local function getBoundingCentroid( pts )
	pts = math.ensurePointsTable( pts )
	
	local xMin, xMax, yMin, yMax = 100000000, -100000000, 100000000, -100000000
	
	for i=1, #pts do
		local pt = pts[i]
		if (pt.x < xMin) then xMin = pt.x end
		if (pt.x > xMax) then xMax = pt.x end
		if (pt.y < yMin) then yMin = pt.y end
		if (pt.y > yMax) then yMax = pt.y end
	end
	
	local width, height = xMax-xMin, yMax-yMin
	local cx, cy = xMin+(width/2), yMin+(height/2)
	
	local output = {
		centroid = { x=cx, y=cy },
		width = width,
		height = height,
		bounding = { xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax },
	}
	
	return output
end
math.getBoundingCentroid = getBoundingCentroid

--[[
	Produces an adjusted polygon so that the vertices are centred on the bounding centroid (square bounding box middle.)
	
	Parameters:
		List of coordinates, display group or table of points of the polygon.
		cx, cy: Optional centre to focus on
	
	Returns:
		Table of points for the adjusted polygon.
		x, y of the centre of the polygon.
	
	Notes:
		Centres the polygon around the provided point or calculated midpoint. To avoid this use: math.getBoundingCentroid
]]--
local function centerPoly( pts, cx, cy )
	local count, islist
	pts, count, islist = math.ensurePointsTable( pts )
	local output = {}
	
	local x, y
	local minx, maxx, miny, maxy = 100000000, -100000000, 100000000, -100000000
	
	-- get dimensions
	for i=1, #pts do
		x, y = pts[i].x, pts[i].y
		if (x < minx) then minx = x end
		if (x > maxx) then maxx = x end
		if (y < miny) then miny = y end
		if (y > maxy) then maxy = y end
	end
	
	-- get bounds
	local width, height = maxx-minx, maxy-miny
	
	-- get centre
	if (cx and cy) then
		x, y = cx, cy
	else
		x, y = minx+(width/2), miny+(height/2)
	end
	
	-- centre the polygon around the midpoint
	for i=1, #pts do
		output[#output+1] = {x=pts[i].x-x,y=pts[i].y-y}
	end
	
	if (islist) then
		output = math.pointsToTable( output )
	end
	
	return output, { x=x, y=y, width=width, height=height, minx=minx, miny=miny, maxx=maxx, maxy=maxy }
end
math.centrePolygon = centerPoly
math.centerPolygon = centerPoly

local function centerPolyForOutline( pts )
	local points, tbl = centerPoly( pts )
	return centerPoly( pts, tbl.minx, tbl.miny )
end
math.centerPolyForOutline = centerPolyForOutline

--[[
	Scales a path or {x,y,x,y,...} points.
	
	Parameters:
		path: path of points as accepted by newLine and newPolygon
		scale: amount to multiple the values in the path by
	
	Returns:
		A table of {x,y,x,y,...} values each simply multiplied by the scale parameter.
]]--
local function scalePath( path, scale )
	local p = {}
	for i=1, #path do
		p[i] = scale * path[i]
	end
	return p
end
math.scalePath = scalePath

--[[
	Description:
		Calculates the average of all the x's and all the y's and returns the average centre of all points.
		Works with a table proceeding {x,y,x,y,...} as used with display.newLine or physics.addBody
	
	Params:
		pts = table of x,y values in sequence
	
	Returns:
		x, y = average centre location of all points
]]--
local function midPointOfShape( pts )
	local x, y, c, t = 0, 0, #pts, #pts/2
	for i=1, c-1, 2 do
		x = x + pts[i]
		y = y + pts[i+1]
	end
	return x/t, y/t
end
math.midPointOfShape = midPointOfShape

--[[
	Description:
		Takes two polygons of the form {{x,y},{x,y},...} and determines if they intersect.
		Accepts parameters as display groups, tables of points {x,y} or lists of coords {x,y,x,y,...}
	
	Parameters:
		subjectPolygon: first polygon to intersect with the second
		clipPolygon: second polygon to intersect with the first
	
	Returns:
		Polygon of points of intersection between the two input polygons.
		True if the two do intersect, false if they are not touching.
	
	Example:
		subjectPolygon = {{x=50, y=150}, {x=200, y=50}, {x=350, y=150}, {x=350, y=300}, {x=250, y=300}, {x=200, y=250}, {x=150, y=350}, {x=100, y=250}, {x=100, y=200}}
		clipPolygon = {{x=100, y=100}, {x=300, y=100}, {x=300, y=300}, {x=100, y=300}}
		outputList, intersects = clip(subjectPolygon, clipPolygon)
	
	Ref:
		http://rosettacode.org/wiki/Sutherland-Hodgman_polygon_clipping#Lua
]]--
local function getPolygonIntersection( subjectPolygon, clipPolygon )
	subjectPolygon = math.copyToPointsTable( subjectPolygon )
	clipPolygon = math.copyToPointsTable( clipPolygon )
	
	local function inside(p, cp1, cp2)
		return (cp2.x-cp1.x)*(p.y-cp1.y) > (cp2.y-cp1.y)*(p.x-cp1.x)
	end
	
	local function intersection(cp1, cp2, s, e)
		local dcx, dcy = cp1.x-cp2.x, cp1.y-cp2.y
		local dpx, dpy = s.x-e.x, s.y-e.y
		local n1 = cp1.x*cp2.y - cp1.y*cp2.x
		local n2 = s.x*e.y - s.y*e.x
		local n3 = 1 / (dcx*dpy - dcy*dpx)
		local x = (n1*dpx - n2*dcx) * n3
		local y = (n1*dpy - n2*dcy) * n3
		return {x=x, y=y}
	end
	
	local outputList = subjectPolygon
	local cp1 = clipPolygon[#clipPolygon]
	
	for _, cp2 in ipairs(clipPolygon) do  -- WP clipEdge is cp1,cp2 here
		local inputList = outputList
		outputList = {}
		local s = inputList[#inputList]
		for _, e in ipairs(inputList) do
			if inside(e, cp1, cp2) then
				if not inside(s, cp1, cp2) then
					outputList[#outputList+1] = intersection(cp1, cp2, s, e)
				end
				outputList[#outputList+1] = e
			elseif inside(s, cp1, cp2) then
				outputList[#outputList+1] = intersection(cp1, cp2, s, e)
			end
			s = e
		end
		cp1 = cp2
	end
	
	return outputList, #outputList > 0
end
math.getPolygonIntersection = getPolygonIntersection

--[[
	Products
]]--

--[[
        Calculates the dot product of two lines.
        This function implements the simple form of the dot product calculation: a · b = ax × bx + ay × by
        The lines can be provided in 3 forms:
        
        Parameters:
        	a: {x,y}
        	b: {x,y}
        
        Example:
        	print( dotProduct( {x=10,y=10}, {x=-10,y=10} ) )
        
        Parameters:
        	a: {a,b}
        	b: {a,b}
        
        Example:
                print( dotProduct(
                        { a={x=10,y=10}, b={x=101,y=5} },
                        { a={x=10,y=-10}, b={x=51,y=10} }
                ))
        
		Params:
			lenA: Length A
			lenB: Length B
			deg: Angle between points A and B in degrees
		
		Example:
			print( dotProduct( 23, 10, 90 ) )
		
        Ref:
		http://www.mathsisfun.com/algebra/vectors-dot-product.html
		http://members.tripod.com/c_carleton/dotprod.html/
		http://www.mathsisfun.com/algebra/vector-calculator.html
]]--
local function dotProduct( ... )
	local ax, ax, bx, by
	
	if (#arg == 2 and arg[1].a == nil) then
		-- two vectors - get the vectors
		ax, ay = arg[1].x, arg[1].y
		bx, by = arg[2].x, arg[2].y
		
	elseif (#arg == 2 and a.x == nil) then
		-- two lines - calculate the vectors
		ax = arg[1].b.x - arg[1].a.x
		ay = arg[1].b.y - arg[1].a.y
		bx = arg[2].b.x - arg[2].a.x
		by = arg[2].b.y - arg[2].a.y
		
	elseif (#arg == 3 and type(arg[1]) == "number") then
		-- two lengths and an angle: lenA * lenB * math.cos( deg )
		return arg[1] * arg[2] * math.cos( arg[3] )
		
	elseif (#arg == 4 and type(arg[1]) == "number") then
		-- two lines, params are (x,y,x,y) - get the vectors
		ax, ay = arg[1], arg[2]
		bx, by = arg[3], arg[4]
	end
	
	-- multiply the x's, multiply the y's, then add
	local dot = ax * bx + ay * by
	return dot
end
math.dotProduct = dotProduct

--[[
        Description:
                Calculates the cross product of a vector.
        
        Ref:
                http://www.math.ntnu.no/~stacey/documents/Codea/Library/Vec3.lua
]]--
local function crossProduct( a, b )
        local x, y, z
        x = a.y * (b.z or 0) - (a.z or 0) * b.y
        y = (a.z or 0) * b.x - a.x * (b.z or 0)
        z = a.x * b.y - a.y * b.x
        return { x=x, y=y, z=z }
end
math.crossProduct = crossProduct

--[[
        Description:
                Perform the cross product on two vectors. In 2D this produces a scalar.
        
        Params:
                a: {x,y}
                b: {x,y}
        
        Ref:
                http://www.iforce2d.net/forums/viewtopic.php?f=4&t=79&sid=b9ecd62533361594e321de04b3929d4f
]]--
local function b2CrossVectVect( a, b )
        return a.x * b.y - a.y * b.x;
end
math.b2CrossVectVect = b2CrossVectVect

--[[
        Description:
                Perform the cross product on a vector and a scalar. In 2D this produces a vector.
        
        Params:
                a: {x,y}
                b: float
        
        Ref:
                http://www.iforce2d.net/forums/viewtopic.php?f=4&t=79&sid=b9ecd62533361594e321de04b3929d4f
]]--
local function b2CrossVectFloat( a, s )
        return { x = s * a.y, y = -s * a.x }
end
math.b2CrossVectFloat = b2CrossVectFloat

--[[
        Description:
                Perform the cross product on a scalar and a vector. In 2D this produces a vector.
        
        Params:
                a: float
                b: {x,y}
        
        Ref:
                http://www.iforce2d.net/forums/viewtopic.php?f=4&t=79&sid=b9ecd62533361594e321de04b3929d4f
]]--
local function b2CrossFloatVect( s, a )
        return { x = -s * a.y, y = s * a.x }
end
math.b2CrossFloatVect = b2CrossFloatVect

--[[
	Point Collections
]]--

--[[
	Wraps table index addresses to keep the index value within the valid table index range.
	Eg: Where index -1 and list length 10 function returns 9, true
	Eg: Where index 11 and list length 10 function returns 1, true
	eg: Where index 5 and list is empty function returns 5, false
	
	Parameters:
		index: The table index to be addressed (1-based index)
		list: Table or display group to determine the real index of
	
	Returns:
		Valid index into the table/display group or original index if list has no content
		True if valid index value returned, false if the list/display group is empty
	
	Note:
		An extra small function name has been added 'math.wp' to avoid unsightly code when addressing tables.
]]--
local function wrapIndex( index, list )
	local len = list.numChildren or #list
	if (len == 0) then
		return index, false
	elseif (index > len) then
		return index-len, true
	elseif (index < 1) then
		return len+index, true
	end
	return index, true
end
math.wrapIndex = wrapIndex
math.wp = wrapIndex

-- converts a table of {x,y,x,y,...} to points {x,y}
local function tableToPoints( tbl )
	local pts = {}
	
	for i=1, #tbl-1, 2 do
		pts[#pts+1] = { x=tbl[i], y=tbl[i+1] }
	end
	
	return pts
end
math.tableToPoints = tableToPoints

-- converts a list of points {x,y} to a table of coords {x,y,x,y,...}
local function pointsToTable( pts )
	local tbl = {}
	
	for i=1, #pts do
		tbl[#tbl+1] = pts[i].x
		tbl[#tbl+1] = pts[i].y
	end
	
	return tbl
end
math.pointsToTable = pointsToTable

-- ensures that a list of coordinates is converted to a table of {x,y} points
-- returns a table of {x,y} points, the number of points, whether a list or not
local function ensurePointsTable( tbl )
	if (type(tbl[1]) == "number") then
		-- list contains {x,y,x,y,...} coordinates - convert to table of {x,y} 
		tbl = tableToPoints( tbl )
		return tbl, #tbl, true
	else
		-- table is already in {x,y} point format...
		-- check for display group
		local count = tbl.numChildren
		if (count == nil) then
			count = #tbl
		end
		return tbl, count, false
	end
end
math.ensurePointsTable = ensurePointsTable

-- copies the points from a list of coords, table of points or display group into a new table of {x,y} points
local function copyToPointsTable( points )
	local tbl = {}
	
	local count = points.numChildren
	if (count == nil) then
		count = #points
	end
	
	local isCoords = (type(points[1]) == "number")
	local step = 1
	if (isCoords) then
		step = 2
	end
	
	for i=1, count, step do
		if (isCoords) then
			tbl[#tbl+1] = {x=points[i],y=points[i+1]}
		else
			tbl[#tbl+1] = {x=points[i].x,y=points[i].y}
		end
	end
	
	return tbl
end
math.copyToPointsTable = copyToPointsTable

--[[
	Removes points in found in sequence at the same location.
	Eg: two points both at {x=10,y=21} will be deduped to just one point.
	
	Parameters:
		points: The list of points - can be list of coords but will convert to points on return.
		maxdist: If nil the points are directly compared. If provided, points will be deduped if they are closer than this distance.
	
	Returns:
		List of points where no two points exist at the same location.
	
	Comments:
		Requires the table.lua library file: https://gist.github.com/HoraceBury/9307117
]]--
local function dedupePoints( points, maxdist )
	local count, converted
	
	-- ensure points list not coords
	points, count, converted = math.ensurePointsTable( points )
	
	-- create output list and copy first point
	local pts = {}
	pts[1] = points[1]
	
	-- compare points from second to last against previous
	for i=2, #points do
		if (maxdist) then
			if (math.lengthOf( pts[#pts], points[i] ) > maxdist) then
				pts[#pts+1] = points[i]
			end
		else
			if (not table.compare( pts[#pts], points[i] )) then
				pts[#pts+1] = points[i]
			end
		end
	end
	
	-- compare first and last points
	if (maxdist) then
		if (math.lengthOf( pts[1], pts[#pts] ) <= maxdist) then
			pts[#pts] = nil
		end
	else
		if (table.compare( pts[1], pts[#pts] )) then
			pts[#pts] = nil
		end
	end
	
	if (converted) then
		return pointsToTable( pts )
	else
		return pts
	end
end
math.dedupePoints = dedupePoints

local function generateCurve( easingFunction, xStart, yStart, xEnd, yEnd )
	easingFunction = easingFunction or easing.inOutQuint
	local duration = xEnd-xStart
	local path = {}
	for x=0, duration do
		table.append( path, xStart+x, easingFunction( x, duration, yStart, yEnd-yStart ) )
	end
	return path
end
math.generateCurve = generateCurve
