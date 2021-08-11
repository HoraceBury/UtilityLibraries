-- table library extensions

if (dump == nil) then
	dump = function(t)
		print("=============")
		for k,v in pairs(t) do
			print("\t",k,v)
		end
		print("=============")
	end
end

-- returns a new table containing the elements of the passed table which the compare function returned true for
-- works with display groups
table.find = function( t, compare )
	local newT = {}
	local count = t.numChildren
	
	if (count == nil) then
		count = #t
	end
	
	for i=1, count do
		local item = t[i]
		if (compare(item)) then
			newT[#newT+1] = item
		end
	end
	
	return newT
end

--[[
	Returns a shallow copy of the input table, starting at index and copying 'count' elements.
	
	Parameters:
		tbl: The table to copy a range of items from.
		index: The first index to start the copy from.
		count: (Optional) The maximum number of items to copy or nil to copy to the end.
	
	Returns:
		A table containing a copy of the items from position index, for the count range size.
	
	Example:
		local original = { 1, 2, 3, 4, 5 }
		local rangecopy = table.range( original, 2, 3 )
		print( table.concat( rangecopy, "," ) )
	
	Example Output:
		2,3,4
]]--
table.range = function( tbl, index, count )
	count = count or #tbl-index+1
	local output = {}
	for i=index, index+count-1 do
		output[#output+1] = tbl[i]
	end
	return output
end

-- returns shallow copy of the table elements from startIndex to finalIndex, wrapping at end->start if necessary
table.portion = function( tbl, startIndex, finalIndex )
	local portion = {}
	while (startIndex ~= finalIndex) do
		portion[#portion+1] = tbl[startIndex]
		
		startIndex = startIndex + 1
		if (startIndex > #tbl) then startIndex=1 end
	end
	portion[#portion+1] = tbl[startIndex]
	return portion
end

--[[
	Returns a range of values from the provided table, wrapping the copy when the end of the table is reached.
	Will also wrap multiple times making this a duplicating function.
	
	Parameters:
		tbl: Table to copy values from
		index: Start of the position to copy values from
		size: Number of values to copy, including the index position
	
	Returns:
		A copy of the tbl parameter table, wrapped at the end so that indices start from 1 when the end of the table is hit.
	
	Example:
		See the test_wraprange() function
]]--
table.wraprange = function( tbl, index, size )
	local out = {}
	while (size > 0) do
		out[#out+1] = tbl[index]
		index = index + 1
		if (index > #tbl) then index=1 end
		size = size - 1
	end
	return out
end

local function test_wraprange()
	local tbl = {1,2,3,4,5,6,7,8,9,10}
	print("(tbl, 1, 10)",table.concat(table.wraprange(tbl, 1, 10),","))
	print("(tbl, 4, 3)",table.concat(table.wraprange(tbl, 4, 3),","))
	print("(tbl, 6, 10)",table.concat(table.wraprange(tbl, 6, 10),","))
	print("(tbl, 6, 20)",table.concat(table.wraprange(tbl, 6, 20),","))
	print("(tbl, 2, 20)",table.concat(table.wraprange(tbl, 2, 20),","))
end
--test_wraprange()

local function wrapindex( tbl, index )
	while (index < 1) do index=index+#tbl end
	while (index > #tbl) do index=index-#tbl end
	return index
end
table.wrapindex = wrapindex

-- splits a table's content into multiple tables (all returned in a single table)
table.split = function( tbl, size )
	local tbls = {}
	for i=1, #tbl, size do
		tbls[#tbls+1] = table.range( tbl, i, size )
	end
	return tbls
end

-- extends table.remove to remove objects directly, without requiring table.indexOf
local _remove = table.remove
table.remove = function( t, pos )
	if (type(pos) == "number") then
		return _remove( t, pos )
	else
		pos = table.indexOf( t, pos )
		return _remove( t, pos )
	end
end

-- replaces entries of old with new
-- returns number of entries replaced
table.replace = function( tbl, old, new )
	local index = table.indexOf( tbl, old )
	local count = 0
	while (index) do
		count = count + 1
		tbl[index] = new
		index = table.indexOf( tbl, old )
	end
	return count
end

-- reverses the order of the items in the table
local function reverse( t )
	local tbl = {}
	for i=#t, 1, -1 do
		tbl[#tbl+1] = t[i]
	end
	return tbl
end
table.reverse = reverse

local function testReverse()
	local t = { 1, 2, 3, 4, 5 }
	dump( table.reverse( t ) )
end
--testReverse()

--[[
	Compares two tables and returns true if they contain the same values, false if not.
	Does not do a deep comparison - only using pairs() function to retrieve keys.
	
	Parameters:
		a: first table to compare
		b: second table to compare
	
	Returns:
		true if both contain the same values
]]--
local function compareTables( a, b )
	for k,v in pairs(a) do
		if (a[k] ~= b[k]) then
			return false
		end
	end
	for k,v in pairs(b) do
		if (b[k] ~= a[k]) then
			return false
		end
	end
	return true
end
table.compareTables = compareTables

--[[
	Converts a table of integer indexed values into a series of named-index tables.
	
	Parameters:
		values: The table of values, eg: {100,200,300,400}
		names: The names of the properties to be assigned, in order, eg: {"x","y"}
	
	Returns:
		A collection of tables with named-index values, eg: { {x=100,y=200}, {x=300,y=400} }
]]--
local function pack( values, names )
	local nameIndex = 1
	local collections = {}
	local tbl = {}
	
	for i=1, #values do
		local name = names[nameIndex]
		
		tbl[name] = values[i]
		
		nameIndex = nameIndex + 1
		if (nameIndex > #names) then
			nameIndex=1
			collections[ #collections+1 ] = tbl
			tbl = {}
		end
	end
	
	return collections
end
table.pack = pack

local function test_pack()
	local tbl = table.pack( {100,200,300,400}, {"x","y"} )
	for i=1, #tbl do
		print("Named table: "..i)
		dump(tbl[i])
	end
end
--test_pack()

--[[
	Converts a table of named values into an integer indexed table.
	
	Parameters:
		values: The table of named values, eg: { {x=100,y=200}, {x=300,y=400} }
		names: The names of the properties to be retrieved, in order, eg: {"x","y"}
	
	Returns:
		A table of integer indexed values, eg: {100,200,300,400}
]]--
local function _unpack( values, names )
	local tbl = {}
	
	for i=1, #values do
		local collection = values[i]
		for n=1, #names do
			local name = names[n]
			tbl[#tbl+1] = collection[name]
		end
	end
	
	return tbl
end
table.unpack = _unpack

local function test_unpack()
	local tbl = table.unpack( { {x=100,y=200,z=900}, {x=300,y=400,z=500} }, {"x","y"} )
	print( require("json").prettify( tbl ) )
end
--test_unpack()

local function equals( a, b )
	if (a == nil or b == nil or #a ~= #b) then
		return false
	end
	
	for i=1, #a do
		if (a[i] ~= b[i]) then
			return false
		end
	end
	
	return true
end
table.equals = equals

local function swap( tbl, aIndex, bIndex )
	local a, b = tbl[aIndex], tbl[bIndex]
	tbl[bIndex] = a
	tbl[aIndex] = b
end
table.swap = swap

local function randomise( tbl )
	for i=1, #tbl do
		table.swap( tbl, i, math.random(1,#tbl) )
	end
end
table.randomise = randomise

local function append( tbl, ... )
	for i=1, #arg do
		tbl[ #tbl+1 ] = arg[i]
	end
	
	
end
table.append = append

--[[
	Returns true if all the arg parameters are contained in the tbl.
	
	Parameters:
		tbl: The table to check within for the values passed in the following parameters
	
	Returns:
		true if all the values were in the table
]]--
local function contains( tbl, ... )
	local count = 0
	for i=1, #tbl do
		for c=1, #arg do
			if (tbl[i] == arg[c]) then
				count = count + 1
			end
		end
	end
	return count >= #arg
end
table.contains = contains

local function testContains()
	local t = {"ball","flag","line"}
	print("Table:", unpack(t))
	print("ball", table.contains(t,"ball"))
	print("flag", table.contains(t,"flag"))
	print("balls", table.contains(t,"balls"))
	print("line", table.contains(t,"line"))
	print("paint", table.contains(t,"paint"))
	print("paint", "line", table.contains(t,"paint","line"))
	print("ball","line", table.contains(t,"ball","line"))
end
--testContains()

--[[
	Provides the ability to search the numerically indexed items within a table by a named property
	of those items.
	
	The fist parameter is provided by the fact that the findByName function is attached to the table
	at initialisation and is later called with the : notation.
	
	When calling the function to find an indexed item, only pass the name of the item property and
	the value to search for.
	
	Parameters:
		name: The name of the property to find the item with.
		value: The value of the named property to find the item by.
	
	Example Code:
		local t = { {n="a", v=1}, {n="b", v=2}, {n="c", v=3} }
		t.findByName = table.findByName
		print("Found: ",t:findByName("n","b").v)
	
	Example Output:
		Found: 2
]]--
local function findByName( self, name, value )
	for i=1, #self do
		local item = self[i]
		if (item[name] == value) then
			return item
		end
	end
end
table.findByName = findByName

--[[
	Similar to table.copy, this function copies all the named indices from the tables passed in
	the parameter list.
	
	Note:
		Any keys which are duplicate among the input tables will be overridden.
]]--
table.replicate = function( ... )
	local tbl = {}
	
	for i=1, #arg do
		for k,v in pairs(arg[i]) do
			tbl[k] = v
		end
	end
	
	return tbl
end
