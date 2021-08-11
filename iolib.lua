-- io operations

local json = require("json")
local lfs = require( "lfs" )

local iolib = {}

--[[ FACILITIES ]]--

local function toDate( timestamp )
	return os.date( "!%Y-%m-%d %H:%M:%S", timestamp )
end
iolib.toDate = toDate

iolib.isdebug = false

--[[ ENUMS ]]--

iolib.stacktype = {
	fifo = 1,
	lifo = 2,
}

--[[ ATTRIBUTES ]]--

local function fileAttributes( filename, base )
	local path = system.pathForFile( filename, base )
	return lfs.attributes( path )
end
iolib.fileAttributes = fileAttributes

-- returns the resource attributes
local function resourceAttributes( filename )
	return fileAttributes( filename, system.ResourceDirectory )
end
iolib.resourceAttributes = resourceAttributes

-- returns true if the document attributes
local function docAttributes( filename )
	return fileAttributes( filename, system.DocumentsDirectory )
end
iolib.docAttributes = docAttributes

-- returns true if the temporary file attributes
local function tempAttributes( filename )
	return fileAttributes( filename, system.TemporaryDirectory )
end
iolib.tempAttributes = tempAttributes

-- returns true if the cached file attributes
local function cacheAttributes( filename )
	return fileAttributes( filename, system.CachesDirectory )
end
iolib.cacheAttributes = cacheAttributes

--[[ EXISTS ]]--

-- returns true if the file exists
local function fileExists( filename, base )
	local path = system.pathForFile( filename, base )
	if (path == nil) then return false end
	local file = io.open( path, "r" )
	if file then
		io.close(file)
	    return true
	end
	return false
end
iolib.fileExists = fileExists

-- returns true if the resource exists
local function resourceExists( filename )
	return fileExists( filename, system.ResourceDirectory )
end
iolib.resourceExists = resourceExists

-- returns true if the document exists
local function docExists( filename )
	return fileExists( filename, system.DocumentsDirectory )
end
iolib.docExists = docExists

-- returns true if the temporary file exists
local function tempExists( filename )
	return fileExists( filename, system.TemporaryDirectory )
end
iolib.tempExists = tempExists

-- returns true if the cached file exists
local function cacheExists( filename )
	return fileExists( filename, system.CachesDirectory )
end
iolib.cacheExists = cacheExists

--[[ DELETE ]]--

-- deletes a file
function removeFile( filename, base )
	local path = system.pathForFile( filename, base )
	if (iolib.isdebug) then print("removeFile:",path) end
	return os.remove( path )
end
iolib.removeFile = removeFile

-- removes file if the resource exists
local function removeResource( filename )
	return removeFile( filename, system.ResourceDirectory )
end
iolib.removeResource = removeResource

-- removes file if the document exists
local function removeDoc( filename )
	return removeFile( filename, system.DocumentsDirectory )
end
iolib.removeDoc = removeDoc

-- removes file if the temporary file exists
local function removeTemp( filename )
	return removeFile( filename, system.TemporaryDirectory )
end
iolib.removeTemp = removeTemp

-- removes file if the cached file exists
local function removeCache( filename )
	return removeFile( filename, system.CachesDirectory )
end
iolib.removeCache = removeCache

--[[ WRITE / READ ]]--

-- will write data to the specified application directory
function write( filename, base, data )
	if (type(data) == "table") then
		data = json.prettify( json.encode( data ) )
	end
	local path = system.pathForFile( filename, base )
	if (iolib.isdebug) then print("Write Path:",path) end
	local file = io.open( path, "w" )
	if (data ~= nil) then
		file:write( tostring( data ) )
		if (iolib.isdebug) then print(tostring(data)) end
	end
	io.close( file )
	file = nil
	return nil
end
iolib.write = write

-- will read data from the specified application directory
function read( filename, base, decode )
	local path = system.pathForFile( filename, base )
	if (iolib.isdebug) then print("Read Path:",path) end
	local file, errStr = io.open( path, "rb" )
	local data = nil
	if (file) then
		data = file:read( "*a" )
		io.close( file )
	end
	file = nil
	if (data ~= nil and decode) then
		data = json.decode( data )
	end
	return data, success
end
iolib.read = read

-- reads from the read-only resource directory
function rResource( filename )
	if (data) then
		return nil
	else
		return read( filename, system.ResourceDirectory )
	end
end
iolib.rResource = rResource

-- writes and reads to/from the permanent documents directory
function wrDocs( filename, data )
	if (data) then
		return write( filename, system.DocumentsDirectory, data )
	else
		return read( filename, system.DocumentsDirectory )
	end
end
iolib.wrDocs = wrDocs

-- writes and reads to/from the short-term temporary directory
function wrTemp( filename, data )
	if (data) then
		return write( filename, system.TemporaryDirectory, data )
	else
		return read( filename, system.TemporaryDirectory )
	end
end
iolib.wrTemp = wrTemp

-- writes and reads to/from the cache directory (longer life than temp)
function wrCache( filename, data )
	if (data) then
		return write( filename, system.CachesDirectory, data )
	else
		return read( filename, system.CachesDirectory )
	end
end
iolib.wrCache = wrCache

--[[ PUT / PULL ]]--

-- put named single value into permanent store, overwriting previous value
local function put( name, value )
	local filename = "putpull"..tostring(name)..".txt"
	wrTemp( filename, tostring(value) )
end
iolib.put = put

-- pull named single value from permanent store, removing the store
local function pull( name )
	local filename = "putpull"..tostring(name)..".txt"
	local value = wrTemp( filename )
	removeTemp( tostring( filename ) )
	return value
end
iolib.pull = pull

--[[ PUSH / POP ]]--

-- pushes an item onto the permanent named list
local function push( listname, stacktype, item )
	local filename = listname..".json"
	local list = wrDocs( filename )
	if (list == nil) then
		list = {}
	else
		list = json.decode( list )
	end
	list[ #list+1 ] = item
	wrDocs( filename, json.encode( list ) )
	return #list
end
iolib.push = push

-- reads an item from the permanent named list and optionally removes it
local function pop( listname, stacktype, isRemove )
	local filename = listname..".json"
	local list = wrDocs( filename )
	if (list == nil) then
		list = {}
	else
		list = json.decode( list )
	end
	local item
	if (#list > 0) then
		if (stacktype == iolib.stacktype.fifo) then
			item = list[1]
			if (isRemove) then
				table.remove( list, 1 )
			end
		elseif (stacktype == iolib.stacktype.lifo) then
			item = list[ #list ]
			if (isRemove) then
				list[ #list ] = nil
			end
		end
		wrDocs( filename, json.encode( list ) )
	end
	return item, #list
end
iolib.pop = pop

--[[ DIRECTORY LISTING ]]--

local function directoryListing( foldername, base, filter )
	-- Get raw path to the app documents directory
	local doc_path = system.pathForFile( foldername or "", base )
	
	local tbl = {}
	
	for file in lfs.dir( doc_path ) do
		-- "file" is the current file or directory name
		if (filter == nil or filter == "" or string.find( file, filter )) then
			if (iolib.isdebug) then print( "Found file: " .. file ) end
			tbl[#tbl+1] = file
		end
	end
	
	return tbl
end

-- returns the resource folder contents
local function resourceListing( foldername, filter )
	return directoryListing( foldername, system.ResourceDirectory, filter )
end
iolib.resourceListing = resourceListing

-- returns true if the document contents
local function docListing( foldername, filter )
	return directoryListing( foldername, system.DocumentsDirectory, filter )
end
iolib.docListing = docListing

-- returns true if the temporary file contents
local function tempListing( foldername, filter )
	return directoryListing( foldername, system.TemporaryDirectory, filter )
end
iolib.tempListing = tempListing

-- returns true if the cached file contents
local function cacheListing( foldername, filter )
	return directoryListing( foldername, system.CachesDirectory, filter )
end
iolib.cacheListing = cacheListing

--[[ CREATE DIRECTORY ]]--

local function createDirectory( foldername, base )
	-- Get raw path to documents directory
	local docs_path = system.pathForFile( "", base )
	
	-- Change current working directory
	local success = lfs.chdir( docs_path ) -- Returns true on success
	
	local new_folder_path
	
	if ( success ) then
		lfs.mkdir( foldername )
		new_folder_path = lfs.currentdir() .. "/" .. foldername
		return true, new_folder_path
	end
	
	return false
end

local function createDocDir( foldername )
	return createDirectory( foldername, system.DocumentsDirectory )
end
iolib.createDocDir = createDocDir

local function createTempDir( foldername )
	return createDirectory( foldername, system.TemporaryDirectory )
end
iolib.createTempDir = createTempDir

return iolib
