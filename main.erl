-module(helloworld).

% main functions
-export([partition/5, split/2, separateFiles/3, file_server/1, dir_service/2, start_file_server/1, start_dir_service/0, get/2, create/2, quit/1]).

-import(util, [readFile/1,get_all_lines/1,saveFile/2]).

% can access own ual w/ node()
% can acces own PID w/ self()

% you are free (and encouraged) to create helper functions
% but note that the functions that will be called when
% grading will be those below

%split into 64 character strings
split(Fullfile, List) ->
	Length = string:len(Fullfile),
	if
		Length < 64 -> 
			lists:append(List,[Fullfile]);
		true -> 
			First64 = lists:slice(Fullfile,0,64),
			AppendList = lists:append(List,[First64]),
			split(lists:prefix(Fullfile, First64), AppendList)
	end.

% Map each filename to server
partition(ServerList, FileMap, [HeadString | Rest], Filenames, Index) -> 
	case Filenames of
		[] ->
			FileMap;
		[ Head| Tail ] -> 
			ServersLength = length(ServerList),
			if
				Index > ServersLength ->
					NewMap = maps:put(Head, lists:nth(1,ServerList), FileMap),
					lists:nth(1,ServerList) ! {create_file, HeadString, Head},
					partition(ServerList, NewMap, Rest, Tail, 2);
				true -> 
					NewMap = maps:put(Head, lists:nth(Index,ServerList), FileMap),
					lists:nth(Index,ServerList) ! {create_file, HeadString, Head},
					partition(ServerList, NewMap, Rest, Tail, Index + 1)
			end
	end.

%create filenames from 64 character 
create_files(SplitFile,Filename, OutputList) -> 
	case SplitFile of
		[] ->
			OutputList;
		[Head | Tail] -> 
			NewName = lists:last(string:split(Filename, "/", trailing)),
			NewList = lists:append(OutputList, [NewName ++ "_" ++  (integer_to_list(length(OutputList)) + 1)]),
			create_files(Tail, Filename, NewList)
	end.

%separatesFile into 64 characters each, and maps with Servers
separateFiles(ServerList, FileMap, Filename) -> 
	Fullfile = util:readFile(Filename),
	SplitFile = split(Fullfile, []),
	Filenames = create_files(SplitFile,Filename, []),
	partition(ServerList, FileMap, SplitFile, Filenames, 1).


dir_service(ServerList, FileMap) -> 
	
	receive 
	    {create_fserver, ServerPID} -> UpdatedList = lists:append(ServerList, [ServerPID]), 
	    							    lists:sort(UpdatedList),
	    							    ServerPID ! {make_dir, integer_to_list(length(UpdatedList))},
	    						   	    dir_service(UpdatedList,FileMap);
	    quit -> UpdatedList = lists:map(fun(Fserver) -> {Fserver ! quit} end, ServerList),
	    		dir_service(UpdatedList,FileMap);
	    {create_file, Filename} ->  NewMap = separateFiles(ServerList, FileMap, Filename),
	    							dir_service(ServerList, NewMap)

	end.

file_server(DirUAL) -> 
	receive 
		create -> {direc, DirUAL} ! {create_fserver, self()},
				   file_server(DirUAL);
		{make_dir, Number} -> file:make_dir("./servers/fs" ++ Number),
						      file_server(DirUAL);
		{create_file, Contents, Filename} -> util:saveFile(Filename, Contents),
										     file_server(DirUAL)
	end.

% when starting the Directory Service and File Servers, you will need
% to register a process name and spawn a process in another node

% starts a directory service
start_dir_service() -> 
	KnownServers = [],
	register(direc, spawn(helloworld, dir_service, [KnownServers])).
	% CODE THIS


% starts a file server with the UAL of the Directory Service
%contacts dir service, dir service updates list of known file servers, alphabetically sorted
start_file_server(DirUAL) -> 
	F = spawn(helloworld, file_server, [DirUAL]),
	F ! create.

	% CODE THIS

% requests file information from the Directory Service (DirUAL) on File
% then requests file parts from the locations retrieved from Dir Service
% then combines the file and saves to downloads folder

%
get(DirUAL, File) -> 
	pass.
	% CODE THIS

% gives Directory Service (DirUAL) the name/contents of File to create
% splits file into chunks of 64 characters, sends each chunk to a known file server, following alphabetical
% round robin, for all chunks, dir server updates file meta-data to include where chunks are located
create(DirUAL, File) ->
	DirUAL ! File.
	% CODE THIS

% sends shutdown message to the Directory Service (DirUAL)
% ends all file servers 
quit(DirUAL) ->
	pass.
	% CODE THIS




