-module(helloworld).

% main functions
-export([client/1, combineFiles/2, getServers/4, partition/5, split/2, separateFiles/3, file_server/2, dir_service/2, start_file_server/1, start_dir_service/0, get/2, create/2, quit/1]).

-import(util, [readFile/1,get_all_lines/1,saveFile/2]).

% can access own ual w/ node()
% can acces own PID w/ self()

% you are free (and encouraged) to create helper functions
% but note that the functions that will be called when
% grading will be those below

%split into 64 character strings

split(Fullfile, List) ->
	Length = length(Fullfile),
	if
		Length < 64 -> 
			lists:append(List,[Fullfile]);
		true -> 
			First64 = string:slice(Fullfile,0,64),
			AppendList = lists:append(List,[First64]),
			split(lists:prefix(Fullfile, First64), AppendList)
	end.

% Map each filename to server
partition(ServerList, FileMap, SplitFile, Filenames, Index) -> 
	case {Filenames} of
		[] ->
			FileMap;
		[ Head| Tail ] -> 
			if
				Index > length(ServerList) ->
					NewMap = maps:put(Head, lists:nth(1,ServerList), FileMap),
					[HeadString | Rest] = SplitFile,
					lists:nth(1,ServerList) ! {create_file, HeadString, Head},
					partition(ServerList, NewMap, Rest, Tail, 2);
				true -> 
					NewMap = maps:put(Head, lists:nth(Index,ServerList), FileMap),
					[HeadString | Rest] = SplitFile,
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
			OnlyFileName =  lists:last(string:split(Filename, "/", trailing)),
			WithoutTxt = lists:nth(1, string:split(OnlyFileName, ".", trailing)),
			NewList = lists:append(OutputList, [WithoutTxt ++ "_" ++  (integer_to_list(length(OutputList) + 1))]),
			create_files(Tail, Filename, NewList)
	end.

%separatesFile into 64 characters each, and maps with Servers
separateFiles(ServerList, FileMap, Filename) -> 
	Fullfile = util:readFile("./input/" ++ Filename),
	SplitFile = split(Fullfile, []),
	Filenames = create_files(SplitFile,Filename, []),
	partition(ServerList, FileMap, SplitFile, Filenames, 1).


getServers(Filepattern, FileMap, Servers, Index) -> 
	Filename = Filepattern ++ integer_to_list(Index),
	MatchedServer = maps:get(Filename, FileMap, "CANT_FIND"),
	case MatchedServer of
		"CANT_FIND" -> 
			Servers;
		X -> 
			NewServers = lists:append(Servers, [{X, Filename}]),
			getServers(Filepattern, FileMap, NewServers, Index + 1)
	end.


dir_service(ServerList, FileMap) -> 
	
	receive 
	    {create_fserver, ServerPID} -> UpdatedList = lists:append(ServerList, [ServerPID]), 
	    							    lists:sort(UpdatedList),
	    							    ServerPID ! {make_dir, integer_to_list(length(UpdatedList))},
	    						   	    dir_service(UpdatedList,FileMap);
	    quit -> UpdatedList = lists:map(fun(Fserver) -> {Fserver ! quit} end, ServerList),
	    		dir_service(UpdatedList,FileMap);
	    {create_file, Filename} ->  NewMap = separateFiles(ServerList, FileMap, Filename),
	    							dir_service(ServerList, NewMap);
	    {get_file, Filename, ClientPID} -> WithoutTxt = lists:nth(1, string:split(Filename, ".", trailing)),
								ClientPID ! {receive_servers, getServers(WithoutTxt, FileMap, [], 1)},
								dir_service(ServerList, FileMap)
	end.

file_server(DirUAL, Path) -> 
	receive 
		{make_dir, Number} -> file:make_dir("./servers/fs" ++ Number),
							  NewPath = "./servers/fs" ++ Number ++ "/",
						      file_server(DirUAL,NewPath);
		{create_file, Contents, Filename} -> util:saveFile(Path ++ Filename ++ ".txt", Contents),
										     file_server(DirUAL, Path);
		{get_file, Filename, ClientPID} -> StringContent = util:readFile(Path ++ Filename ++ ".txt"),
										   ClientPID ! {retrieve_content, StringContent}
	end.

% when starting the Directory Service and File Servers, you will need
% to register a process name and spawn a process in another node

% starts a directory service
start_dir_service() -> 
	register(direc, spawn(helloworld, dir_service, [ [] ,maps:new()])).
	% CODE THIS


% starts a file server with the UAL of the Directory Service
%contacts dir service, dir service updates list of known file servers, alphabetically sorted
start_file_server(DirUAL) -> 
	F = spawn(helloworld, file_server, [DirUAL, ""]),
	{direc, DirUAL} ! {create_fserver, F}.

	% CODE THIS

% requests file information from the Directory Service (DirUAL) on File
% then requests file parts from the locations retrieved from Dir Service
% then combines the file and saves to downloads folder

combineFiles(StringContent, OutputString) ->
	case StringContent of
		[] ->
			OutputString;
		[Head | Tail] ->
			NewOutput = OutputString ++ Head,
			combineFiles(Tail, NewOutput)
	end.

contact_servers(ServersPlusFile) -> 
	case ServersPlusFile of
		[] ->
			nothing;
		[{Servers, Filename} | Tail] ->
			Servers ! {get_file, Filename},
			contact_servers(Tail)
	end.

	
client(Content) ->
	receive 
		{get_file, File, DirUAL} -> {direc, DirUAL} ! {get_file, File, self()},
									client(Content);
		{receive_servers, ServersPlusFile} ->  contact_servers(ServersPlusFile),
											   client(Content);
		{retrieve_content, StringContent} -> NewContent = Content ++ [StringContent], 
											 client(NewContent);
		download_file -> FullString = combineFiles(Content, [])

	end.



%
get(DirUAL, File) -> 
	OnlyFileName =  lists:last(string:split(File, "/", trailing)),
	Client = spawn(helloworld, client, []),
	Client ! {get_file, File, DirUAL},
	Client ! {download_file}.
	
	%FullString = combineFiles(ServersPlusFile, ""),
	%util:saveFile("./downloads/" ++ OnlyFileName).

	% CODE THIS

% gives Directory Service (DirUAL) the name/contents of File to create
% splits file into chunks of 64 characters, sends each chunk to a known file server, following alphabetical
% round robin, for all chunks, dir server updates file meta-data to include where chunks are located
create(DirUAL, File) ->
	{direc, DirUAL} ! {create_file, File}.
	% CODE THIS

% sends shutdown message to the Directory Service (DirUAL)
% ends all file servers 
quit(DirUAL) ->
	pass.
	% CODE THIS




