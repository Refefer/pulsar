-module(p_persister_dets).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {directory,
                site,
                table,
                toc,
                max_history,
                current_minute=gb_trees:empty()}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         store_table/4,
         merge_dets/2,
         consolidated/4,
         query_between/4]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Props) ->
    gen_server:start_link(?MODULE, Props, []).

store_table(Server, Site, Timestamp, Table) ->
    gen_server:cast(Server, {store, Site, Timestamp, Table}).

consolidated(Server, OldMinute, Filename, OldTimestamps) ->
    gen_server:cast(Server, {consolidated, OldMinute, Filename, OldTimestamps}).

query_between(Server, Key, Start, End) ->
    % Get Tables between Start and End
    {ok, Timestamps} = gen_server:call(Server, {get_timestamps, norm_to_min(Start), norm_to_min(End)}),
    {ok, search_key(Server, Key, Timestamps)}.

query_key(Server, Timestamp, Key) ->
    gen_server:call(Server, {query_key, Timestamp, Key}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Props) ->
    Dir = proplists:get_value(directory, Props, "/tmp"),
    Site = proplists:get_value(site, Props, <<"undefined">>),
    Storage = generate_site_dir(Dir, Site),
    Toc = get_toc(Storage),

    % Max History is represented in minutes.
    History = proplists:get_value(max_history, Props, infinity),

    % Figure out if we need to expire anything
    expire_toc(Toc, History),

    p_stat_utils:register_persistence_server(Site, self()),
    {ok, #state{directory=Storage, site=Site, toc=Toc, max_history=History}}.

handle_call({get_timestamps, Start, End}, _From, #state{toc=Toc} = State) ->
    % Get the tables that exist between the two times
    Tables = p_persister_toc:between(Toc, Start, End),
    {reply, {ok, gb_trees:keys(Tables)}, State};

handle_call({query_key, Timestamp, Key}, _From, #state{toc=Toc} = State) ->
    R = case p_persister_toc:lookup(Toc, Timestamp) of
        {ok, Filename} ->
            {ok, D} = dets:open_file(Filename) ,
            try get_values_from_table(D, Key)
            catch
                error:_Reason ->
                    dict:new()
            end;
        {error, missing} ->
            dict:new()
    end,
    {reply, {ok, Timestamp, R}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({store, _Site, Timestamp, Table}, #state{directory=Dir, toc=Toc, current_minute=CurMinAcc} = State) ->
    case store_ets(Dir, Timestamp, Table) of
        {ok, Filename} ->
            % Store it with second granularity
            p_persister_toc:store(Toc, Timestamp, Filename),
            case consolidate_history(CurMinAcc, Timestamp, Dir) of
                no_change ->
                    % Didn't consolidate, add the key and move on
                    NewMinAcc = gb_trees:insert(Timestamp, Filename, CurMinAcc),
                    {noreply, State#state{current_minute=NewMinAcc}};

                consolidating ->
                    % Consolidating the tables, start a new set and move along
                    NewMinAcc = gb_trees:insert(Timestamp, Filename, gb_trees:empty()),
                    {noreply, State#state{current_minute=NewMinAcc}}
            end;

        {error, _Reason} ->
            % Probably should complain if we can't save
            {noreply, State}
    end;

handle_cast({consolidated, Timestamp, DFilename, OldTimestamps}, #state{toc=Toc, max_history=Hist} = State) ->

    % Update toc.
    p_persister_toc:store(Toc, Timestamp, DFilename),

    % Make a note of when to expire
    case Hist of
        infinity -> ok;
        Hist -> erlang:send_after(Hist*60000, self(), {expire, Timestamp})
    end,

    % Build the tree for deleting
    DeleteTree = lists:foldl(fun(Tmsp, Tree) ->
        {ok, Filename} = p_persister_toc:lookup(Toc, Tmsp),
        gb_trees:insert(Tmsp, Filename, Tree)
    end, gb_trees:empty(), OldTimestamps),

    % Clean up.
    purge_old_tables(Toc, DeleteTree),
    {noreply, State#state{toc=Toc}};

handle_cast(_Msg, State) ->
    {noreply, State}.

% Expire an old record
handle_info({expire, Timestamp}, #state{toc=Toc} = State) ->
    {ok, Filename} = p_persister_toc:lookup(Toc, Timestamp),
    purge_old_tables(Toc, gb_trees:insert(Timestamp, Filename, gb_trees:empty())),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
time_to_minute({{Year, Month, Day},{Hour, Minute, _Second}}) ->
    {Year, Month, Day, Hour, Minute}.

time_to_string({{Year, Month, Day},{Hour, Minute, Second}}) ->
    io_lib:format("~p-~2..0B-~2..0B_~2..0B:~2..0B:~2..0B", [Year, Month, Day, Hour, Minute, Second]);
time_to_string({Year, Month, Day, Hour, Minute}) ->
    io_lib:format("~p-~2..0B-~2..0B_~2..0B:~2..0B", [Year, Month, Day, Hour, Minute]).

generate_site_dir(Directory, Site) ->
    CleanSite = binary:replace(Site, <<"/">>, <<"_">>),
    filename:join([Directory, CleanSite]).

generate_filename(Directory, Timestamp) ->
    filename:join([Directory, time_to_string(Timestamp)]).

get_toc(StorageDir) ->
    Path = filename:join([StorageDir, "toc.dets"]),
    filelib:ensure_dir(Path),
    p_persister_toc:open(Path).

% Attempts ot consolidate history
consolidate_history(CurMinAcc, Timestamp, Dir) ->
    case gb_trees:size(CurMinAcc) of
        0 ->
            % If nothing in the tree, nothing to do.
            no_change;
        _Other ->
            % Check if the current minute is up
            CurMinute = time_to_minute(Timestamp),
            {TreeTmsp, _Value} =  gb_trees:smallest(CurMinAcc),
            case time_to_minute(TreeTmsp) of
                CurMinute ->
                    % Time hasn't changed, move along.
                    no_change;
                OldMinute ->
                    % We are going to consolidate the table
                    Server = self(),
                    spawn(fun() ->
                        consolidate_tables(Server, CurMinAcc, OldMinute, Dir)
                    end),
                    consolidating
            end
    end.

% Takes a set of dets and converts it into a single dict
merge_dets(Tables, Dict) ->
    merge_dets(0, Tables, Dict).
merge_dets(_PadAmount, [], Dict) ->
    Dict;
merge_dets(PadAmount, [Table|Rest], Dict) ->
    Padding = [0 || _ <- lists:seq(1, PadAmount)],
    % Build a dictionary, assuming that keys are not
    % in the current dict.
    ToMerge = dets:foldl(fun({Key, Value}, Acc) ->
        % Pad out the Values to the given length
        dict:append_list(Key, [Value|Padding], Acc)
    end, dict:new(), Table),

    % Merge the dicts together, resolving collisions.
    Merged = dict:merge(fun(_Key, V1, [Value|_V2]) ->
        [Value|fill_list(PadAmount, V1)]
    end, Dict, ToMerge),
    merge_dets(PadAmount + 1, Rest, Merged).

% Fills a partially filled list with zeros, up to amount
fill_list(Amount, List) ->
    case length(List) of
        Amount ->
            List;
        LLength ->
            [0 || _ <- lists:seq(1, Amount - LLength)] ++ List
    end.

store_ets(Dir, Timestamp, Table) ->
    Filename = generate_filename(Dir, Timestamp),
    filelib:ensure_dir(Filename),
    % Store the running tally
    case dets:open_file(Filename, [{ram_file, true},{min_no_slots, ets:info(Table, size)}]) of
        {ok, Dets} ->
            to_dets(Table, Dets),
            dets:insert_new(Dets, {timestamp, Timestamp}),
            dets:close(Dets),
            {ok, Filename};
        {error, Reason} ->
            {error, Reason}
    end.

purge_old_tables(Toc, Tables) ->
    % Delete old keys
    lists:foreach(fun(Tmsp) ->
        p_persister_toc:delete(Toc, Tmsp)
    end, gb_trees:keys(Tables)),

    % Delete the dets tables
    lists:foreach(fun(Filename) ->
        file:delete(Filename)
    end, gb_trees:values(Tables)).

to_dets(Table, Dets) ->
    case ets:info(Table, owner) of
        null ->
            % This is disabled since there appears to be a bug
            % in dets when converting large tables 
            dets:from_ets(Dets, Table);
        _Other ->
            % We don't own it, so we have to iterate over it
            dets:delete_all_objects(Dets),
            ets:foldl(fun({Key, Value}, DTable) ->
                dets:insert(DTable, {Key, Value}),
                DTable
            end, Dets, Table)
    end.

% Expires TOC items
expire_toc(Toc, infinity) ->
    {ok, Toc};
expire_toc(Toc, MaxHistory) ->
    Keys = p_persister_toc:keys(Toc),
    Now = erlang:localtime(),
    Self = self(),
    MaxSeconds = MaxHistory * 60,
    lists:foreach(fun(Time) ->
        Diff = time_diff_to_seconds(calendar:time_difference(norm_time(Time), Now)),
        Delta = MaxSeconds - Diff,
        io:format("TTL ~p: ~p~n", [Time, Delta]),
        erlang:send_after(max(Delta, 0)*1000, Self, {expire, Time})
    end, Keys).

norm_time({Year, Month, Day, Hour, Minute}) ->
    {{Year, Month, Day}, {Hour, Minute, 0}};

norm_time(Other) ->
    Other.

time_diff_to_seconds({Days, {Hours, Minutes, Seconds}}) ->
    Days * 24 * 3600 + Hours * 3600 + Minutes * 60 + Seconds.

% Loops through a set of Timestamps, querying for the key
search_key(Server, Key, Timestamps) ->
    search_key(Server, Key, Timestamps, []).
search_key(_Server, _Key, [], Acc) ->
    % Merge the dicts together into a gb_trees
    lists:foldl(fun(Dict1, Tree) ->
        % Insert each item into the tree
        lists:foldl(fun({Timestamp, Items}, TreeAcc) ->
            gb_trees:insert(Timestamp, Items, TreeAcc)
        end, Tree, dict:to_list(Dict1))
    end, gb_trees:empty(), Acc);
search_key(Server, Key, [Tmsp|Rest], Acc) ->
    % Check the cache first
    CacheKey = {?MODULE, Tmsp, Key},
    Results = case simple_cache:lookup(CacheKey) of
        {ok, R} ->
            R;
        {error, missing} ->
            {ok, Tmsp, R} = query_key(Server, Tmsp, Key),
            % We add random variance to prevent stampeding herd
            simple_cache:set(CacheKey, R, 180 + random:uniform(180)),
            R
    end,
    search_key(Server, Key, Rest, [Results|Acc]).

% Given a compact table, looks for a key
get_values_from_table(Table, Key) ->
    % Create a dict for timestamps
    [[Timestamps]] = dets:match(Table, {timestamp, '$1'}),
    Dict = dict:from_list(lists:map(fun(T) -> {T, []} end, Timestamps)),

    % For each key/value found, append to the end of the dict
    KeyValues = dets:match(Table, {{Key, '$1'}, '$2'}),
    lists:foldl(fun([Value, Counts], Acc) ->
        % Join the two lists and add them to their personal dicts 
        Zipped = lists:zip(lists:sublist(Timestamps, length(Counts)), Counts),
        lists:foldl(fun({Timestamp, Count}, D) ->
            case Count of
                0 ->
                    D;
                _NotZero ->
                    dict:append(Timestamp, {Value,Count}, D)
            end
        end, Acc, Zipped)
    end, Dict, KeyValues).

norm_to_min({{Year, Month, Day}, {Hour, Minute, _Second}}) ->
    {Year, Month, Day, Hour, Minute};
norm_to_min(Other) ->
    Other.

% Consolidates a set of individual det tables into a single Dets table
consolidate_tables(Server, TableSet, OldMinute, Dir) ->
    % Current minute is up, consolidate the dets tables.
    Tables = lists:map(fun(Filename) ->
        {ok, D} = dets:open_file(Filename),
        D
    end, gb_trees:values(TableSet)),

    % Merge the dets tables
    Dict = merge_dets(Tables, dict:new()),

    % Convert it to ets table
    Ets = ets:new(consolidated, []),
    ets:insert(Ets, dict:to_list(Dict)),

    % Clean up
    lists:foreach(fun(Table) ->
        dets:close(Table)
    end, Tables),

    % Store the consolidated table as dets
    {ok, DFilename} = store_ets(Dir, OldMinute, Ets),

    % Delete the ets table 
    ets:delete(Ets),

    % Tell the server we have finished consolidating
    consolidated(Server, OldMinute, DFilename, gb_trees:keys(TableSet)).

