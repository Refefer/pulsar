-module(p_persister_toc).
-export([open/1, lookup/2, store/3, delete/2, keys/1, between/3]).

-include_lib("stdlib/include/qlc.hrl").

-record(toc, {dets}).
-record(tmsp_idx, {tmsp, filename}).

open(FileName) ->
    {ok, Dets} = dets:open_file(FileName, [{keypos, 2}]),
    #toc{dets=Dets}.

lookup(#toc{dets=Dets}=Toc, Timestamp) ->
    %[{Timestamp, Value}] = dets:lookup(Dets, #tmsp_idx{idx=Timestamp}),
    [[Filename]] = dets:match(Dets, #tmsp_idx{tmsp=Timestamp, filename='$1'}),
    {Toc, Filename}.

store(#toc{dets=Dets}=Toc, Timestamp, File) ->
    case dets:insert(Dets, #tmsp_idx{tmsp=Timestamp, filename=File}) of
        ok ->
            dets:sync(Dets),
            {Toc, ok};
        {error, Reason} ->
            {Toc, Reason}
    end.

delete(#toc{dets=Dets}=Toc, Timestamp) ->
    %dets:delete(Dets, Timestamp),
    dets:match_delete(Dets, #tmsp_idx{tmsp=Timestamp, filename='_'}),
    dets:sync(Dets),
    {ok, Toc}.

keys(#toc{dets=Dets} = Toc) ->
    Keys = lists:flatten(dets:match(Dets, #tmsp_idx{tmsp='$1', filename='_'})),
    {Toc, Keys}.

between(#toc{dets=Dets}=Toc, Start, End) ->
    NStart = normalize_time(Start),
    NEnd = normalize_time(End),
    Q = qlc:q([{T, F} || #tmsp_idx{tmsp=T, filename=F} <- dets:table(Dets), T >= NStart, T =< NEnd]),
    Tree = lists:foldl(fun({K, V}, Tree) ->
        gb_trees:insert(K, V, Tree)
    end, gb_trees:empty(), qlc:e(Q)),
    {Toc, Tree}.

normalize_time({{Year, Month, Day}, {Hour, Minute, _Second}}) ->
    {Year, Month, Day, Hour, Minute};
normalize_time({Year, Month, Day, Hour, Minute}) ->
    {Year, Month, Day, Hour, Minute}.

