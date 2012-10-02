-module(p_persister_toc).
-export([open/1, lookup/2, store/3, delete/2]).
-record(toc, {dets}).

open(FileName) ->
    {ok, Dets} = dets:open_file(FileName, []),
    #toc{dets=Dets}.

lookup(#toc{dets=Dets}=Toc, Timestamp) ->
    [{Timestamp, Value}] = dets:lookup(Dets, Timestamp),
    {Toc, Value}.

store(#toc{dets=Dets}=Toc, Timestamp, File) ->
    case dets:insert(Dets, {Timestamp, File}) of
        ok ->
            dets:sync(Dets),
            {Toc, ok};
        {error, Reason} ->
            {Toc, Reason}
    end.

delete(#toc{dets=Dets}=Toc, Timestamp) ->
    dets:delete(Dets, Timestamp),
    dets:sync(Dets),
    {ok, Toc}.
