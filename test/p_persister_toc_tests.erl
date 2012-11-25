-module(p_persister_toc_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TIMESTAMPS, [{2012, 10, 11, 12, 5},
                     {2012, 10, 11, 12, 6},
                     {2012, 10, 11, 12, 7},
                     {2012, 10, 11, 12, 8},
                     {2012, 10, 11, 12, 9},
                     {2012, 10, 11, 12, 10}]).

-define(MISSING_TMSP, {2012, 10, 11, 12, 11}).

-define(TEST_FILE, "/tmp/p_persister_toc_test").

get_test_file() ->
    file:delete(?TEST_FILE),
    p_persister_toc:open(?TEST_FILE).

setup() ->
    {Toc, _Count} = lists:foldl(fun(Tmsp, {Toc, Count}) ->
        {Toc, ok} = p_persister_toc:store(Toc, Tmsp, Count),
        {Toc, Count+1}
    end, {get_test_file(), 1},?TIMESTAMPS),
    Toc.

new_test() ->
    Toc = get_test_file(),
    Keys = p_persister_toc:keys(Toc),
    ?assert(length(Keys) == 0).

store_test() ->
    Toc = setup(),
    Keys = p_persister_toc:keys(Toc),
    ?assert(lists:sort(Keys) == ?TIMESTAMPS).

lookup_test() ->
    Toc = setup(),
    [Tmsp1, Tmsp2| _Rest] = ?TIMESTAMPS,
    {ok, Count1} = p_persister_toc:lookup(Toc,Tmsp1),
    {ok, Count2} = p_persister_toc:lookup(Toc, Tmsp2),
    ?assert(Count1 == 1),
    ?assert(Count2 == 2),
    {error, missing} = p_persister_toc:lookup(Toc, ?MISSING_TMSP).

delete_test() ->
    Toc = setup(),
    [Tmsp1|Rest] = ?TIMESTAMPS,
    {ok, Toc} = p_persister_toc:delete(Toc, Tmsp1),
    Keys = p_persister_toc:keys(Toc),
    ?assert(lists:sort(Keys) == Rest).

between_test() ->
    Toc = setup(),
    [Tmsp1, Tmsp2, Tmsp3|Rest] = ?TIMESTAMPS,

    % Test simple inclusive range
    Tree = p_persister_toc:between(Toc, Tmsp1, Tmsp3),
    ?assert(gb_trees:to_list(Tree) == lists:zip([Tmsp1,Tmsp2,Tmsp3], [1,2,3])),

    % Check Overlapping right
    Tree2 = p_persister_toc:between(Toc, Tmsp3, ?MISSING_TMSP),
    Seq = lists:zip([Tmsp3|Rest], lists:seq(3, length(Rest)+3)),
    ?assertEqual(gb_trees:to_list(Tree2), Seq).
