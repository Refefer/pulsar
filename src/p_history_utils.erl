-module(p_history_utils).
-export([dict_to_ets/1]).

dict_to_ets(Dict) ->
    Table = ets:new(some_table, []),
    ets:insert(Table, dict:to_list(Dict)),
    Table.

