-module(lpr_reg_ets).
-export([new/1, get/2, put/3, put_if_not_found/3, delete/2, dispose/1]).

new(Opts) ->
    TableName = case maps:get(table_name, Opts, nil) of
                    nil ->
                        UniqueNum = erlang:unique_integer(),
                        UniqueId = erlang:integer_to_list(UniqueNum),
                        TableNameStr = "lpr_reg_ets_table_" ++ UniqueId,
                        erlang:list_to_atom(TableNameStr);
                    V -> V
                end,

    WriteConcurrency = maps:get(write_concurrency, Opts, false),
    ReadConcurrency = maps:get(read_concurrency, Opts, false),
    TableId = ets:new(TableName, [set, {write_concurrency, WriteConcurrency},
                                  {read_concurrency, ReadConcurrency}]),

    {ok, TableId}.

get(TableId, Key) ->
    case ets:lookup(TableId, Key) of
        [] -> {not_found, Key};
        [{_, Value}] -> {ok, Value}
    end.

put(TableId, Key, Val) ->
    true = ets:insert(TableId, {Key, Val}),
    ok.

put_if_not_found(TableId, Key, Val) ->
    case get(TableId, Key) of
        {not_found, _} -> 
            true = ets:insert(TableId, {Key, Val}),
            ok;
        {ok, V} ->
            {error, {found, V}}
    end.

delete(TableId, Key) ->
    true = ets:delete(TableId, Key),
    ok.

dispose(TableId) ->
    true = ets:delete(TableId),
    ok.
