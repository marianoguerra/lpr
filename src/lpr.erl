-module(lpr).
-export([start_link/0, stop/1]).
-export([register/3, register/4, unregister/2, find_by_key/2, find_by_key/3,
         registry_count/1]).

start_link() ->
    lpr_s:start_link().

stop(Pid) ->
    lpr_s:stop(Pid).

register(Ref, Key, Pid) ->
    lpr_s:register(Ref, Key, Pid).

register(Ref, Key, Pid, Meta) ->
    lpr_s:register(Ref, Key, Pid, Meta).

unregister(Ref, Key) ->
    lpr_s:unregister(Ref, Key).

find_by_key(Ref, Key) ->
    lpr_s:find_by_key(Ref, Key).

find_by_key(Ref, Key, Opts) ->
    lpr_s:find_by_key(Ref, Key, Opts).

registry_count(Ref) ->
    lpr_s:registry_count(Ref).
