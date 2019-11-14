-module(lpr_SUITE).
-compile(export_all).

all() -> [smoke_test, not_found, register_and_find,
          register_and_find_with_undefined_meta, register_and_find_with_meta,
          register_taken,
          unregister_find_not_found,
          register_same_pid_two_keys_error,
          register_unregister_register,
          unregister_not_found_doesnt_decrease_count,
          remove_dead_pids].

smoke_test(_) ->
    {ok, Ref} = lpr:start_link(),
    ok = lpr:stop(Ref).

not_found(_) ->
    {ok, Ref} = lpr:start_link(),
    undefined = lpr:find_by_key(Ref, key1),
    ok = lpr:stop(Ref).

register_and_find(_) ->
    {ok, Ref} = lpr:start_link(),
    Pid = self(),
    ok = lpr:register(Ref, key1, Pid),
    Pid = lpr:find_by_key(Ref, key1),
    ok = lpr:stop(Ref).

register_taken(_) ->
    {ok, Ref} = lpr:start_link(),
    Pid = self(),
    Pid1 = spawn(fun() -> receive _ -> ok end end),
    ok = lpr:register(Ref, key1, Pid),
    {error, taken} = lpr:register(Ref, key1, Pid1),
    Pid = lpr:find_by_key(Ref, key1),
    Pid1 ! die,
    ok = lpr:stop(Ref).

register_and_find_with_meta(_) ->
    {ok, Ref} = lpr:start_link(),
    Pid = self(),
    Meta = my_meta,
    ok = lpr:register(Ref, key1, Pid, Meta),
    {Pid, Meta} = lpr:find_by_key(Ref, key1, with_meta),
    ok = lpr:stop(Ref).

register_and_find_with_undefined_meta(_) ->
    {ok, Ref} = lpr:start_link(),
    Pid = self(),
    ok = lpr:register(Ref, key1, Pid),
    {Pid, undefined} = lpr:find_by_key(Ref, key1, with_meta),
    ok = lpr:stop(Ref).

unregister_find_not_found(_) ->
    {ok, Ref} = lpr:start_link(),
    Pid = self(),
    ok = lpr:register(Ref, key1, Pid),
    1 = lpr:registry_count(Ref),
    ok = lpr:unregister(Ref, key1),
    undefined = lpr:find_by_key(Ref, key1),
    0 = lpr:registry_count(Ref),
    ok = lpr:stop(Ref).

register_same_pid_two_keys_error(_) ->
    {ok, Ref} = lpr:start_link(),
    Pid = self(),
    ok = lpr:register(Ref, key1, Pid),
    {error, pid_already_registered} = lpr:register(Ref, key2, Pid),
    Pid = lpr:find_by_key(Ref, key1),
    undefined = lpr:find_by_key(Ref, key2),
    ok = lpr:stop(Ref).

unregister_not_found_doesnt_decrease_count(_) ->
    {ok, Ref} = lpr:start_link(),
    {error, undefined} = lpr:unregister(Ref, key1),
    0 = lpr:registry_count(Ref),
    ok = lpr:stop(Ref).

register_unregister_register(_) ->
    {ok, Ref} = lpr:start_link(),
    Pid = self(),
    ok = lpr:register(Ref, key1, Pid),
    1 = lpr:registry_count(Ref),
    ok = lpr:unregister(Ref, key1),
    undefined = lpr:find_by_key(Ref, key1),
    0 = lpr:registry_count(Ref),
    ok = lpr:register(Ref, key1, Pid),
    1 = lpr:registry_count(Ref),
    ok = lpr:stop(Ref).

remove_dead_pids(_) ->
    {ok, Ref} = lpr:start_link(),
    Pid = spawn(fun() -> receive _ -> ok end end),
    ok = lpr:register(Ref, key1, Pid),
    1 = lpr:registry_count(Ref),
    Pid ! die,
    ct:sleep(100),
    0 = lpr:registry_count(Ref),
    ok = lpr:stop(Ref).
