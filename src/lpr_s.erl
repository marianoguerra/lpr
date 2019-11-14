%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(lpr_s).

% http://erlang.org/doc/design_principles/gen_server_concepts.html
-behaviour(gen_server).

% API
-export([stop/1, register/3, register/4, unregister/2,
         find_by_key/2, find_by_key/3,
         registry_count/1]).

-export([start_link/0, start_link/1, start_link/2]).
-export([start_link_local/0, start_link_local/1, start_link_local/2]).

-ignore_xref([start_link/0, start_link/1, start_link/2,
              start_link_local/0, start_link_local/1, start_link_local/2]).

% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {reg_ref, count=0}).
-record(reg_entry, {pid, meta}).
-record(reverse_entry, {key}).

% API

% see: http://erlang.org/doc/man/gen_server.html#start_link-3
start_link_local() ->
    start_link_local(#{}).

start_link_local(Args) ->
    start_link_local(Args, []).

start_link_local(Args, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

start_link() ->
    start_link(#{}).

start_link(Args) ->
    start_link(Args, []).

start_link(Args, Opts) ->
    gen_server:start_link(?MODULE, Args, Opts).

stop(Ref) ->
    gen_server:call(Ref, stop).

register(Ref, Key, Pid) ->
    register(Ref, Key, Pid, undefined).

register(Ref, Key, Pid, Meta) ->
    gen_server:call(Ref, {register, Key, Pid, Meta}).

unregister(Ref, Key) ->
    gen_server:call(Ref, {unregister, Key}).

find_by_key(Ref, Key) ->
    find_by_key(Ref, Key, nil).

find_by_key(Ref, Key, Opts) ->
    gen_server:call(Ref, {find_by_key, Key, Opts}).

registry_count(Ref) ->
    gen_server:call(Ref, registry_count).


% Callbacks

init(Args) ->
    RegArgs = maps:get(reg_args, Args, #{}),
    {ok, RegRef} = lpr_reg_ets:new(RegArgs),
    {ok, #state{reg_ref=RegRef}}.

handle_call(stop, _From, State=#state{reg_ref=RegRef}) ->
    lpr_reg_ets:dispose(RegRef),
    {stop, normal, ok, State};
handle_call({register, Key, Pid, Meta}, _From, State=#state{reg_ref=RegRef,
                                                            count=Count}) ->
    {R, NewCount} = do_register(RegRef, Count, Key, Pid, Meta),
    {reply, R, State#state{count=NewCount}};
handle_call({unregister, Key}, _From,
            State=#state{reg_ref=RegRef, count=Count}) ->
    {R, NewCount} = do_unregister(RegRef, Count, Key),
    {reply, R, State#state{count=NewCount}};
handle_call({find_by_key, Key, Opt}, _From, State=#state{reg_ref=RegRef}) ->
    R = case lpr_reg_ets:get(RegRef, entry_key(Key)) of
            {ok, #reg_entry{pid=Pid, meta=Meta}} ->
                case Opt of
                    with_meta -> {Pid, Meta};
                    _ -> Pid
                end;
            {not_found, _} -> undefined
        end,
    {reply, R, State};
handle_call(registry_count, _From, State=#state{count=Count}) ->
    {reply, Count, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'DOWN', _MonitorRef, process, Pid, _Info},
            State=#state{reg_ref=RegRef, count=Count}) ->
    ReverseKey = reverse_key(Pid),
    NewCount = case lpr_reg_ets:get(RegRef, ReverseKey) of
                   {ok, #reverse_entry{key=Key}} ->
                       ok = lpr_reg_ets:delete(RegRef, entry_key(Key)),
                       ok = lpr_reg_ets:delete(RegRef, ReverseKey),
                       Count - 1;

                   {not_found, _} -> Count
               end,
    {noreply, State#state{count=NewCount}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

entry_key(Key) -> {reg_entry_key, Key}.
reverse_key(Pid) -> {reg_entry_key_reverse, Pid}.

do_register(RegRef, Count, Key, Pid, Meta) ->
    case lpr_reg_ets:get(RegRef, reverse_key(Pid)) of
        {ok, #reverse_entry{key=_Key}} ->
            {{error, pid_already_registered}, Count};

        {not_found, _} ->
            RegEntry = #reg_entry{pid=Pid, meta=Meta},
            PutRes = lpr_reg_ets:put_if_not_found(RegRef,
                                                  entry_key(Key),
                                                  RegEntry),
            case PutRes of
                ok ->
                    erlang:monitor(process, Pid),
                    ReverseEntry = #reverse_entry{key=Key},
                    lpr_reg_ets:put_if_not_found(RegRef, reverse_key(Pid),
                                                 ReverseEntry),
                    {ok, Count + 1};
                {error, {found, _}} ->
                    {{error, taken}, Count}
            end
    end.

do_unregister(RegRef, Count, Key) ->
    EntryKey = entry_key(Key),
    case lpr_reg_ets:get(RegRef, EntryKey) of
        {ok, #reg_entry{pid=Pid}} ->
            ok = lpr_reg_ets:delete(RegRef, EntryKey),
            ok = lpr_reg_ets:delete(RegRef, reverse_key(Pid)),
            {ok, Count - 1};
        {not_found, _} ->
            {{error, undefined}, Count}
    end.
