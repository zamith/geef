%%%-------------------------------------------------------------------
%%% @author Carlos Martin Nieto <cmn@dwim.me>
%%% @copyright (C) 2013, Carlos Martin Nieto
%%% @doc
%%%
%%% @end
%%% Created : 23 May 2013 by Carlos Martin Nieto <cmn@dwim.me>
%%%-------------------------------------------------------------------
-module(geef_odb).

-behaviour(gen_server).

%% API
-export([start_link/1, exists/2, write/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("geef_records.hrl").

-record(state, {handle}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Starts the server
-spec start_link(term()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Handle) ->
    gen_server:start_link(?MODULE, Handle, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init(Handle) ->
    {ok, #state{handle=Handle}}.

%% @private
handle_call({exists, Oid}, _From, State = #state{handle=Handle}) ->
    Reply = geef_nif:odb_object_exists(Handle, Oid),
    {reply, Reply, State};

handle_call({write, Content, Type}, _From, State = #state{handle=Handle}) ->
    Reply = case geef_nif:odb_write(Handle, Content, Type) of
		{ok, Oid} ->
		    {ok, #geef_oid{oid=Oid}};
		Other ->
		    Other
	    end,
    {reply, Reply, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.


%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec exists(pid(), geef_oid() | iolist()) -> boolean().
exists(Pid, #geef_oid{oid=Oid}) ->
    gen_server:call(Pid, {exists, Oid});
exists(Pid, Sha) ->
    #geef_oid{oid=Oid} = geef_oid:parse(Sha),
    gen_server:call(Pid, {exists, Oid}).

-spec write(pid(), iolist(), atom()) -> {ok, geef_oid()} | {error, term()}.
write(Pid, Contents, Type) ->
    gen_server:call(Pid, {write, Contents, Type}).

-ifdef(TEST).

exists_test() ->
    {ok, Repo} = geef_repo:open(".."),
    {ok, Odb} = geef_repo:odb(Repo),
    ?assert(exists(Odb, "80d5c15a040c93a4f98f4496a05ebf30cdd58650")),
    ?assert(exists(Odb, geef_oid:parse("80d5c15a040c93a4f98f4496a05ebf30cdd58650"))).

-endif.
