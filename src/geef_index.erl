%%%-------------------------------------------------------------------
%%% @author Carlos Martin Nieto <cmn@dwim.me>
%%% @copyright (C) 2013, Carlos Martin Nieto
%%% @doc
%%%
%%% @end
%%% Created : 25 Apr 2013 by Carlos Martin Nieto <cmn@dwim.me>
%%%-------------------------------------------------------------------
-module(geef_index).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([new/0, write/1, write_tree/1, write_tree/2, clear/1, stop/1, read_tree/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {handle}).
-include("geef_records.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Create a new index. This index is empty and not associated to
%% any repository.
-spec new() -> pid().
new() ->
    {ok, Handle} = geef_nif:index_new(),
    start_link(Handle).

%% @doc Write out the index file. This is only possible for indices
%% that are associated with a repository.
-spec write(pid()) -> ok | {error, term()}.
write(Pid) ->
    gen_server:call(Pid, write).

%% @doc Write out the index's contents to its the repository.
-spec write_tree(pid()) -> {ok, oid()} | {error, term()}.
write_tree(Pid) ->
    gen_server:call(Pid, write_tree).

%% @doc Write out the index's contents to the given repository
-spec write_tree(pid(), pid()) -> {ok, oid()} | {error, term()}.
write_tree(Pid, Repo) ->
    gen_server:call(Pid, {write_tree, Repo}).

read_tree(Pid, #object{type=tree, handle=TreeHandle}) ->
    gen_server:call(Pid, {read_tree, TreeHandle}).

%% @doc Clear the contents of the index.
-spec clear(pid()) -> ok.
clear(Pid) ->
    gen_server:call(Pid, clear).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:call(Pid, stop).

%% @private
start_link(Handle) ->
    gen_server:start_link(?MODULE, Handle, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init(Handle) ->
    {ok, #state{handle=Handle}}.

%% @private
handle_call(write, _From, State = #state{handle=Handle}) ->
    Reply = geef_nif:index_write(Handle),
    {reply, Reply, State};
handle_call(write_tree, _From, State = #state{handle=Handle}) ->
    {ok, Oid} = geef_nif:index_write_tree(Handle),
    Reply = #oid{oid=Oid},
    {reply, Reply, State};
handle_call({write_tree, Repo}, _From, State = #state{handle=Handle}) ->
    RepoHandle = geef_repo:handle(Repo),
    {ok, Oid} = geef_nif:index_write_tree(Handle, RepoHandle),
    Reply = #oid{oid=Oid},
    {reply, Reply, State};
handle_call(clear, _From, State = #state{handle=Handle}) ->
    Reply = geef_nif:index_clear(Handle),
    {reply, Reply, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({read_tree, TreeHandle}, _From, State = #state{handle=Handle}) ->
    Reply = geef_nif:index_read_tree(Handle, TreeHandle),
    {reply, Reply, State}.

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
