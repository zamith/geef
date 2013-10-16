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
-export([new/0, write/1, write_tree/1, write_tree/2, clear/1, stop/1, read_tree/2, add/2, count/1, get/3, nth/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {handle}).
-include("geef_records.hrl").

-type entry() :: #geef_index_entry{}.
-export_type([entry/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Create a new index. This index is empty and not associated to
%% any repository.
-spec new() -> {ok, pid()} | ignore | {error, term()}.
new() ->
    {ok, Handle} = geef_nif:index_new(),
    start_link(Handle).

%% @doc Write out the index file. This is only possible for indices
%% that are associated with a repository.
-spec write(pid()) -> ok | {error, term()}.
write(Pid) ->
    gen_server:call(Pid, write).

%% @doc Write out the index's contents to its the repository.
-spec write_tree(pid()) -> {ok, geef_oid:oid()} | {error, term()}.
write_tree(Pid) ->
    gen_server:call(Pid, write_tree).

%% @doc Write out the index's contents to the given repository
-spec write_tree(pid(), pid()) -> {ok, geef_oid:oid()} | {error, term()}.
write_tree(Pid, Repo) ->
    gen_server:call(Pid, {write_tree, Repo}).

read_tree(Pid, #geef_object{type=tree, handle=TreeHandle}) ->
    gen_server:call(Pid, {read_tree, TreeHandle}).

%% @doc Add an entry to the index
-spec add(pid(), entry()) -> ok | {error, term()}.
add(Pid, Entry) ->
    gen_server:call(Pid, {add, Entry}).

-spec count(pid()) -> non_neg_integer().
count(Pid) ->
    gen_server:call(Pid, count).

%% @doc Retrieve an entry with a particular path and stage.
-spec get(pid(), iolist(), non_neg_integer()) -> {ok, entry()} | {error, term()}.
get(Pid, Path, Stage) ->
    maybe_entry(gen_server:call(Pid, {get, Path, Stage})).

%% @doc Retrieve an entry by index. Different stages of a path are
%% considered different entries.
-spec nth(pid(), non_neg_integer()) -> {ok, entry()} | {error, term()}.
nth(Pid, Nth) ->
    maybe_entry(gen_server:call(Pid, {nth, Nth})).

maybe_entry({ok, Ctime, Mtime, Dev, Ino, Mode, Uid, Gid, Size, Id, Flags, FlagsExtended, Path}) ->
    {ok, #geef_index_entry{ctime=Ctime, mtime=Mtime, dev=Dev, ino=Ino, mode=Mode, uid=Uid, gid=Gid,
			   size=Size, id=Id, flags=Flags, flags_extended=FlagsExtended, path=Path}};
maybe_entry(Error = {error, _}) ->
    Error.


%% @doc Clear the contents of the index.
-spec clear(pid()) -> ok.
clear(Pid) ->
    gen_server:call(Pid, clear).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:call(Pid, stop).

%% @private
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
handle_call(write, _From, State = #state{handle=Handle}) ->
    Reply = geef_nif:index_write(Handle),
    {reply, Reply, State};
handle_call(write_tree, _From, State = #state{handle=Handle}) ->
    Reply = geef_nif:index_write_tree(Handle),
    {reply, Reply, State};
handle_call({write_tree, Repo}, _From, State = #state{handle=Handle}) ->
    RepoHandle = geef_repo:handle(Repo),
    Reply = geef_nif:index_write_tree(Handle, RepoHandle),
    {reply, Reply, State};
handle_call(clear, _From, State = #state{handle=Handle}) ->
    Reply = geef_nif:index_clear(Handle),
    {reply, Reply, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({read_tree, TreeHandle}, _From, State = #state{handle=Handle}) ->
    Reply = geef_nif:index_read_tree(Handle, TreeHandle),
    {reply, Reply, State};

handle_call(count, _From, State = #state{handle=Handle}) ->
    Reply = geef_nif:index_count(Handle),
    {reply, Reply, State};

handle_call({nth, Nth}, _From, State = #state{handle=Handle}) ->
    Reply = geef_nif:index_nth(Handle, Nth),
    {reply, Reply, State};

handle_call({get, Path, Stage}, _From, State = #state{handle=Handle}) ->
    Reply = geef_nif:index_get(Handle, Path, Stage),
    {reply, Reply, State};

handle_call({add, Entry = #geef_index_entry{}}, _From, State = #state{handle=Handle}) ->
    Reply = geef_nif:index_add(Handle, Entry),
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
