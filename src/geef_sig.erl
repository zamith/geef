-module(geef_sig).

-include("geef_records.hrl").

-type time() :: {erlang:timestamp(), non_neg_integer()}.
-type signature() :: #geef_signature{}.
-export_type([time/0, signature/0]).

-export([default/1, now/2]).
-export([convert/4]).

%% @doc Create a signature for the repository's configured username and
%% email, with a timestamp of now.
-spec default(pid()) -> {ok, signature()} | {error, term()}.
default(Repo) ->
    RepoHandle = geef_repo:handle(Repo),
    case geef_nif:signature_default(RepoHandle) of
        {ok, Name, Email, Timestamp, Offset} ->
	    {ok, convert(Name, Email, Timestamp, Offset)};
        Err = {error, _} ->
            Err
    end.

%% @doc Create a signature with the specified username and email, with
%% a timestamp of now
now(Name, Email) ->
    Now = now(),
    %% We ask two questions "what's the time here?" and "what's the
    %% time in UTC-Land?". The difference in minutes is our offset.
    Local = calendar:datetime_to_gregorian_seconds(calendar:now_to_local_time(Now)),
    UTC = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(Now)),
    Offset = (Local - UTC) div 60,
    #geef_signature{name=Name, email=Email, time={Now, Offset}}.

%% @private
%% @doc convert the return from the NIF to a signature
-spec convert(binary(), binary(), non_neg_integer(), non_neg_integer()) -> signature().
convert(Name, Email, Timestamp, Offset) ->
            Time = {{Timestamp div 1000000, Timestamp rem 1000000, 0}, Offset},
            #geef_signature{name=Name, email=Email, time=Time}.
