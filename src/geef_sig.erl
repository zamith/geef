-module(geef_sig).

-include("geef_records.hrl").

-export([new/2, new/3]).

-spec new(iolist(), iolist()) -> geef_signature() | {error, term()}.
new(Name0, Email0) ->
    case geef_nif:signature_new(Name0, Email0) of
        {ok, Name, Email, Time0, Offset} ->
            Time = {{Time0 div 1000000, Time0 rem 1000000, 0}, Offset},
            {ok, #geef_signature{name=Name, email=Email, time=Time}};
        Err ->
            Err
    end.

-spec new(iolist(), iolist(), geef_time()) ->  geef_signature() | {error, term()}.
new(Name0, Email0, Time) ->
    case geef_nif:signature_new(Name0, Email0, 0) of
        {ok, Name, Email} ->
            {ok, #geef_signature{name=Name, email=Email, time=Time}};
        Err ->
            Err
    end.
