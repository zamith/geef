-module(geef_pkt).

-export([line/1]).

-spec line(iolist()) -> iolist().
line(Text) ->
    Prefix = io_lib:format("~4.16.0b", [iolist_size(Text) + 1]),
    [Prefix, Text, "\n"].
