-module(geef_pkt).

-export([line/1]).

-spec line(iolist()) -> iolist().
line(Text) ->
    % prefix's own size + text size + LF
    Len = 4 + iolist_size(Text) + 1,
    Prefix = io_lib:format("~4.16.0b", [Len]),
    [Prefix, Text, "\n"].
