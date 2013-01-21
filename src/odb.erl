-module(odb, [Handle]).
-export([exists/1]).

exists(Sha) ->
    geef:odb_object_exists(Handle, Sha).
