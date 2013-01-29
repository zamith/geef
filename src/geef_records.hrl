-record(ref, {handle, type :: atom()}).
-record(repo, {handle}).
-record(odb, {handle}).
-record(oid, {oid}).

-type ref() :: #ref{}.
-type repo() :: #repo{}.
-type odb() :: #odb{}.
-type oid() :: #oid{}.
