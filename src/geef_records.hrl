-record(geef_reference, {repo :: pid(), name :: binary(), type :: geef_ref:type(), target :: geef_ref:target()}).
-record(geef_oid, {oid :: binary()}).
-record(geef_object, {type :: atom(), id :: geef_oid:oid(), handle}).
-record(geef_index_entry,
	{ctime :: non_neg_integer(), mtime :: non_neg_integer(),
	 dev :: non_neg_integer(), ino :: non_neg_integer(),
	 mode :: non_neg_integer(), uid :: non_neg_integer(), gid :: non_neg_integer(),
	 size :: non_neg_integer(),
	 id :: geef_oid:oid(),
	 flags, flags_extended,
	 path :: iolist()}).

-record(geef_request, {service :: atom(), path :: binary(), host :: binary()}).
-record(geef_tree_entry, {mode, type, id, name}).
-record(geef_signature, {name :: iolist(), email :: iolist(), time :: geef_sig:time()}).
-record(geef_iterator, {type :: atom(), repo :: pid(), regexp :: iolist(), handle}).

-type geef_request() :: #geef_request{}.
