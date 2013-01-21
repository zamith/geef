#include <git2.h>
#include "geef.h"
#include "oid.h"

ERL_NIF_TERM
geef_oid_fmt(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary bin, bin_out;
	git_oid id;

	if (!enif_inspect_binary(env, argv[0], &bin))
		return enif_make_badarg(env);

	if (bin.size < GIT_OID_RAWSZ)
		return enif_make_badarg(env);

	if (!enif_alloc_binary(GIT_OID_HEXSZ, &bin_out))
		return atoms.error;

	git_oid_fromraw(&id, bin.data);
	git_oid_fmt((char *)bin_out.data, &id);

	return enif_make_binary(env, &bin_out);
}
