#include "geef.h"
#include "repository.h"
#include "object.h"
#include "oid.h"
#include <string.h>
#include <git2.h>

ERL_NIF_TERM
geef_commit_tree_id(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	const git_oid *id;
	geef_object *obj;
	ErlNifBinary bin;

	if (!enif_get_resource(env, argv[0], geef_object_type, (void **) &obj))
		return enif_make_badarg(env);

	id = git_commit_tree_id((git_commit *) obj->obj);

	if (geef_oid_bin(&bin, id) < 0)
		return atoms.error;

	return enif_make_binary(env, &bin);
}
