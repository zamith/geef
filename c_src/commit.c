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
		return geef_oom(env);

	return enif_make_binary(env, &bin);
}

ERL_NIF_TERM
geef_commit_tree(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_object *obj, *tree;
	ERL_NIF_TERM term_obj;

	if (!enif_get_resource(env, argv[0], geef_object_type, (void **) &obj))
		return enif_make_badarg(env);

	tree = enif_alloc_resource(geef_object_type, sizeof(geef_object));

	if (git_commit_tree((git_tree **) &tree->obj, (git_commit *) obj->obj) < 0)
		return geef_error(env);

	term_obj = enif_make_resource(env, obj);
	enif_release_resource(obj);

	return enif_make_tuple3(env, atoms.ok, atoms.tree, term_obj);

}
