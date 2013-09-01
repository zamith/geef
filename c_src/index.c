#include "geef.h"
#include "oid.h"
#include "index.h"
#include "object.h"
#include <string.h>
#include <git2.h>

void geef_index_free(ErlNifEnv *env, void *cd)
{
	geef_index *index = (geef_index *) cd;
	git_index_free(index->index);
}

ERL_NIF_TERM
geef_index_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_index *index;
	ERL_NIF_TERM term;

	index = enif_alloc_resource(geef_index_type, sizeof(geef_index));
	if (!index)
		return geef_oom(env);

	if (git_index_new(&index->index) < 0)
		return geef_error(env);

	term = enif_make_resource(env, index);
	enif_release_resource(index);

	return enif_make_tuple2(env, atoms.ok, term);
}

ERL_NIF_TERM
geef_index_write(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_index *index;

	if (!enif_get_resource(env, argv[0], geef_index_type, (void **) &index))
		return enif_make_badarg(env);

	if (git_index_write(index->index) < 0)
		return geef_error(env);

	return atoms.ok;
}

ERL_NIF_TERM
geef_index_write_tree(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_index *index;
	geef_repository *repo;
	ErlNifBinary bin;
	git_oid id;
	int error;

	if (!enif_get_resource(env, argv[0], geef_index_type, (void **) &index))
		return enif_make_badarg(env);

	if (argc == 2) {
		if (!enif_get_resource(env, argv[1], geef_repository_type, (void **) &repo))
			return enif_make_badarg(env);

		error = git_index_write_tree_to(&id, index->index, repo->repo);
	} else {
		error = git_index_write_tree(&id, index->index);
	}

	if (error < 0)
		return geef_error(env);

	if (geef_oid_bin(&bin, &id) < 0)
		return geef_oom(env);

	return enif_make_tuple2(env, atoms.ok, enif_make_binary(env, &bin));
}

ERL_NIF_TERM
geef_index_read_tree(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_index *index;
	geef_object *tree;

	if (!enif_get_resource(env, argv[0], geef_index_type, (void **) &index))
		return enif_make_badarg(env);

	if (!enif_get_resource(env, argv[0], geef_object_type, (void **) &tree))
		return enif_make_badarg(env);

	if (git_index_read_tree(index->index, (git_tree *)tree->obj) < 0)
		return geef_error(env);

	return atoms.ok;
}

ERL_NIF_TERM
geef_index_add(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_index *index;
	const ERL_NIF_TERM *eentry, *oid;
	int arity;
	ErlNifBinary path, id;
	git_index_entry entry;

	if (!enif_get_resource(env, argv[0], geef_index_type, (void **) &index))
		return enif_make_badarg(env);

	if (!enif_get_tuple(env, argv[1], &arity, &eentry))
		return enif_make_badarg(env);

	memset(&entry, 0, sizeof(entry));

	if (!enif_get_uint(env, eentry[5], &entry.mode))
		return enif_make_badarg(env);

	if (!enif_get_int64(env, eentry[8], &entry.file_size))
		return enif_make_badarg(env);

	if (!enif_inspect_iolist_as_binary(env, eentry[12], &path))
		return enif_make_badarg(env);

	if (!geef_terminate_binary(&path))
		return geef_oom(env);

	entry.path = (char *) path.data;
	/* Extract the oid from the tuple */
	if (!enif_get_tuple(env, eentry[9], &arity, &oid))
		return enif_make_badarg(env);

	if (!enif_inspect_binary(env, oid[1], &id))
		return enif_make_badarg(env);

	git_oid_fromraw(&entry.oid, id.data);

	if (git_index_add(index->index, &entry) < 0)
		return geef_error(env);

	return atoms.ok;
}

ERL_NIF_TERM
geef_index_count(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_index *index;

	if (!enif_get_resource(env, argv[0], geef_index_type, (void **) &index))
		return enif_make_badarg(env);

	return enif_make_uint(env, git_index_entrycount(index->index));
}

ERL_NIF_TERM entry_to_term(ErlNifEnv *env, const git_index_entry *entry)
{
	ErlNifBinary id, path;
	ERL_NIF_TERM mode, file_size;
	size_t len;

	if (geef_oid_bin(&id, &entry->oid) < 0)
		return geef_oom(env);

	len = strlen(entry->path);
	if (!enif_alloc_binary(len, &path)) {
		enif_release_binary(&id);
		return geef_oom(env);
	}
	memcpy(path.data, entry->path, len);

	mode = enif_make_uint(env, entry->mode);
	file_size = enif_make_int64(env, entry->file_size);

	return enif_make_tuple5(env, atoms.ok, enif_make_binary(env, &path), enif_make_binary(env, &id),
				mode, file_size);
}

ERL_NIF_TERM
geef_index_nth(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	size_t nth;
	geef_index *index;
	const git_index_entry *entry;

	if (!enif_get_resource(env, argv[0], geef_index_type, (void **) &index))
		return enif_make_badarg(env);

	if (!enif_get_ulong(env, argv[1], &nth))
		return enif_make_badarg(env);

	entry = git_index_get_byindex(index->index, nth);
	if (entry == NULL)
		return geef_error(env);

	return entry_to_term(env, entry);
}

ERL_NIF_TERM
geef_index_get(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	unsigned int stage;
	ErlNifBinary path;
	geef_index *index;
	const git_index_entry *entry;

	if (!enif_get_resource(env, argv[0], geef_index_type, (void **) &index))
		return enif_make_badarg(env);

	if (!enif_get_uint(env, argv[2], &stage))
		return enif_make_badarg(env);

	if (!enif_inspect_iolist_as_binary(env, argv[1], &path))
		return enif_make_badarg(env);

	if (geef_terminate_binary(&path) < 0) {
		enif_release_binary(&path);
		return geef_oom(env);
	}

	entry = git_index_get_bypath(index->index, (char *) path.data, stage);
	enif_release_binary(&path);
	if (entry == NULL)
		return geef_error(env);

	return entry_to_term(env, entry);
}

ERL_NIF_TERM
geef_index_clear(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_index *index;

	if (!enif_get_resource(env, argv[0], geef_index_type, (void **) &index))
		return enif_make_badarg(env);

	git_index_clear(index->index);

	return atoms.ok;
}
