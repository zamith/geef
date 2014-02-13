#include "geef.h"
#include "repository.h"
#include "reference.h"
#include "oid.h"
#include "signature.h"
#include <string.h>
#include <git2.h>

ERL_NIF_TERM
geef_reflog_read(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	git_reflog *reflog;
	geef_repository *repo;
	ErlNifBinary bin;
	int error;
	size_t count, i;
	ERL_NIF_TERM list;

	if (!enif_get_resource(env, argv[0], geef_repository_type, (void **) &repo))
		return enif_make_badarg(env);

	if (!enif_inspect_iolist_as_binary(env, argv[1], &bin))
		return enif_make_badarg(env);

	if (!geef_terminate_binary(&bin))
		return geef_oom(env);

	if ((error = git_reflog_read(&reflog, repo->repo, (char *)bin.data)) < 0)
		return geef_error(env);

	count = git_reflog_entrycount(reflog);
	list = enif_make_list(env, 0);

	for (i = count; i > 0; i--) {
		ErlNifBinary id_old, id_new, message;
		ERL_NIF_TERM committer, tentry;
		const git_reflog_entry *entry;

		entry = git_reflog_entry_byindex(reflog, i-1);

		if (geef_oid_bin(&id_old, git_reflog_entry_id_old(entry)))
			goto on_oom;

		if (geef_oid_bin(&id_new, git_reflog_entry_id_new(entry)))
			goto on_oom;

		if (geef_signature_to_erl(&committer, env, git_reflog_entry_committer(entry)))
			goto on_oom;

		if (geef_string_to_bin(&message, git_reflog_entry_message(entry)))
			goto on_oom;

		tentry = enif_make_tuple5(env, atoms.reflog_entry, committer,
					  enif_make_binary(env, &id_old),
					  enif_make_binary(env, &id_new),
					  enif_make_binary(env, &message));
		list = enif_make_list_cell(env, tentry, list);
	}

	git_reflog_free(reflog);
	return enif_make_tuple2(env, atoms.ok, list);

on_oom:
	git_reflog_free(reflog);
	return geef_oom(env);
}
