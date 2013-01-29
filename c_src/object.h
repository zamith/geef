#ifndef GEEF_OBJECT_H
#define GEEF_OBJECT_H

#include "erl_nif.h"
#include <git2.h>

extern ErlNifResourceType *geef_object_type;

typedef struct {
    git_object *obj;
} geef_object;

ERL_NIF_TERM geef_object_lookup(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

void geef_object_free(ErlNifEnv *env, void *cd);

#endif
