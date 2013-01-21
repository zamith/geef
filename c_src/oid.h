#ifndef GEEF_OID_H
#define GEEF_OID_H

#include "erl_nif.h"
#include <git2.h>

ERL_NIF_TERM geef_oid_fmt(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#endif
