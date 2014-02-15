-module(ref_test).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("src/geef_records.hrl").

test_fun(F, [{Name, Val} | Tail]) ->
    ?assertEqual(Val, F(Name)),
    test_fun(F, Tail);
test_fun(_, []) ->
    ok.

branch_test() ->
    ?assertEqual(true, geef_ref:is_branch("refs/heads/master")),
    ?assertEqual(true, geef_ref:is_branch(<<"refs/heads/master">>)),
    ?assertEqual(false, geef_ref:is_branch("refs/tags/v0.1")),
    ?assertEqual(false, geef_ref:is_branch(#geef_reference{name= <<"refs/tags/foo">>})),
    ?assertEqual(true, geef_ref:is_branch(#geef_reference{name= <<"refs/heads/foo">>})).

tag_test() ->
    ?assertEqual(false, geef_ref:is_tag("refs/heads/master")),
    ?assertEqual(true, geef_ref:is_tag(<<"refs/tags/mastery">>)),
    ?assertEqual(true, geef_ref:is_tag("refs/tags/v0.1")),
    ?assertEqual(true, geef_ref:is_tag(#geef_reference{name= <<"refs/tags/foo">>})),
    ?assertEqual(false, geef_ref:is_tag(#geef_reference{name= <<"refs/heads/foo">>})).

note_test() ->
    ?assertEqual(false, geef_ref:is_note("refs/heads/master")),
    ?assertEqual(true, geef_ref:is_note(<<"refs/notes/commits">>)),
    ?assertEqual(true, geef_ref:is_note("refs/notes/bugs")),
    ?assertEqual(true, geef_ref:is_note(#geef_reference{name= <<"refs/notes/foo">>})),
    ?assertEqual(false, geef_ref:is_note(#geef_reference{name= <<"refs/heads/foo">>})).

remote_test() ->
    ?assertEqual(false, geef_ref:is_remote("refs/heads/master")),
    ?assertEqual(true, geef_ref:is_remote(<<"refs/remotes/origin/master">>)),
    ?assertEqual(true, geef_ref:is_remote("refs/remotes/origin/development")),
    ?assertEqual(true, geef_ref:is_remote(#geef_reference{name= <<"refs/remotes/svn">>})),
    ?assertEqual(false, geef_ref:is_remote(#geef_reference{name= <<"refs/heads/foo">>})).
