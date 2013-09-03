Code.require_file "../test_helper.exs", __FILE__

defmodule ReferenceTest do
  use ExUnit.Case
  use Geef
  import RepoHelpers

  setup do
    {{:ok, repo}, path} = tmp_bare()
    {:ok, [repo: repo, path: path]}
  end

  teardown meta do
    Repository.stop(meta[:repo])
    File.rm_rf!(meta[:path])
    :ok
  end

  test "creating and looking up", meta do
    repo = meta[:repo]
    { :ok, odb } = Repository.odb(repo)
    content = "I'm some content"
    {:ok, id} = Odb.write(odb, content, :blob)

    refname = "refs/tags/foo"
    {:ok, ref} = Reference.create(repo, refname, id)
    {:ok, looked_up} = Reference.lookup(repo, refname)
    ref == looked_up

    refname = "refs/tags/foo2"
    {:ok, ref} = Reference.create_symbolic(repo, refname, id)
    {:ok, looked_up} = Reference.lookup(repo, refname)
    ref == looked_up
  end

end
