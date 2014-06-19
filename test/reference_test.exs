defmodule ReferenceTest do
  use ExUnit.Case
  use Geef
  import RepoHelpers

  setup do
    {repo, path} = tmp_bare()
    on_exit(fn -> File.rm_rf!(path) end)
    {:ok, [repo: repo, path: path]}
  end

  test "creating and looking up", meta do
    repo = meta[:repo]
    { :ok, odb } = Repository.odb(repo)
    content = "I'm some content"
    {:ok, id} = Odb.write(odb, content, :blob)

    refname = "refs/tags/foo"
    {:ok, ref} = Reference.create(repo, refname, id)
    {:ok, looked_up} = Reference.lookup(repo, refname)
    assert ref == looked_up

    refname = "refs/tags/foo2"
    {:ok, ref} = Reference.create_symbolic(repo, refname, id)
    {:ok, looked_up} = Reference.lookup(repo, refname)
    assert ref == looked_up

    Repository.stop(repo)
  end

end
