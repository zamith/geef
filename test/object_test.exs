defmodule ObjectTest do
  use ExUnit.Case
  use Geef
  import RepoHelpers

  setup do
    {repo, path} = tmp_bare()
    Process.link(repo)
    on_exit(fn -> File.rm_rf!(path) end)
    {:ok, [repo: repo, path: path]}
  end

  test "object OO helpers", meta do
    repo = meta[:repo]
    { :ok, odb } = Repository.odb(repo)
    #{ :ok, index } = :geef_index.new

    content = "This is some text that will go in a file"
    {:ok, id} = Odb.write(odb, content, :blob)
    assert id == Oid.parse("c300118399f01fe52b316061b5d32beb27e0adfd")

    Repository.stop(repo)
  end
end
