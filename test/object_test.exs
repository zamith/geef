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

    content = "I'm some content"
    Odb.write(odb, content, :blob)

    Repository.stop(repo)
  end
end
