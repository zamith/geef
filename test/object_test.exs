Code.require_file "../test_helper.exs", __FILE__

defmodule ObjectTest do
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

  test "object OO helpers", meta do
    repo = meta[:repo]
    { :ok, odb } = Repository.odb(repo)
    #{ :ok, index } = :geef_index.new

    content = "I'm some content"
    Odb.write(odb, content, :blob)
  end
end
