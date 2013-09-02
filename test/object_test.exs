Code.require_file "../test_helper.exs", __FILE__

defmodule ObjectTest do
  use ExUnit.Case
  use Geef

  defp tmp_dir do
    {a, b, c} = :erlang.now()
    n = node()
    dir = :io_lib.format("geef-~p~p~p~p.git", [n, a, b, c])
    Path.join(System.tmp_dir!, dir)
  end

  test "object OO helpers" do
    dir = tmp_dir
    { :ok, repo } = Repository.init(dir, true)
    { :ok, odb } = Repository.odb(repo)
    #{ :ok, index } = :geef_index.new

    content = "I'm some content"
    Odb.write(odb, content, :blob)
  end
end
