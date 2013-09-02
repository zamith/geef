ExUnit.start

defmodule RepoHelpers do
  def tmp_bare do
    {a, b, c} = :erlang.now()
    n = node()
    dir = :io_lib.format("geef-~p~p~p~p.git", [n, a, b, c])
    path = Path.join(System.tmp_dir!, dir)
    {Geef.Repository.init(path, true), path}
  end
end
