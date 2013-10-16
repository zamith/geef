defmodule Geef.Revparse do
  import Geef
  use Geef

  @spec single(pid, iolist) :: {:ok, Object.t} | {:error, term}
  def single(repo, str) do
    case :geef_revparse.single(repo, str) do
      {:ok, obj} ->
        {:ok, Object.from_erl(obj)}
      error -> error
    end
  end

  @spec single!(pid, iolist) :: {:ok, Object.t}
  def single!(repo, str), do: single(repo, str) |> assert_ok

end