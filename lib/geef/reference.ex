require Record

defmodule Geef.Reference do
  import Geef
  alias Geef.Reference

  record = Record.extract(:geef_reference, from: "src/geef_records.hrl")
  keys   = :lists.map(&elem(&1, 0), record)
  vals   = :lists.map(&{&1, [], nil}, keys)
  pairs  = :lists.zip(keys, vals)

  defstruct keys

  def to_record(%Geef.Reference{unquote_splicing(pairs)}) do
    {:geef_reference, unquote_splicing(vals)}
  end

  def from_record({:geef_reference, unquote_splicing(vals)}) do
    %Geef.Reference{unquote_splicing(pairs)}
  end

  defp maybe_ref({:ok, ref}), do: {:ok, Reference.from_record(ref)}
  defp maybe_ref(err = {:error, _}), do: err

  def create(repo, name, target, force \\ :false) do
    :geef_ref.create(repo, name, target, force) |> maybe_ref
  end
  def create!(repo, name, target, force \\ :false) do
    create(repo, name, target, force) |> assert_ok
  end
  def create_symbolic(repo, name, target, force \\ :false) do
    :geef_ref.create(repo, name, target, force) |> maybe_ref
  end
  def create_symbolic!(repo, name, target, force \\ :false) do
    create(repo, name, target, force) |> assert_ok
  end

  def lookup(repo, name), do: :geef_ref.lookup(repo, name) |> maybe_ref
  def lookup!(repo, name), do: lookup(repo, name) |> assert_ok

  def resolve(ref = %Reference{}), do: :geef_ref.resolve(to_record(ref)) |> maybe_ref
  def resolve!(ref = %Reference{}), do: resolve(ref) |> assert_ok

  def dwim(repo, name), do: :geef_ref.dwim(repo, name) |> maybe_ref
  def dwim!(repo, name), do: dwim(repo, name) |> assert_ok

  def shorthand(%Reference{name: name}) do
    :geef_ref.shorthand(name)
  end
  def shorthand(name) do
    :geef_ref.shorthand(name)
  end

  def iterator(repo, regexp \\ :undefined) do
    case :geef_ref.iterator(repo, regexp) do
      {:ok, iter} ->
        {:ok, Geef.Iterator.from_erl iter}
      other ->
        other
    end
  end

end
