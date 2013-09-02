defrecord Geef.Reference, Record.extract(:geef_reference, from: "src/geef_records.hrl") do
  import Geef
  alias Geef.Reference
  alias Geef.Oid
  import Oid, only: :macros

  def from_erl(ref) do
    case set_elem(ref, 0, Geef.Reference) do
      ref = Reference[type: :symbolic] -> ref
      ref = Reference[type: :oid, target: oid] -> ref.target(set_elem(oid, 0, Geef.Oid))
    end
  end

  def to_erl(ref = Reference[type: :symbolic]) do
    set_elem(ref, 0, :geef_reference)
  end
  def to_erl(ref = Reference[type: :oid, target: oid]) do
    ref |> set_elem(0, :geef_reference) |> set_elem(4, set_elem(oid, 0, :geef_oid))
  end

  defp maybe_ref({:ok, ref}), do: {:ok, Reference.from_erl ref}
  defp maybe_ref(err = {:error, _}), do: err

  def lookup(repo, name), do: :geef_ref.lookup(repo, name) |> maybe_ref
  def lookup!(repo, name), do: lookup(repo, name) |> assert_ok

  def resolve(ref = Reference[]), do: :geef_ref.resolve(to_erl(ref)) |> maybe_ref
  def resolve!(ref =Reference[]), do: resolve(ref) |> assert_ok

  def dwim(repo, name), do: :geef_ref.dwim(repo, name) |> maybe_ref
  def dwim!(repo, name), do: dwim(repo, name) |> assert_ok

  def shorthand(Reference[name: name]) do
    :geef_ref.shorthand(name)
  end
  def shorthand(name) do
    :geef_ref.shorthand(name)
  end

  def iterator(repo, regexp // :undefined) do
    case :geef_ref.iterator(repo, regexp) do
      {:ok, iter} ->
        {:ok, Geef.Iterator.from_erl iter}
      other ->
        other
    end
  end

end
