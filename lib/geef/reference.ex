defrecord Geef.Reference, Record.extract(:geef_reference, from: "src/geef_records.hrl") do
  import Geef
  alias Geef.Reference

  def from_erl(ref), do: set_elem(ref, 0, Geef.Reference)
  def to_erl(ref), do: set_elem(ref, 0, :geef_reference)

  defp maybe_ref({:ok, ref}), do: {:ok, Reference.from_erl ref}
  defp maybe_ref(err = {:error, _}), do: err

  def create(repo, name, target, force // :false) do
    :geef_ref.create(repo, name, target, force) |> maybe_ref
  end
  def create!(repo, name, target, force // :false) do
    create(repo, name, target, force) |> assert_ok
  end
  def create_symbolic(repo, name, target, force // :false) do
    :geef_ref.create(repo, name, target, force) |> maybe_ref
  end
  def create_symbolic!(repo, name, target, force // :false) do
    create(repo, name, target, force) |> assert_ok
  end

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
