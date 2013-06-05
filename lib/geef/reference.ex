defrecord :geef_reference, Record.extract(:geef_reference, from: "src/geef_records.hrl")

defmodule Geef.Reference do
  alias Geef.Repository

  def lookup(repo, name), do: :geef_ref.lookup(repo, name)
  def lookup!(repo, name) do
    case lookup(repo, name) do
      {:ok, ref} ->
        ref
      {:error, err} ->
        raise err
    end
  end

  def resolve(ref), do: :geef_ref.resolve(ref)
  def resolve!(ref) do
    case :geef_ref.resolve(ref) do
      {:ok, resolved} ->
        resolved
      {:error, err} ->
        raise err
    end
  end

end
