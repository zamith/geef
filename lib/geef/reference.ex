require Record

defmodule Geef.Reference do
  import Geef
  alias Geef.Reference

  defstruct repo: nil, name: nil, type: nil, target: nil

  def create(repo, name, target, force \\ :false) do
    # fixme: this needs to ask the repo itself
    repo_handle = :geef_repo.handle(repo)
    case :geef_nif.reference_create(repo_handle, name, :oid, target, force) do
      :ok ->
        {:ok, %Reference{repo: repo, name: name, type: :oid, target: target}}
      error ->
        error
    end
  end
  def create!(repo, name, target, force \\ :false) do
    create(repo, name, target, force) |> assert_ok
  end

  def create_symbolic(repo, name, target, force \\ :false) do
    # fixme: this needs to ask the repo itself
    repo_handle = :geef_repo.handle(repo)
    case :geef_nif.reference_create(repo_handle, name, :symbolic, target, force) do
      :ok ->
        {:ok, %Reference{repo: repo, name: name, type: :symbolic, target: target}}
      error ->
        error
    end
  end
  def create_symbolic!(repo, name, target, force \\ :false) do
    create_symbolic(repo, name, target, force) |> assert_ok
  end

  def lookup(repo, name) do
    case :geef_repo.reference_lookup(repo, name) do
      {:ok, type, target} ->
        {:ok, %Reference{repo: repo, name: name, type: type, target: target}}
      error ->
        error
    end
  end
  def lookup!(repo, name), do: lookup(repo, name) |> assert_ok

  # A direct referene is only ever going to be itself
  def resolve(ref = %Reference{type: :oid}) do
    {:ok, ref}
  end
  def resolve(%Reference{repo: repo, name: name}) do
    case :geef_repo.reference_resolve(repo, name) do
      {:ok, resolved_name, target} ->
        {:ok, %Reference{repo: repo, name: resolved_name, type: :oid, target: target}}
      error ->
        error
    end
  end

  def resolve!(ref = %Reference{}), do: resolve(ref) |> assert_ok

  def dwim(repo, name) do
    case :geef_repo.reference_dwim(repo, name) do
      {:ok, real_name, type, target} ->
        {:ok, %Reference{repo: repo, name: real_name, type: type, target: target}}
      error ->
        error
    end
  end

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
