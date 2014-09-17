defmodule Geef.Tag do
  alias Geef.Object
  import Geef

  def lookup(repo, id) do
    Object.lookup(repo, id, :tag)
  end

  def peel(%Object{type: :tag, handle: handle}) do
    case :geef_nif.tag_peel(handle) do
      {:ok, type, id, peeled_handle} ->
        {:ok, %Object{type: type, id: id, handle: peeled_handle}}
      error ->
        error
    end
  end

  def peel!(tag = %Object{type: :tag}), do: peel(tag) |> assert_ok

end