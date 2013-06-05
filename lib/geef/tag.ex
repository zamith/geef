defmodule Geef.Tag do
  alias Geef.Object

  def peel(tag = Object[type: :tag]) do
    case :geef_tag.peel(set_elem(tag, 0, :geef_object)) do
      {:ok, peeled} ->
        {:ok, Object.new peeled}
      error ->
        error
    end
  end

  def peel!(tag = Object[type: :tag]) do
    case peel(tag) do
      {:ok, peeled} -> peeled
      {:error, err} -> raise err
    end
  end

end