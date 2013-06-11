defmodule Geef.Tag do
  alias Geef.Object
  import :macros, Object
  import Geef

  def peel(tag = Object[type: :tag]) do
    case :geef_tag.peel(rebind(tag)) do
      {:ok, peeled} ->
        {:ok, Object.new peeled}
      error ->
        error
    end
  end

  def peel!(tag = Object[type: :tag]), do: peel(tag) |> assert_ok

end