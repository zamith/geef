defmodule Geef.Blob do
  alias Geef.Object
  import :macros, Object

  def lookup(repo, id) do
    case :geef_obj.lookup(repo, id) do
      {:ok, obj} ->
        {:ok, Object.from_erl obj}
      error ->
        error
    end
  end

  def size(obj = Object[type: :blob]) do
    :geef_blob.size(rebind(obj))
  end

  def content(obj = Object[type: :blob]) do
    :geef_blob.content(rebind(obj))
  end

end
