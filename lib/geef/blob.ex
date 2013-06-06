defmodule Geef.Blob do
  alias Geef.Object

  def lookup(repo, id) do
    case :geef_obj.lookup(repo, id) do
      {:ok, obj} ->
        {:ok, Object.new obj}
      error ->
        error
    end
  end

  def size(obj = Object[type: :blob]) do
    :geef_blob.size(set_elem(obj, 0, :geef_object))
  end

  def content(obj = Object[type: :blob]) do
    :geef_blob.content(set_elem(obj, 0, :geef_object))
  end

end
