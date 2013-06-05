defrecord :geef_object, Record.extract(:geef_object, from: "src/geef_records.hrl")

defmodule Geef.Object do

  def lookup(repo, id), do: :geef_obj.lookup(repo, id)
  def lookup!(repo, id) do
    case :geef_obj.lookup(repo, id) do
      {:ok, obj} ->
        obj
      {:error, err} ->
        raise err
    end
  end


  def lookup(repo, id, type), do: :geef_obj.lookup(repo, id, type)

end
