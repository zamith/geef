defmodule Geef.Odb do
  def exists?(pid, id) do
    :geef_odb.exists(pid, id)
  end

  def stop(pid), do: :geef_odb.stop(pid)
end
