defmodule Geef.Odb do
  def exists?(pid, id) do
    :geef_odb.exists(pid, id)
  end

  def write(pid, content, type), do: :geef_odb.write(pid, content, type)

  def stop(pid), do: :geef_odb.stop(pid)
end
