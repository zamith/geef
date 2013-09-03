defmodule Geef.Oid do

  @type t :: <<_ :: 160>>

  @spec parse(iolist) :: t
  def parse(str) do
    :geef_oid.parse(str)
  end

  @spec hex(t) :: String.t
  def hex(oid) do
    :geef_oid.hex(oid)
  end

end
