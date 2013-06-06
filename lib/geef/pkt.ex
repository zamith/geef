defrecord Geef.Request, Record.extract(:geef_request, from: "src/geef_records.hrl")

defmodule Geef.Pkt do
  
  def parse_request(str) do
    case :geef_pkt.parse_request(str) do
      {:ok, req} ->
        {:ok, Geef.Request.new req}
      error ->
        error
    end
  end

  def parse(str), do: :geef_pkt.parse(str)
  def line(str), do: :geef_pkt.line(str)

end
