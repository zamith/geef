defmodule Geef do

  defmacro __using__(_) do
    quote do
      alias Geef.Repository
      alias Geef.Odb
      alias Geef.Oid
      alias Geef.Reference
      alias Geef.Object
      alias Geef.Commit
      alias Geef.Tree
      alias Geef.TreeEntry
      alias Geef.Blob
      alias Geef.Tag
    end
  end

  def assert_ok({:ok, ref}), do: ref
  def assert_ok({:error, error}), do: raise error

end
