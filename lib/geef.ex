defmodule Geef do

  defmacro __using__(_) do
    quote do
      alias Geef.Repository
      alias Geef.Reference
      alias Geef.Object
      alias Geef.Commit
      alias Geef.Tree
      alias Geef.TreeEntry
      alias Geef.Blob
      alias Geef.Tag
    end
  end

end
