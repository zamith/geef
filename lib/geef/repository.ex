defmodule Geef.Repository do

  def stop(repo), do: :geef_repo.stop(repo)

  def open(path), do: :geef_repo.open(path)
  def open!(path) do
    case :geef_repo.open(path) do
      {:ok, pid} ->
        pid
      {:error, err} ->
        raise err
    end
  end

  def init(path, bare), do: :geef_repo.open(path, bare)
  def init!(path, bare) do
    case :geef_repo.init(path, bare) do
      {:ok, pid} ->
        pid
      {:error, err} ->
        raise err
    end
  end

  def odb(repo), do: :geef_repo.odb(repo)
  def odb!(repo) do
    case :geef_repo.odb(repo) do
      {:ok, pid} ->
        pid
      {:error, err} ->
        raise err
    end
  end

  def discover(path), do: :geef_repo.disover(path)

  def bare?(repo), do: :geef_repo.is_bare(repo)
  def workdir(repo), do: :geef_repo.workdir(repo)

  def reference_names(repo), do: :geef_repo.references(repo)
end
