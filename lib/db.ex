defmodule Repo do
  use Ecto.Repo, otp_app: :metalixir, adapter: Sqlite.Ecto2
end
