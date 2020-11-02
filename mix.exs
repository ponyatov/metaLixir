defmodule M.MixProject do
  use Mix.Project

  def project do
    [
      app:         :metalixir,
      version:     "0.0.1",
      description: "`metaL` circular implementation in Elixir/Erlang",
      source_url:  "https://github.com/ponyatov/metaLixir",
      deps:        deps()
    ]
  end
  def application do
    [
            applications: [:metalixir],
      extra_applications: [:logger,:cowboy]
    ]
  end
  defp deps do
    [
      {:cowboy,       "~> 2.8"},
      {:ecto,         "~> 2.0"},
      {:sqlite_ecto2, "~> 2.4"},
    ]
  end
end
