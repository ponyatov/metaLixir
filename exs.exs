app     = "metaLixir"
title   = "`metaL` circular implementation in Elixir/Erlang"
author  = "Dmitry Ponyatov"
email   = "dponyatov@gmail.com"
license = "MIT"
github  = "https://github.com/ponyatov/#{app}"

dir = "."
doc = "#{dir}/doc"
lib = "#{dir}/lib"
src = "#{dir}/src"
tmp = "#{dir}/tmp"

appl = app|>String.downcase
appc = app|>String.capitalize

File.write("#{dir}/.formatter.exs", """
[ inputs: [
    "{config,lib,test}/**/*.{ex,exs}",
    "{mix,.formatter}.exs"
] ]
""")

File.write("#{dir}/mix.exs", """
""")

File.write("#{dir}/.gitignore", """
*.beam
/.elixir_ls/
/_build/
/deps/
/mix.lock
""")

File.write("#{dir}/README.md", """
#  `#{app}`
## #{title}

* automatic code generation
* homoiconic (meta)language for source code transformations

(c) #{author} <<#{email}>> 2020 #{license}

github: #{github}

## Code Generation

* [Code Generation in Elixir // Bryan Weber](https://www.youtube.com/watch?v=-mgwW3RVI50)
""")

File.mkdir("#{dir}/.vscode")

multiCommand = fn (key,command) ->
~s|{"command": "multiCommand.#{key}", "sequence":[
      "workbench.action.files.saveAll",
        {"command": "workbench.action.terminal.sendSequence",
          "args": {"text": "\\u000D#{command}\\u000D"}}]},| end

vs_exclude = ~S|
    "**/.elixir_ls/**":true,
    "**/_build/**":true,
    "**/deps/**":true,|

File.write("#{dir}/.vscode/settings.json","""
{
  "multiCommand.commands": [
    #{multiCommand.("f9" ,"elixir exs.exs")}
    #{multiCommand.("f11","make repl")}
    #{multiCommand.("f12","System.stop")}
  ],
  "files.watcherExclude": {#{vs_exclude}
  },
  "files.exclude": {#{vs_exclude}
  },
}
""")

File.write("#{dir}/mix.exs","""
defmodule M.MixProject do
  use Mix.Project

  def project do
    [
      app:         :#{appl},
      version:     "0.0.1",
      description: "#{title}",
      source_url:  \"#{github}\",
      deps:        deps()
    ]
  end
  def application do
    [
            applications: [:#{appl}],
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
""")

File.write("#{dir}/Makefile","""
MODULE  = $(notdir $(CURDIR))
OS     ?= $(shell uname -s)

# dirs

CWD = $(CURDIR)
DOC = $(CWD)/doc
LIB = $(CWD)/lib
SRC = $(CWD)/src
TMP = $(CWD)/tmp
GZ  = $(HOME)/gz

# tools

WGET    = wget -c
ERL     = erl
ERLC    = erlc
ELIXIR  = elixir
ELIXIRC = elixirc
MIX     = mix
IEX     = iex

# targets

EX  += $(LIB)/#{appl}.ex
ERL += $(SRC)/#{appl}.erl

.PHONY: all
all: $(EX)

.PHONY: repl
repl:
\t$(ELIXIR) exs.exs
#\t$(MIX)    format
\t$(MIX)    test
\t$(IEX) -S mix
\t$(MAKE)   $@

# rules

$(LIB)/%.ex: $(LIB)/%.exs

# install

.PHONY: install update
install update:
\tmake $(OS)_install
\t$(MIX) deps.get
.PHONY: Linux_install Linux_update
Linux_install Linux_update:
\tsudo apt update
\tsudo apt install -u `cat apt.txt`

# git

MERGE  = Makefile .gitignore README.md .vscode apt.txt
MERGE += exs.exs mix.exs .formatter.exs lib src test

master:
	git checkout $@
	git pull -v
	git checkout shadow -- $(MERGE)

shadow:
	git checkout $@
	git pull -v

release:
	git tag $(NOW)-$(REL)
	git push -v && git push -v --tags
	$(MAKE) shadow
""")

File.write("#{dir}/apt.txt","""
git make wget
erlang elixir
""")

File.mkdir(src)
File.write("#{src}/.gitignore","")

File.mkdir(src)
File.write("#{src}/.gitignore","")

erl = "#{src}/#{appl}.erl"
File.write(erl,"""
-module(#{appl}).
-export([]).
-on_load(init/0).
init() -> nil.
""")

File.mkdir(tmp)
File.write("#{tmp}/.gitignore","*")

File.mkdir(lib)

readme = ~S|
  @readme   """
            #  `#{@app}`
            ## #{@title}

            * automatic code generation
            * homoiconic (meta)language for source code transformations

            (c) #{@author()} <<#{@email}>> 2020 #{@license}

            github: #{@github}

            ## Code Generation

            * [Code Generation in Elixir // Bryan Weber](https://www.youtube.com/watch?v=-mgwW3RVI50)
            * [ElixirConf 2017 - Don't Write Macros But Do Learn How They Work - Jesse Anderson](https://www.youtube.com/watch?v=Bo48sQDb-hk)
            """

  def readme,  do: @readme
|

metainfo = ~s|
defmodule M.MetaInfo do

  @app     "metaLixir"
  @title   "`metaL` circular implementation in Elixir/Erlang"
  @author  "Dmitry Ponyatov"
  @email   "dponyatov@gmail.com"
  @license "MIT"
  @github  "https://github.com/ponyatov/\#{@app}"

  def app,     do: @app
  def title,   do: @title
  def author,  do: @author
  def email,   do: @email
  def license, do: @license
  def github,  do: @github

  #{readme}
end |

object = ~S|
defmodule M.Object do

  defstruct type: :object, val: nil, nest: []

end |

File.write("#{dir}/lib/#{appl}.ex","""
  #{metainfo}
""")

File.write("#{dir}/lib/object.ex",object)

File.mkdir("#{dir}/test")
File.write("#{dir}/test/test_helper.exs","""
ExUnit.start()
""")
File.write("#{dir}/test/#{appl}_test.exs","""
defmodule #{appc}Test do
  use ExUnit.Case
end
""")

File.write("#{dir}/lib/web.ex",~s|
defmodule #{appc}.Web.Handeler do
end

defmodule #{appc}.Web.Route do

  def call(path,req), do: route(path,req)

  def head, do: """
  <HEAD>
  </HEAD>
  """

  def style, do: """
  <STYLE>
  * { background: #222; color: lightgreen; }
  </STYLE>
  """

  def route(path,req) do
      """
      \#\{head()\}
      \#\{style()\}
      <H1>no route to <\#\{path\}>"</H1>
      <HR>
      <PRE>
      #\{req\}
      </PRE>
      """
  end

end |)

File.write("#{dir}/lib/db.ex","""
defmodule Repo do
  use Ecto.Repo, otp_app: :#{appl}, adapter: Sqlite.Ecto2
end
""")

File.mkdir("#{dir}/config")
File.write("#{dir}/config/config.exs","""
use Mix.Config

config :#{app},Repo,
  adapter: Sqlite.Ecto2,
  database: "#{appl}.sqlite3"
""")
