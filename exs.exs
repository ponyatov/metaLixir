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
""")

File.write("#{dir}/README.md", """
#  `#{app}`
## #{title}

* automatic code generation
* homoiconic (meta)language for source code transformations

(c) #{author} <<#{email}>> 2020 #{license}

github: #{github}
""")

File.mkdir("#{dir}/.vscode")

multiCommand = fn (key,command) ->
~s|{"command": "multiCommand.#{key}", "sequence":[
      "workbench.action.files.saveAll",
        {"command": "workbench.action.terminal.sendSequence",
          "args": {"text": "\\u000D#{command}\\u000D"}}]},| end

vs_exclude = ~S|
    "**/.elixir_ls/**":true,|

File.write("#{dir}/.vscode/settings.json","""
{
  "multiCommand.commands": [
    #{multiCommand.("f11","iex -S mix")}
    #{multiCommand.("f12","elixir exs.exs")}
  ],
  "files.watcherExclude": {#{vs_exclude}
  },
  "files.exclude": {#{vs_exclude}
  },
}
""")

File.write("#{dir}/mix.exs","""
defmodule M.MixProject do
  @docmodule " #{app} "
  use Mix.Project
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
IEX     = iex

# targets

EX  += $(LIB)/#{appl}.ex
ERL += $(SRC)/#{appl}.erl

.PHONY: all
all: $(EX)

.PHONY: repl
repl:
\t$(IEX) -S mix

# rules

$(LIB)/%.ex: $(LIB)/%.exs

# install

.PHONY: install update
install:
\tmake $(OS)_install
update:
\tmake $(OS)_update
.PHONY: Linux_install Linux_update
Linux_install Linux_update:
\tsudo apt update
\tsudo apt install -u `cat apt.txt`

# git

MERGE  = Makefile .gitignore README.md .vscode apt.txt
MERGE += exs.exs mix.exs lib src

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
File.write("#{dir}/lib/#{appl}.ex","""
defmodule M do
  @app     "metaLixir"
  @title   "`metaL` circular implementation in Elixir/Erlang"
  @author  "Dmitry Ponyatov"
  @email   "dponyatov@gmail.com"
  @license "MIT"
  @github  "https://github.com/ponyatov/#{app}"
end
""")

# require M
