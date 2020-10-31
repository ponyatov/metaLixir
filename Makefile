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

EX  += $(LIB)/metalixir.ex
ERL += $(SRC)/metalixir.erl

.PHONY: all
all: $(EX)

.PHONY: repl
repl:
	$(IEX) -S mix

# rules

$(LIB)/%.ex: $(LIB)/%.exs

# install

.PHONY: install update
install:
	make $(OS)_install
update:
	make $(OS)_update
.PHONY: Linux_install Linux_update
Linux_install Linux_update:
	sudo apt update
	sudo apt install -u `cat apt.txt`

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

