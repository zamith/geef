# Find rebar3 or download it if missing
REBAR=$(shell which rebar3 || echo "./rebar3")

# Detect Erlang installation via Homebrew
ERL_VERSION ?= 27.2.1
ERL_PREFIX ?= /opt/homebrew/Cellar/erlang/$(ERL_VERSION)/lib/erlang
ERL_INCLUDE_PATH ?= $(ERL_PREFIX)/erts-15.2.1/include
ERL_LIB_PATH ?= $(ERL_PREFIX)/usr/lib

# Detect libgit2 installation via Homebrew
LIBGIT2_CFLAGS   ?= $(shell pkg-config --cflags libgit2 2>/dev/null || echo "-I/opt/homebrew/include")
LIBGIT2_LDFLAGS  ?= $(shell pkg-config --libs libgit2 2>/dev/null || echo "-L/opt/homebrew/lib -lgit2")

CFLAGS += -I$(ERL_INCLUDE_PATH) $(LIBGIT2_CFLAGS)
LDFLAGS += -L$(ERL_LIB_PATH)

CFLAGS += -std=c11 -Wall -fPIC -O3 -c

# macOS specific flags
UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
  LDFLAGS  += -flat_namespace
endif

LDFLAGS  += $(ERL_LDFLAGS) $(LIBGIT2_LDFLAGS) -shared

SOURCES := $(wildcard c_src/*.c)
OBJECTS = $(addsuffix .o, $(basename $(SOURCES)))

COMPILE_C = cc $(CFLAGS)

# Default target
all: compile

# Ensure rebar3 is available
$(REBAR):
	@echo "Downloading rebar3..."
	@curl -fsSL https://s3.amazonaws.com/rebar3/rebar3 -o rebar3
	@chmod +x rebar3

# Compile the NIF (C extension)
compile_source: $(OBJECTS)
	@echo "Compiling NIF (Native Implemented Function)..."
	cc -v -dynamiclib -undefined dynamic_lookup $(OBJECTS) -o priv/geef.so $(LDFLAGS)

%.o: %.c $(wildcard *.h)
	$(COMPILE_C) -o $@ $<

# Compile Elixir/Erlang code
compile: $(REBAR) compile_source
	@echo "Compiling Elixir/Erlang sources..."
	@$(REBAR) compile

# Clean build artifacts
clean: $(REBAR)
	@echo "Cleaning project..."
	@$(REBAR) clean
	@rm -rf priv/geef.so rebar3
	@rm -rf c_src/*.o

# Run tests
test: compile
	@echo "Running tests..."
	@$(REBAR) eunit

# Define phony targets (not actual files)
.PHONY: all compile_source compile clean test
