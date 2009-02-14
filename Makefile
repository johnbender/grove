MODULES   	= grove_mnesia grove grove_appmod grove_util grove_mnesia_ops grove_custom smerl 
BEAMS     	= $(MODULES:%=%.beam)

BINDIR    = ebin
SRCDIR    = src
INCDIR    = include
TESTDIR   = test
VPATH     = $(BINDIR):$(SRCDIR):$(INCDIR):$(TESTDIR)

ERL       = erl
ERLC      = erlc
ERLCFLAGS = -W -smp

all: $(BEAMS)

test: ERLCFLAGS += -DTEST
test: clean $(BEAMS)

%.beam: %.erl
		$(ERLC) -b beam $(ERLCFLAGS) -I $(INCDIR) -o $(BINDIR) $<

clean:
		rm -rf $(BINDIR)/*.beam		s

