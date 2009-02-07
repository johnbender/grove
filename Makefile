MODULES   = grove grove_appmod grove_util grove_mnesia grove_mnesia_ops grove_custom smerl 
BEAMS     = $(MODULES:%=%.beam)
TEST 			= $(MODULES:%=%.beam)

BINDIR    = ebin
SRCDIR    = src
INCDIR    = include
TESTDIR 	= test
VPATH     = $(BINDIR):$(SRCDIR):$(INCDIR):$(TESTDIR)

ERL       = erl
ERLC      = erlc
ERLCFLAGS = -W -smp
DOCFLAGS = 

all: $(BEAMS)

test: ERLCFLAGS += -DTEST
test: $(BEAMS) test_mnesia.beam 

%.beam: %.erl
		$(ERLC) -b beam $(ERLCFLAGS) -I $(INCDIR) -o $(BINDIR) $<

clean:
		rm -rf $(BINDIR)/*.beam		s

