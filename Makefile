MODULES   = grove grove_appmod grove_util grove_mnesia smerl 

BEAMS     = $(MODULES:%=%.beam)

BINDIR    = ebin
SRCDIR    = src
INCDIR    = include
VPATH     = $(BINDIR):$(SRCDIR):$(INCDIR)

ERL       = erl
ERLC      = erlc
ERLCFLAGS = -W -smp

all: $(BEAMS)

%.beam: %.erl
		$(ERLC) -b beam $(ERLCFLAGS) -I $(INCDIR) -o $(BINDIR) $< 
	
clean:
		rm -rf $(BINDIR)/*.beam		
