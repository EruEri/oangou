PREFIX=/usr/local
BINDIR=$(PREFIX)/bin
MANDIR=$(PREFIX)/share/man

all:
	dune build

clean: 
	dune clean

install:
	mkdir -p $(BINDIR)
	mkdir -p $(MANDIR)/man1
	install -m 755 _build/default/bin/oangou.exe $(BINDIR) 
	mv $(BINDIR)/oangou.exe $(BINDIR)/oangou
	install -m 644 $(shell ls -x _build/default/man/*.1) $(MANDIR)/man1
