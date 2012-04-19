CC = gcc
CFLAGS = -Wall -ggdb
FLEX = flex
BISON = bison
YFLAGS = -d

PROGRAM=xcrashify

OBJS = \
	clex.yy.o \
	clang.tab.o \
	parser.o \
	tools.o \
	varscope.o \
	indirect.o \
	xform.o

all: $(PROGRAM)

clex.yy.c: clang.tab.h

$(PROGRAM): $(OBJS)
	$(call mcmd,link)

clean:
	rm -rf $(PROGRAM) $(OBJS)

-include Makefile.lib
