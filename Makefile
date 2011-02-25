FLEX = flex
BISON = bison
YFLAGS = -d
CFLAGS = -Wall -ggdb

all: xcrashify

clex.yy.c: clex.l clang.tab.h
	$(FLEX) -o $@ $<

clang.tab.c clang.tab.h: clang.y
	$(BISON) -d $<

clex.yy.o: clex.yy.c clang.tab.h parser.h
clang.tab.o: clang.tab.c parser.h
parser.o: parser.c parser.h clang.tab.h
xform.o: xform.c parser.h clang.tab.h

xcrashify: clex.yy.o clang.tab.o parser.o xform.o
	$(CC) $^ -o $@
