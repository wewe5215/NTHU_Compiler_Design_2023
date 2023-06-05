FLEX = flex
BYACC = byacc
YACC = yacc
CC = gcc
DEBUG = 
LDFLAGS = -lfl

all: ytab scanner parser

ytab: parser.y
	${BYACC} -d parser.y

conflict: parser.y
	${YACC} -v -Wcounterexamples $^

scanner: scanner.l y.tab.h 
	${FLEX} -o lex.yy.c $<

parser: lex.yy.c y.tab.c
	$(CC) -o parser lex.yy.c y.tab.c -lfl

.PHONY: clean

clean:
	rm -f lex.yy.c y.tab.h y.tab.c y.output parser 
