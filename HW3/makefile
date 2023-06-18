parser:
	flex scanner.l
	byacc -d parser.y
	gcc -o parser lex.yy.c y.tab.c -lfl
clean:
	rm lex.yy.c
	rm y.tab.h y.tab.c
	rm parser 
do: 
	parser clean
