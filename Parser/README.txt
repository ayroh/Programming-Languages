The way i did it:

FOR PART 1:
	flex gpp_lexer.l
	yacc -d gpp_interpreter.y
	gcc lex.yy.c y.tab.c -w -lm
	./a.out

	or since i send all files already

	./a.out

FOR PART 2:

	clisp gpp_interpreter.lisp
