the way i did it:

For Lisp: clisp gpp_lexer.lisp 

-file.txt is instance for file reading with lisp

For Flex: flex gpp_lexer.l(already did it for changing name of lex.yy.c to gpp_lexer.c ) 
		  cc gpp_lexer.c main.c -lfl
		  ./a.out