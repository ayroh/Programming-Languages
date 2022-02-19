#include <stdio.h>

int yylex();

int main(void)
{
	while(1){
		int token = yylex();
		if(token == 0) printf("KW_AND\n");
		else if(token == 1) printf("KW_OR\n");
		else if(token == 2) printf("KW_NOT\n");
		else if(token == 3) printf("KW_EQUAL\n");
		else if(token == 4) printf("KW_LESS\n");
		else if(token == 5) printf("KW_NIL\n");
		else if(token == 6) printf("KW_LIST\n");
		else if(token == 7) printf("KW_APPEND\n");
		else if(token == 8) printf("KW_CONCAT\n");
		else if(token == 9) printf("KW_SET\n");
		else if(token == 10) printf("KW_DEFFUN\n");
		else if(token == 11) printf("KW_FOR\n");
		else if(token == 12) printf("KW_IF\n");
		else if(token == 13) printf("KW_EXIT\n");
		else if(token == 14) printf("KW_LOAD\n");
		else if(token == 15) printf("KW_DISP\n");
		else if(token == 16) printf("KW_TRUE\n");
		else if(token == 17) printf("KW_FALSE\n");
		else if(token == 18) printf("OP_PLUS\n");
		else if(token == 19) printf("OP_MINUS\n");
		else if(token == 20) printf("OP_DIV\n");
		else if(token == 21) printf("OP_DIV2\n");
		else if(token == 22) printf("OP_MULT\n");
		else if(token == 23) printf("OP_OP\n");
		else if(token == 24) printf("OP_CP\n");
		else if(token == 25) printf("OP_DBLMULT\n");
		else if(token == 26) printf("OP_OC\n");
		else if(token == 27) printf("OP_CC\n");
		else if(token == 28) printf("OP_COMMA\n");
		else if(token == 29) printf("COMMENT\n");
		else if(token == 30) printf("VALUE\n");
		else if(token == 31) printf("IDENTIFIER\n");
	}
}
