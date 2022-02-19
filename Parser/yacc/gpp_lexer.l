%{
	#include <stdio.h>	
	#include "y.tab.h"

	int isQuoteOpen = 0;
	int lastToken = 0;


%}

DIGIT [0-9]
ID [a-zA-Z][_a-zA-Z0-9]*

%option noyywrap

%%

"and" return KW_AND;

"or" return KW_OR;

"not" return KW_NOT;

"equal" return KW_EQUAL;

"less" return KW_LESS;

"nil" return KW_NIL;

"list" return KW_LIST;

"append" return KW_APPEND;

"concat" return KW_CONCAT;

"set" return KW_SET;

"deffun" return KW_DEFFUN;

"for" return KW_FOR;

"if" return KW_IF;

"exit" return KW_EXIT;

"load " return KW_LOAD;

"disp " return KW_DISP;

"true" return KW_TRUE;

"false" return KW_FALSE;

")" return OP_CP;

"("	return OP_OP;

"+"	return OP_PLUS;

"-"	return OP_MINUS;

"*"	return OP_MULT;

"/"	return OP_DIV;

"**" return OP_DBLMULT;

"\"" {
	if(isQuoteOpen == 0){
		isQuoteOpen = 1;
		return OP_OC;
	}
	else if(isQuoteOpen == 1){
		isQuoteOpen = 0;
		return OP_CC;
	}
}

"," return OP_COMMA;

;;.*\n	return COMMENT;

"-"{DIGIT}+"."{DIGIT}+  {
						yylval.value = atoi(yytext);
						return VALUE;
						}	// negative float

{DIGIT}+"."{DIGIT}+ 	{
						yylval.value = atoi(yytext);
						return VALUE;
						}		// positive float

"-"{DIGIT}+ 			{
						yylval.value = atoi(yytext);
						return VALUE;
						};			// negative integer

{DIGIT}+ 				{
						yylval.value = atoi(yytext);
						return VALUE;
						};			// positive integer

{ID} 					{
						yylval.name = strdup(yytext);
						return IDENTIFIER;
						}



%%



