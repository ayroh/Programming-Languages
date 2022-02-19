%{
	#include <stdio.h>	

	#define KW_AND 0
	#define KW_OR 1
	#define KW_NOT 2
	#define KW_EQUAL 3
	#define KW_LESS 4
	#define KW_NIL 5
	#define KW_LIST 6
	#define KW_APPEND 7
	#define KW_CONCAT 8
	#define KW_SET 9
	#define KW_DEFFUN 10
	#define KW_FOR 11
	#define KW_IF 12
	#define KW_EXIT 13
	#define KW_LOAD 14
	#define KW_DISP 15
	#define KW_TRUE 16
	#define KW_FALSE 17
	#define OP_PLUS 18
	#define OP_MINUS 19
	#define OP_DIV 20
	#define OP_DIV2 21
	#define OP_MULT 22
	#define OP_OP 23 
	#define OP_CP 24
	#define OP_DBLMULT 25 
	#define OP_OC 26 
	#define OP_CC 27 
	#define OP_COMMA 28
	#define COMMENT 29
	#define VALUE 30
	#define IDENTIFIER 31

	int isQuoteOpen = 0;
	int lastToken = 0;


%}

DIGIT [0-9]
ID [a-zA-Z][_a-zA-Z0-9]*
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

"load" return KW_LOAD;

"disp" return KW_DISP;

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

"-"{DIGIT}+"."{DIGIT}+ return VALUE;	// negative float

{DIGIT}+"."{DIGIT}+ return VALUE;		// positive float

"-"{DIGIT}+ return VALUE;			// negative integer

{DIGIT}+ return VALUE;			// positive integer

{ID} return IDENTIFIER;



%%



