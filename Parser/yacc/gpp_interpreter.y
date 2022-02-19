%{
	#include <stdio.h>
	#include <stdlib.h>
	#include <math.h>
	#include <string.h>
	extern int yylex();
	int yyerror(char *msg);

	struct variable{ 
		int type;		// int 1, binary 2
		char name[20];
		int value;
	};

	struct function{
		char name[20];
	};

	struct variable variables[100];
	struct variable findVariable(char* name);
	int numberOfVariables = 0;
	int tempCounter = 0;
	struct function functions[100];
	int numberOfFunctions = 0;

%}

%union {
	struct{
		int value;
		char* name;
		int values[100];
	};
}


%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL KW_LIST KW_APPEND KW_CONCAT KW_SET KW_DEFFUN KW_DEFVAR KW_FOR KW_WHILE KW_IF KW_EXIT KW_LOAD KW_DISP KW_TRUE KW_FALSE 
%token OP_PLUS OP_MINUS OP_DIV OP_DBLMULT OP_MULT OP_OP OP_CP OP_AP OP_OC OP_CC OP_COMMA
%token VALUE COMMENT IDENTIFIER
%start START

%%

	START : | START INPUT
	INPUT :  EXPI{
				printf("SYNTAX: OK\n");
				if($<value>1 != -999)
					printf("Result: %d\n",$<value>1);
			}
			| EXPB{
				printf("SYNTAX: OK\n");
				if($<value>1 == 1)
					printf("Result: true\n");
				else
					printf("Result: false\n");

			}

			| EXPLISTI{
				printf("SYNTAX: OK\n");
				int i ;
				for(i = 0;i < $<value>1; i++)
					printf("%d ",$<values[i]>1);
					printf("\n");

			}

	EXPI: OP_OP OP_PLUS EXPI EXPI OP_CP {$<value>$ = $<value>3 + $<value>4;}
		| OP_OP OP_MINUS EXPI EXPI OP_CP {$<value>$ = $<value>3 - $<value>4;}
		| OP_OP OP_MULT EXPI EXPI OP_CP {$<value>$ = $<value>3 * $<value>4;}
		| OP_OP OP_DIV EXPI EXPI OP_CP {$<value>$ = $<value>3 / $<value>4;}
		| OP_OP OP_DBLMULT EXPI EXPI OP_CP {$<value>$ = pow($<value>3, $<value>4);}
		| VALUE {$<value>$ = $<value>1;}
		| IDENTIFIER {
				struct variable tempVariable = findVariable($<name>1);
				if(tempVariable.type == 0){
					printf("SYNTAX ERROR | IDENTIFIER NOT RECOGNIZED\n");
					exit(0);
				}
				if(tempVariable.type == 1){
					$<value>$ = tempVariable.value;
				}
				else{
					printf("SYNTAX ERROR | IT IS NOT AN INTEGER VALUE");
				}
			}

		| OP_OP KW_SET IDENTIFIER EXPI OP_CP {
				addVal(1, $<name>3, $<value>4);
				$<value>$ = $<value>4;
			}

		| OP_OP KW_DEFVAR IDENTIFIER EXPI OP_CP {
				addVal(1, $<name>3, $<value>4);
			}


		| OP_OP KW_DEFFUN IDENTIFIER OP_OP IDS OP_CP EXPLISTI OP_CP {
				newFunction($<name>3);
			}

		| OP_OP IDENTIFIER EXPLISTI OP_CP {
				if(findFunction($<name>2)){
					printf("%s()\n",$<name>2);
					$<value>$ = -999;
				}
				else{
					printf("SYNTAX ERROR | FUNCTION NOT RECOGNIZED\n");
					exit(0);	
				}

		}

		| OP_OP KW_IF EXPB EXPLISTI OP_CP
		| OP_OP KW_IF EXPB EXPLISTI EXPLISTI OP_CP
		| OP_OP KW_WHILE OP_OP EXPB OP_CP EXPLISTI OP_CP 
		| OP_OP KW_FOR OP_OP IDENTIFIER EXPI EXPI OP_CP EXPLISTI OP_CP 
		| OP_OP KW_DISP EXPI OP_CP {$<value>$ = $<value>3;}

	EXPB: OP_OP KW_AND EXPB EXPB OP_CP {$<value>$ = $<value>3 && $<value>4;}
		| OP_OP KW_OR EXPB EXPB OP_CP {$<value>$ = $<value>3 || $<value>4;}
		| OP_OP KW_NOT EXPB OP_CP {if($<value>3 == 0) $<value>$ = 1; else $<value>$ = 0;}
		| OP_OP KW_EQUAL EXPB EXPB OP_CP {if($<value>3 == $<value>4) $<value>$ = 1; else $<value>$ = 0;}
		| OP_OP KW_LESS EXPB EXPB OP_CP {if($<value>3 < $<value>4) $<value>$ = 1; else $<value>$ = 0;}
		| IDENTIFIER {
				struct variable tempVariable = findVariable($<name>1);
				if(tempVariable.type == 0){
					printf("SYNTAX ERROR | IDENTIFIER NOT RECOGNIZED\n");
					exit(0);
				}
				if(tempVariable.type == 2){
					$<value>$ = tempVariable.value;
				}
				else{
					printf("SYNTAX ERROR | IT IS NOT A BINARY VALUE\n");
				}
			}
		| VALUE {$<value>$ = $<value>1;}
		| KW_TRUE {$<value>$ = 1;}
		| KW_FALSE {$<value>$ = 0;}
		| KW_NIL {$<value>$ = 0;}
		| OP_OP KW_SET IDENTIFIER EXPB OP_CP {
			addVal(2, $<name>3, $<value>4);
		}

	EXPLISTI: OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP {
					int i;
					int firstSize = $<value>3;
					int secondSize = $<value>4;
					for(i = 0;i < firstSize; i++)
						$<values[i]>$ = $<values[i]>3;
					for(i = 0;i < secondSize; i++)
						$<values[i + firstSize]>$ = $<values[i]>4;
					$<value>$ = firstSize + secondSize;
				}

			| OP_OP KW_APPEND EXPI EXPLISTI OP_CP {
					int i;
					int size = $<value>4;
					for(i = 0; i < size; i++)
						$<values[i]>$ = $<values[i]>4;
					$<values[size]>$ = $<value>3;
					$<value>$ = size + 1;
				}

			/*| OP_OP KW_SET IDENTIFIER LISTVALUE OP_CP {
					addList($<name>3 , $<value>4, $<values>4);
				}*/

			| OP_OP KW_DISP EXPLISTI OP_CP {
					int i, size = $<value>3;
					for(i = 0; i < size; i++)
						$<values[i]>$ = $<values[i]>3;
					$<value>$ = size;
				}

			| LISTVALUE {
					int i;
					for(i = 0; i < $<value>1; i++)
						$<values[i]>$ = $<values[i]>1;
				}

			LISTVALUE: OP_OP KW_LIST VALUES OP_CP  {int i; for(i = 0; i < tempCounter; i++) $<values[i]>$ = $<values[i]>3; $<value>$ = tempCounter; tempCounter = 0;}

			VALUES: VALUES VALUE {$<values[tempCounter]>$ = $<value>2; tempCounter++;}
								
				  | VALUE	{$<values[tempCounter]>$ = $<value>1; tempCounter++;}

			IDS: IDS IDENTIFIER | IDENTIFIER



%%

void newFunction(char* name){
	int i;
	for(i = 0;i < numberOfFunctions; i++)
		if(!strcmp(functions[i].name, name))
		return;
	strcpy(functions[numberOfFunctions].name,name);
	numberOfFunctions++;
}

int findFunction(char* name){
	int i;
	for(i = 0;i < numberOfFunctions; i++)
		if(!strcmp(functions[i].name, name))
			return 1;
	return 0;
}


struct variable findVariable(char* name)
{	
	int i;
	for(i = 0; i < numberOfVariables; i++)
		if(!strcmp(variables[i].name,name)) return variables[i];
	return;
}

void addVal(int type,char* name , int value)
{	
	int i;
	strcpy(variables[numberOfVariables].name, name);
	variables[numberOfVariables].value = value;
	variables[numberOfVariables].type = type;
	numberOfVariables++;
}

int yyerror(char *msg) 
{ 
	printf("SYNTAX ERROR | EXPRESSION NOT RECOGNIZED\n");
	exit(0); 
}


int main(){
	yyparse();
	return 0;
}