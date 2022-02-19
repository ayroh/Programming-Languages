(defconstant operators (list "+" "-" "/" "(" ")" "*" "\"" "**" "," ";"))
(defconstant DIGIT 0)
(defconstant LETTER 1)
(defconstant OP 2)
(defvar word "")
(defvar Jumped 0)
(defvar isQuoteOpen 0)
(defvar tokens (list))
(defvar tokenValues (list))
(defvar id (list))
(defvar idValues (list))


;;////////////////////////////////////////////////////////////////// lexer

(defun evaluateLine(line)
	(defvar currentType)
	(defvar firstType -1)
	(dotimes (lineIndex (length line)) ;; reads line char by char
		(if (= firstType -1)		   ;; firstType is for controling string by string
			(progn
				(setf firstType (revealType lineIndex line))
				(setf currentType firstType)

			)
			(setf currentType (revealType lineIndex line))
		)
		(cond
			((if (and (equal firstType DIGIT) (equal currentType LETTER)) ;; digit cant contain letter
				(progn
					(write-line "SYNTAX ERROR | DIGIT CAN'T CONTAIN LETTER")
					(quit)
				)
			 )
			)
			((if (not (or (equal currentType DIGIT) (equal currentType LETTER))) ;; currentType is operator
				(progn
					(if (or (equal firstType DIGIT) (equal firstType LETTER))	;; if word contains identifier and operator at the same time "line*"
						(progn				
							(setf Jumped 1)
							(setf lineIndex (- lineIndex 1))
						)
						(progn
							;(write-line (typeOfOperator currentType))
							(typeOfOperator currentType)
							(if (equal currentType "**")						;; contains 2 char
								(progn
									(setf currentType "*")
									(setf lineIndex (+ 1 lineIndex))
								)
							)
							(if (equal currentType ";;")						;; contains 2 char
								(progn
									(return-from evaluateLine 0)				;; return because comment occured
									(setf currentType ";")
									(setf lineIndex (+ 1 lineIndex))
								)
							)
							(setq tokens (append tokens (list currentType)))
							(setq tokenValues (append tokenValues (list (typeOfOperator currentType))))
							(setf firstType -1)               ;; we will read new char
						)
					)
				)
			 )
			)
		)
		(setf lineIndex (jumpBlankSpaces lineIndex line))
		(if (or (equal Jumped 1) (equal (+ lineIndex 1) (length line))) ;; print time
			(progn
				(cond
					((equal firstType DIGIT)
						(progn
							;(write-line "VALUE")
							(setq tokenValues (append tokenValues (list "VALUE")))
						)
					)
					((equal firstType LETTER)
						(progn
							;(write-line (typeOfIdentifier word))
							(setq tokenValues (append tokenValues (list (typeOfIdentifier word))))
						)
					)
				)
				(setf Jumped 0)
				(setf firstType -1)
				(if (not (equal word ""))
					(progn
						(setq tokens (append tokens (list word)))
						(setf word "")
					)
				)
			)
		)


	)
)

(defun typeOfIdentifier (currentWord)
  (cond
    ((equal currentWord "and")
      (return-from typeOfIdentifier "KW_AND")
    )
    ((equal currentWord "or")
      (return-from typeOfIdentifier "KW_OR")
    )
    ((equal currentWord "not")
      (return-from typeOfIdentifier "KW_NOT")
    )
    ((equal currentWord "equal")
      (return-from typeOfIdentifier "KW_EQUAL")
    )
    ((equal currentWord "less")
      (return-from typeOfIdentifier "KW_LESS")
    )
    ((equal currentWord "nil")
      (return-from typeOfIdentifier "KW_NIL")
    )
    ((equal currentWord "list")
      (return-from typeOfIdentifier "KW_LIST")
    )
    ((equal currentWord "append")
      (return-from typeOfIdentifier "KW_APPEND")
    )
    ((equal currentWord "concat")
      (return-from typeOfIdentifier "KW_CONCAT")
    )
    ((equal currentWord "set")
      (return-from typeOfIdentifier "KW_SET")
    )
    ((equal currentWord "deffun")
      (return-from typeOfIdentifier "KW_DEFFUN")
    )
    ((equal currentWord "for")
      (return-from typeOfIdentifier "KW_FOR")
    )
    ((equal currentWord "while")
      (return-from typeOfIdentifier "KW_WHILE")
    )
    ((equal currentWord "if")
      (return-from typeOfIdentifier "KW_IF")
    )
    ((equal currentWord "exit")
      (return-from typeOfIdentifier "KW_EXIT")
    )
    ((equal currentWord "load")
      (return-from typeOfIdentifier "KW_LOAD")
    )
    ((equal currentWord "disp")
      (return-from typeOfIdentifier "KW_DISP")
    )
    ((equal currentWord "true")
      (return-from typeOfIdentifier "KW_TRUE")
    )
    ((equal currentWord "false")
      (return-from typeOfIdentifier "KW_FALSE")
    )
    ((return-from typeOfIdentifier "IDENTIFIER"))
    )
)

(defun typeOfOperator (currentType)
    (cond
      ((equal currentType "+")
        (return-from typeOfOperator "OP_PLUS")
      )
      ((equal currentType "-")
        (return-from typeOfOperator "OP_MINUS")
      )
      ((equal currentType "+")
        (return-from typeOfOperator "OP_PLUS")
      )
      ((equal currentType "/")
        (return-from typeOfOperator "OP_DIV")
      )
      ((equal currentType "*")
        (return-from typeOfOperator "OP_MULT")
      )
      ((equal currentType "(")
        (return-from typeOfOperator "OP_OP")
      )
      ((equal currentType ")")
        (return-from typeOfOperator "OP_CP")
      )
      ((equal currentType "**")
        (return-from typeOfOperator "OP_DBLMULT")
      )
      ((and (equal currentType "\"") (equal isQuoteOpen 0))
      	(progn
      		(setf isQuoteOpen 1)
        	(return-from typeOfOperator "OP_OC")
        )
      )
      ((and (equal currentType "\"") (equal isQuoteOpen 1))
      	(progn
      		(setf isQuoteOpen 0)
        	(return-from typeOfOperator "OP_CC")
        )
      )
      ((equal currentType ",")
        (return-from typeOfOperator "OP_COMMA")
      )
      ((equal currentType ";;")
        (return-from typeOfOperator "COMMENT")
      )
      (
      	(progn
      		(write-line "SYNTAX ERROR UNKNOWN OPERATOR")
      		(quit)
      	)
      )
    )
)

(defun jumpBlankSpaces(index line)
	(setf index (+ index 1))
	(defvar temp index)
	(dotimes (temp (length line))
		(if (and (< (+ index 1) (length line)) (equal (subseq line index (+ index 1)) " "))
			(progn
				(setf index (+ index 1))
				(setf Jumped 1)
			)
			(progn
					(return-from jumpBlankSpaces (- index 1))
				
			)
		)
	)
)

(defun revealType(index line) ;; returns type
	(cond
		 ((alpha-char-p (aref line index))  ;; if letter
		 	(progn
		 		(setf word (concatenate 'string word (subseq line index (+ index 1))))
		 		(return-from revealType LETTER)) ; check if letter a-zA-z
		 	)
		 ((not (null (digit-char-p (aref line index)))) ;; if digit
			 (progn
			 	(setf word (concatenate 'string word (subseq line index (+ index 1))))
			 	(return-from revealType DIGIT)) ; check if digit 0-9
			 )
		 ((return-from revealType (isOperator (subseq line index (+ index 1)) index line))) ; check if negative digit or operator
	)
)

(defun isOperator(tempChar index line)
	(if (and (equal (nth 1 operators) tempChar) (and (not (equal (+ index 1) (length line))) (not (null (digit-char-p (aref line (+ index 1))))))) ;; if negative
		(progn
		 	(setf word (concatenate 'string word (subseq line index (+ index 1))))
			(return-from isOperator DIGIT)
		)
	)
	(if (and (equal (nth 5 operators) tempChar) (and (not (equal (+ index 1) (length line))) (equal (nth 5 operators) (subseq line index (+ index 1))))) ;; if **
		(progn
			(setf tempChar "**")
			(return-from isOperator tempChar)
		)
	)
	(if (and (equal (nth 9 operators) tempChar) (and (not (equal (+ index 1) (length line))) (equal (nth 9 operators) (subseq line index (+ index 1))))) ;; if **
		(progn
			(setf tempChar ";;")
			(return-from isOperator tempChar)
		)
	)
	(dotimes (x (length operators))
		(if(equal (nth x operators) tempChar)  ;; else for other operators
			(return-from isOperator tempChar)
		)
	)
	(write-line "SYNTAX ERROR | THERE IS NO SUCH OPERATOR")
	(quit)

)

(defun readFromFile(filename)	;; reads from file and returns all lines as a list
	(with-open-file (stream filename)
		(loop for line = (read-line stream nil)
			while line
				collect line
		)
	)
)

(defun gppinterpreter(&optional filename)
	(if (equal filename nil)		;; reads from user
		(progn
			(write-line "type 'quit' for end")
			(dotimes (s 100)
				(setf line (read-line))
				(if (equal line "quit")
					(setf s 99)
					(progn
						(evaluateLine line)
						(if (CFGcontrol tokens tokenValues)
							(progn
								(write-line "SYNTAX: OK.")
								(write "Result: ")
								(write (parse tokens tokenValues))
								(setq tokens (list))
								(setq tokenValues (list))
								(terpri)		
							)
							(write-line "SYNTAX ERROR | CONTROL STATEMENTS ARE NOT COMPATIBLE WITH CFG")
						)
						
					)
				)
				(terpri)
			)		
		)
		(progn
			(setf fileline (readFromFile filename))
			(dotimes (x (length fileline))
				(write-line (nth x fileline))
				(evaluateLine (nth x fileline))	
				(if (CFGcontrol tokens tokenValues)
					(progn
						(write-line "SYNTAX: OK.")
						(write "Result: ")
						(write (parse tokens tokenValues))
						(setq tokens (list))
						(setq tokenValues (list))
						(terpri)		
					)
					(write-line "SYNTAX ERROR | CONTROL STATEMENTS ARE NOT COMPATIBLE WITH CFG")
				)
				(terpri)
			)		
		)
	)
)

;;/////////////////////////////////////////////////////////////////// from here it is parser

(defun CFGcontrol(pTokens pTokenValues)	;; CFG for control statements since i control other cfg's through parsing
		(progn
			(setf curToken (nth 1 pTokenValues))
			(cond
				((if (equal curToken "KW_IF")
					(if (and (isEXPB (nth 2 pTokenValues)) (isEXPLISTI (nth 3 pTokenValues)) (or (isEXPLISTI (nth 4 pTokenValues)) (equal (nth 4 pTokenValues) "OP_CP")))
						(return-from CFGcontrol t)
						(return-from CFGcontrol nil)
					)
				))
				((if (equal curToken "KW_WHILE")
					(if (and  (equal (nth 2 pTokenValues) "OP_OP") (isEXPB (nth 3 pTokenValues)) (equal (nth 4 pTokenValues) "OP_CP") (isEXPLISTI (nth 5 pTokenValues)))
						(return-from CFGcontrol t)
						(return-from CFGcontrol nil)
					)
				))

				((if (equal curToken "KW_FOR")
					(if (and  (equal (nth 2 pTokenValues) "OP_OP") (equal (nth 3 pTokenValues) "IDENTIFIER") (isEXPI (nth 4 pTokenValues)) (isEXPI (nth 5 pTokenValues)) (equal (nth 6 pTokenValues) "OP_CP") (isEXPLISTI (nth 7 pTokenValues)))
						(return-from CFGcontrol t)
						(return-from CFGcontrol nil)
					)
				))
			)
			(return-from CFGcontrol t)

		)
)

(defun isEXPB(Token)
	(if (or (equal Token "KW_TRUE") (equal Token "KW_FALSE"))
		(return-from isEXPB t)
		(return-from isEXPB nil)		
	)
)

(defun isEXPLISTI(Token)
	(if (equal Token "KW_LIST")
		(return-from isEXPLISTI t)
		(return-from isEXPLISTI nil)
	)
)

(defun isEXPI(Token)
	(if (or (equal Token "IDENTIFIER") (equal Token "VALUE"))
		(return-from isEXPI t)
		(return-from isEXPB nil)	
	)
)


(defun parse(pTokens pTokenValues)			;; parse token by token
	(let ((x 0) (y 0) (nextstatementx 0) (nextstatementy 0) (curToken) (listx(list)) (listy(list)))
		(if (and (equal (nth 0 pTokenValues) "OP_OP") (equal (nth (- (list-length pTokenValues) 1) pTokenValues) "OP_CP"))
			(progn
				(setf curToken (nth 1 pTokenValues))
				(cond
					((or (equal curToken "OP_PLUS") (equal curToken "OP_MINUS") (equal curToken "OP_MULT") (equal curToken "OP_DIV") (equal curToken "OP_DBLMULT"))
						(progn
							(cond
								((equal (nth 2 pTokenValues) "VALUE")
										(setf x (parse-integer (nth 2 pTokens)))
								)
								((equal (nth 2 pTokenValues) "IDENTIFIER")
										(setf x (idToValue (nth 2 pTokens) t))
								)
								((progn
									(setf nextstatementx (+ 2 (nextStatement (subseq pTokenValues 2))))		;; another statement is on way so parse it first
									(setf x (parse (subseq pTokens 2 nextstatementx) (subseq pTokenValues 2 nextstatementx)))
								)
								)

							)							
							(if (equal nextstatementx 0) (setf nextstatementx 3))
							(cond
								((equal (nth nextstatementx pTokenValues) "VALUE")
										(setf y (parse-integer (nth nextstatementx pTokens)))
								)
								((equal (nth nextstatementx pTokenValues) "IDENTIFIER")
										(setf y (idToValue (nth nextstatementx pTokens) t))
								)
								((progn
									(setf nextstatementy (+ nextstatementx (nextStatement (subseq pTokenValues nextstatementx))))
									(setf y (parse (subseq pTokens nextstatementx nextstatementy) (subseq pTokenValues nextstatementx nextstatementy)))
								))

							)

							
							(cond
								((equal curToken "OP_PLUS") (return-from parse (+ x y)))
								((equal curToken "OP_MINUS") (return-from parse (- x y)))
								((equal curToken "OP_MULT") (return-from parse (* x y)))
								((equal curToken "OP_DIV") (return-from parse (/ x y)))
								((equal curToken "OP_DBLMULT") (return-from parse (expt x y))) 		

							)
						)
					)

					((and (equal curToken "KW_SET") (equal (nth 2 pTokenValues) "IDENTIFIER"))
						(progn
							(setq id (append id (list (nth 2 pTokens))))
							(cond
								((or (equal (nth 3 pTokenValues) "VALUE") (equal (nth 3 pTokenValues) "KW_FALSE") (equal (nth 3 pTokenValues) "KW_TRUE")) ;; set value,true or false
									(setq idValues (append idValues (list (parse-integer (nth 3 pTokens))))) 
								)
								((equal (nth 3 pTokenValues) "IDENTIFIER")
									(setq idValues (append idValues (list (idToValue (nth 3 pTokens))))			;; set another identifier
								))
								((progn										
									  (setq idValues (append idValues (list (parse (subseq pTokens 3) (subseq pTokenValues 3)))))  ;; parse another statement first
								))
							)
						)
					)

					((or (equal curToken "KW_AND") (equal curToken "KW_OR") (equal curToken "KW_EQUAL") (equal curToken "KW_LESS")) ;; binary operations
						(progn
							(cond
								((equal (nth 2 pTokenValues) "VALUE") (setf x (parse-integer (nth 2 pTokens))))
								((equal (nth 2 pTokenValues) "IDENTIFIER") (setf x (idToValue (nth 2 pTokens) t)))
								((equal (nth 2 pTokenValues) "KW_TRUE") (setf x t))
								((equal (nth 2 pTokenValues) "KW_FALSE") (setf x nil))
								((progn
									(setf nextstatementx (+ 2 (nextStatement (subseq pTokenValues 2))))
									(setf x (parse (subseq pTokens 2 nextstatementx) (subseq pTokenValues 2 nextstatementx)))
								))
							)
							(if (equal nextstatementx 0) (setf nextstatementx 3))
							(cond
								((equal (nth nextstatementx pTokenValues) "VALUE") (setf y (parse-integer (nth nextstatementx pTokens))))
								((equal (nth nextstatementx pTokenValues) "IDENTIFIER") (setf y (idToValue (nth nextstatementx pTokens) t)))
								((equal (nth nextstatementx pTokenValues) "KW_TRUE") (setf y t))
								((equal (nth nextstatementx pTokenValues) "KW_FALSE") (setf y nil))
								((progn
									(setf nextstatementy (+ nextstatementx (nextStatement (subseq pTokenValues nextstatementx))))
									(setf y (parse (subseq pTokens nextstatementx nextstatementy) (subseq pTokenValues nextstatementx nextstatementy)))
								))
							)
							(cond
								((equal curToken "KW_AND") (return-from parse (and x y)))
								((equal curToken "KW_OR") (return-from parse (or x y)))
								((equal curToken "KW_EQUAL") (return-from parse (equal x y)))
								((equal curToken "KW_LESS") (return-from parse (< x y)))
							)
						)


					)

					((equal curToken "KW_NOT")
						(progn
							(cond
								((equal (nth 2 pTokenValues) "VALUE") (setf x (parse-integer (nth 2 pTokens))))
								((equal (nth 2 pTokenValues) "IDENTIFIER")
									(progn
								 		(setf temp (idToValue (nth 2 pTokens) t)))
										(if (equal temp "false")
											(setf x nil)
											(setf x t)
										)
									)
								((equal (nth 2 pTokenValues) "KW_TRUE") (setf x t))
								((equal (nth 2 pTokenValues) "KW_FALSE") (setf x nil))
							)
							(return-from parse (not x))
						)
					)

					((equal curToken "KW_LIST")
						(progn
							(setf (nth 1 pTokens) "(") ;; for parse between brackets since end of list statement there will be close bracket also
							(setf (nth 1 pTokenValues) "OP_OP")
							(setf nextstatementx (nextStatement (subseq pTokenValues 1)))
							(return-from parse (newList (subseq pTokens 1 nextstatementx) (subseq pTokenValues 1 nextstatementx)))
						)
					)
					((equal curToken "KW_APPEND")		;; append value or identifier to list
						(progn
							(cond
								((equal (nth 2 pTokenValues) "VALUE")
									(setf x (parse-integer (nth 2 pTokens)))
								)
								((equal (nth 2 pTokenValues) "IDENTIFIER")
									(progn
										(if (typep (idToValue (nth 2 pTokens) t) 'integer) (setq x (idToValue (nth 2 pTokens) t)))
										(if (typep (idToValue (nth 2 pTokens) t) 'list) (setq x (idToValue (nth 2 pTokens) t)))
									)
								)
								((progn
									(setf nextstatementx (+ 2 (nextStatement (subseq pTokenValues 2))))
									(setf x (parse (subseq pTokens 2 nextstatementx) (subseq pTokenValues 2 nextstatementx)))
								 )
								)
							)
							(if (equal nextstatementx 0) (setf nextstatementx 3))
							(setq listx (parse (subseq pTokens nextstatementx) (subseq pTokenValues nextstatementx)))
							(if (equal listx nil)
								(progn
									(write "SYNTAX ERROR | APPEND METHOD ONLY WORKS WITH (EXPI EXPLISTI)")
									(quit)
								)
							)

							(if (equal (nth 2 pTokenValues) "IDENTIFIER")
								(if (typep (idToValue (nth 2 pTokens) t) 'integer)
									(return-from parse (append listx (list x)))
									(return-from parse (append listx x))
								)
							)
							(return-from parse (append listx (list x)))

						)
					)
					((equal curToken "KW_CONCAT")		;; append list to another list
						(progn
							(setq listx (parse (subseq pTokens 2) (subseq pTokenValues 2)))
							(setf nextstatementx (+ (nextStatement (subseq pTokenValues 2)) 2))
							(setq listy (parse (subseq pTokens nextstatementx) (subseq pTokenValues nextstatementx)))
							(return-from parse (append listx listy))
						)
					)

					((equal curToken "KW_DISP")
						(cond
							((equal (nth 2 pTokenValues) "VALUE") (return-from parse (parse-integer (nth 2 pTokens))))
							((equal (nth 2 pTokenValues) "IDENTIFIER") (return-from parse (idToValue (nth 2 pTokens))))
							((return-from parse (parse (subseq pTokens 2) (subseq pTokenValues 2))))
						)
					)


					((equal curToken "IDENTIFIER")
						(progn
							(write "SYNTAX ERROR | WRONG KEYWORD")
							(quit)	
						)
					)

				)

			)
		)
	)
)

(defun nextStatement(pTokenValues) ;; if another statement occurs between statements this function returns next statements position
	(defvar counter 0)
	(dotimes (x (length pTokenValues))
		(cond
			((equal (nth x pTokenValues) "OP_OP") (setf counter (+ counter 1)))
			((equal (nth x pTokenValues) "OP_CP") (setf counter (- counter 1)))
		)
			(if (equal counter 0) (return-from nextStatement (+ x 1)))
	)
)

(defun idToValue(tempID &optional(end nil)) ;; searches through identifiers and return its value
	(dotimes (a (length id))
		(if (equal (nth a id) tempID) (return-from idToValue (nth a idValues))))
	(if (equal end t)
		(progn
			(write "SYNTAX ERROR | ID NOT RECOGNIZED")
			(quit)
		)
		(return-from idToValue -1)
	)
)

(defun newList(pTokens pTokenValues)  ;; creates new list between brackets
	(let ((tempList (list)))
		(loop for i from 1 to (- (length pTokens) 1)
			do
			(cond
				((equal (nth i pTokenValues) "VALUE")
					(setq tempList (append tempList (list (parse-integer (nth i pTokens)))))
				)
				((equal (nth i pTokenValues) "IDENTIFIER")
					(setq tempList (append tempList (list (parse-integer (idToValue (nth i pTokens) t)))))
				)
				((progn
					(write (nth i pTokens))
					(write "SYNTAX ERROR | THERE HAS TO BE VALUE OR IDENTIFIER AT LIST")
					(quit)
				 )
				)
			)
		)
		(return-from newList tempList)
	)
)


(write-line "Type F for file reading, L for line reading")
(setf choice (read-line))
(if(equal choice "f")
	(progn
		(write-line "Enter filename:")
		(terpri)
		(setf filename (read-line))
		(gppinterpreter filename)
	)
	(gppinterpreter)
)
(write tokens)
(terpri)
(write tokenValues)
