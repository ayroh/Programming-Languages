(defconstant operators (list "+" "-" "/" "(" ")" "*" "\"" "**" "," ";"))
(defconstant DIGIT 0)
(defconstant LETTER 1)
(defconstant OP 2)
(defvar word "")
(defvar Jumped 0)
(defvar isQuoteOpen 0)


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
							(write-line (typeOfOperator currentType))
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
					((equal firstType DIGIT) (write-line "VALUE"))
					((equal firstType LETTER) (write-line (typeOfIdentifier word)))
				)
				(setf Jumped 0)
				(setf firstType -1)
				(setf word "")
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
					(evaluateLine line)
				)
				(terpri)
			)		
		)
		(progn
			(setf fileline (readFromFile filename))
			(dotimes (x (length fileline))
				(write-line (nth x fileline))
				(evaluateLine (nth x fileline))	
				(terpri)
			)		
		)
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
