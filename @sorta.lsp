;################################################################################################
;Name: 	Nicholas Flanders		    Program:    @sorta				Due: 9/2/15
;Contract:	A-  	    Course:	CSCI 220			Prof:	John Broere
;	Reasoning: Added three additional functions including percentage, int-exponent, and line-calc.
;				Provided complete documentation with pre/post conditions, EBNF, and line comments.
;				Provided complete testing demonstration through use of Dribble. 
;
;Description: A basic calculator program that allows a user to enter lisp style input expressions
;				and then receive the answer to the input expressions.
;				Calculates addition, subtraction, multiplication, division, integer exponents,
;				percent discount, and a value table for a line.
;
;################################################################################################

(trace @eval + - / * ^) ;Remove comment to trace eval function

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;@sorta is the basic loop that receives input from the user,
; and then prints out the results of that input.
;
;Special Thanks to:
;
;
;pre: none
;post: the value of the input is displayed.  If the input is
; "quit" the program terminates.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun @sorta ()
	(princ ;print the input info prompt
	"\n<input>      ::=     <expression> | quit\n<expression> ::=     <integer> | (<optr> <expression> <expression>)\n<optr>       ::=     + | - | * | / | ^ | % | =\n<integer>    ::=     [-] <digit> { <digit> }\n<digit>      ::=     0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9\n\n"
	)
	(loop
		(princ "Æ-> " )		;print prompt to screen
		(setf now (read))	;retrieve input
		(cond
			( (equal now 'quit) 	(return)) ;if quit then quit, else evaluate express
			( t			(print (@eval now)))
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;@eval
;
;
;pre:  a list of user input has been provided
;post: a value has been returned, or an answer has been printed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun @eval (theList)
	(cond
		((numberp theList)         theList)
		((equal '+ (first theList))     (+ (@eval(@second theList)) (@eval(@third theList))))
		((equal '- (first theList))     (- (@eval(@second theList)) (@eval(@third theList))))
		((equal '* (first theList))     (* (@eval(@second theList)) (@eval(@third theList))))
		((equal '/ (first theList))     (/ (@eval(@second theList)) (@eval(@third theList))))
		;((equal '& (first theList))     (* (@eval(@second theList)) (@eval(@second theList)))) ;square a number    ;removed because only takes one input
		((equal '^ (first theList))     (@power (@eval(@second theList)) (@eval(@third theList)))) ; exponent
		((equal '% (first theList))     (@eval(@percent theList)))                                ; percent (% <expression(init price)> <expression(discount percent)>)
		((equal '= (first theList))     (@line (@eval(@second theList)) (@eval(@third theList)))) ; line calc (= <expression(slope)> <expression(y-intercept)>)
	)
)

	



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;@second simply finds the second atom within a list and returns it
;
;
;pre:  a list has been provided
;post: the second element of the list has been returned
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun @second (aList)
	(first (rest aList))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;@third simply finds the third element within a list and returns it
;
;
;pre:   a list has been provided
;post:  the third element of list has been returned
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun @third (aList)
	(first(rest(rest aList)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;@square returns the square of a number. x^2
;
;
;pre:   a list with one element has been provided
;post:  the product of the number times itself has been returned
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun @square (aNum)
	(* (@eval(first aNum)) (@eval(first aNum)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;@percent calculates the new price of a discounted item
;
;
;pre:   a list has been provided. (% <initial number> <percent off (0-100)>)
;post:  the answer has been returned
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun @percent (aList)
	(* (@eval(@second aList)) (/ (@eval(@third aList)) 100))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;@power calculates and returns the result of x^y
;			resursively calls itself, until exponent reaches 0
;
;pre:   two numbers have been passed into the function, a base and an exponent
;post:  the value of result has been returned
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun @power (base expo)
	(cond
		((equal expo 0) 1)  ;break case
		(t                      (* base (@power base (- expo 1))))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;@line creates an x-y value table for a provided slope and y-intercept of a line
;		tracing is temperarily turned off to avoid disruption in the printing
;
;pre:   two parameters, slope and y-intercept have been provided
;post:  the third element of list has been returned
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun @line (m b)
	(untrace + - * / ^)    ; researched if it was possible to turn off tracing. 
							; found answer at "http://clhs.lisp.se/Body/m_tracec.htm"
					; prints table
	(princ "\n")
	(princ "y = mx + b")
	(princ "   m=")
	(princ m)
	(princ "   b=")
	(print b)
	(princ "x     y\n0     ")
	(print (+ (* m 0) b))
	(princ "5     ")
	(print (+ (* m 5) b))
	(princ "10    ")
	(print (+ (* m 10) b))
	(princ "\n")
	(trace + - * / ^)   ;resumes tracing
)



(@sorta)