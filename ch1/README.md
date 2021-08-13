
Based on the book "Programming Languages: An Interpreter-Based Approach" by Samuel N. Kamin

This dir is my attempt at implementing the toy language from Chapter 1 of the book.

To run the interpreter as a repl, run

```
cabal run ch1
```

Enter

```
quit
```

to quit the repl.


To run the interpreter on an an input string, run

```
cabal run ch1 -- <your_expression_here>
```


Language Syntax

Syntax for this language exist as the following from the book:

``` 
input -> expression | fundef

fundef -> ( define function arglist expression )

arglist -> ( variable* )

expression -> value
            | variable
            | ( if expression expression expression )
            | ( while expression expression )
            | ( set variable expression )
            | ( begin expression+ )
            | ( optr expression* )

optr -> function | value-op

value -> integer

value-op -> + | * | / | = | < | > | print

function -> name

variable -> name

integer -> -{0,1}[0-9]+ # A sequence of digits that may or may not begin with a -

name -> Any sequence of characters that's not an integer and not containing a blank or: ( ) ;

comment -> Begin with ; and continues until end of the line

```



