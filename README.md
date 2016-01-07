GOG
===
Experimental language with polynotational semantics
-----

# Installation

1. Get gog and place it where you want
2. Install rlwrap and racket. Assuming you have ubuntu:

    sudo apt-get install racket rlwrap

3. Add gog to your path

    sudo ln -s [PATH TO GOG]/gog /usr/bin/

After that, you can run gog:

    gog

## Introduction

What is a language, if not words and rules how combine those words?
While programming languages excel at mind-bending ways to combine their primitives,
examples of elegant simplicity are rare. What if we wanted to create an absolutely minimalistic
language with no syntax rules at all? Wouldn't it be great if a programmer could write
expressions any way he wants? Well, GOG is one step in that direction.

GOG rules are:
* Tokens are separated by spaces
* Expressions are separated by parentheses

That's all. As you can see, no rules about the order of tokens. This is because
GOG is build around polynotational semantics.  The idea of polynotational
semantics is that languages have three main types of notation:

* Prefix notation: `func(arg1, arg2, arg3)`
* Infix notation: `x = 2 + 2 * 3`
* Postfix notation: `ls | rev | tac | split`

Each notation has its strengths, and GOG allows them all. Rurthermore,  
GOG understands what you mean without any syntax clues. For example:

    gog> + 1 2 3
    6
    gog> 1 + 2 + 3
    6
    gog> 9 sqrt prn prn
    3
    3


### What you can do

Some of additional GOG features are:
* Intuitive lambda function semantics:

Instead of

    (lambda (A B) (A + B / c))

you write

     [A + B / c]

Of course, you can use any notation inside lambdas, too. Just make sure that
lambda parameters start with capital letters. A more complex example:

    [map [*Elem / Num + *Elem] List]

GOG understands as:

   (lambda (List Num) (map (lambda (Elem) (Elem / Num + Elem)) List))

Note that Elem starts with an asterisk. This is how you inform that Elem
is a parameter of the inner lambda.  Also, List goes before Num in lambda
parameters, because lambda parameters are formed alphabetically.

* Minimalistic set of Adverbs, or function modifiers, creates reach calculus:

    gog> x = '((1 2 3) (4 5 6) (7 8 9))
    ((1 2 3) (4 5 6) (7 8 9))
    
    gog> $+ x
    (1 2 3 4 5 6 7 8 9)  ;; Flatten
    
    gog> @$+ x
    (6 15 24) ;; Sum rows
    
    gog> $@+ x
    (12 15 18) ;; Sum columns
    
    gog> $@&+ x
    ((1 4 7) (2 5 8) (3 6 9)) ;; Transpose

@ = map
$ = reduce
& = list

### 

### Example: sum square difference

Find the difference between the sum of the squares of the first one hundred
natural numbers and the square of the sum, 
i. e. (1^2 + 2^2 + ... 100^2) - (1 + 2 + ... 100)^2

* Prefix notation:

    (= x (range 1 100))
    (- (**_2 ($+ x)) ($+ (@**_2 x)))

* Infix notation:

* Postfix notation:

    1 range_100 !x @**_2 $+ [x $+ **_2 -_X]

Explanation: this expression is written using postfix notation. Postfix notation means that value is passed from one function to another. Every token is a function of one argument. `1` is  the initial value. `range_100` is a modified function `range`, to which by adverb `_` was attached argument 100. `range` takes 2 arguments, `from` and `to`, and returns a list of increasing values from `from` to `to` . `!` is an adverb that takes any name and makes a function that saves passed value to a variable with that name. So `!x` saves produced list to variable `x`. We use `x` later. `@**_2` looks complex, but it is just a modified function `**`, "power". `**_2` means "square", while `@` is a "mapping" adverb. It makes `**_2` map on our range list, squaring all its values. `$` is a "reduce" adverb. `$+` reduces list by summation. Finally, the last argument in the expression is a lambda. `[` and `]` denote a lambda function. GOG automatically guesses which variables inside a lambda are its parameters, and does this very simple: lambda function parameters start with a capital letter. Let's investigate this function separately.

It starts with `[` and ends with `]`. Inside it there are 3 functions: `$+` (reduce), `**_2` (square) and `-_X` (a function that subtracts variable X from its parameter. X is an argument of the lambda function, because it starts with a capital letter. `x` is the variable which was saved earlier. This lambda function reduces `x` by summation, squares the result, and subtracts its argument from it.

As you can see, GOG can be packed with meaning, boasting both tacit features and compact lambda functions with an arbitrary number of parameters (but not less than one): variables that start with a capital letter are regarded as parameters. Parameters are placed in alphabetic order.

GOG supports nested lambda functions of unlimited depth. To distinguish parameters of a nested function from parameters of its parent, you must prefix them with one or more asterisks. Number of asterisks must correspond to the nesting level of a function, which parameter it is. For example:

    [map [A + *B] C] 

creates a function with parameters `A` and `C`, and inside it a functin with one parameter `*B`.


# Backward compatibility

Gog is mostly backward compatible with the arc, in the sense that traditional arc expressions are recognized as prefix notation, except the following cases:

* You can not use a procedure or macro object as a data-call argument. It would classify as some arbitrary notation.
* Same refers to specifying more than one argument during data call (arc tables support second optional argument).

# Switching polynotation features off

Polynotational semantics is on by default. To switch it off completely, you can use "uniform-notation" declaration:

For example:

    (a = (table))
    (declare 'uniform-notation t)
    (= (a map) 'foo)
    (prn (a map))
    (declare 'uniform-notation nil)
    ((2 + 2) prn)
    =>
    foo
    4

# Executioin speed

I did no optimization whatsoever, as I wanted to keep this highly experimental code as straightforward as possible. That said, the overhead is generally insignificant. First attempt to determine notation is made at compile time. That effectively minimizes run-time overhead, especially for prefix notation. Thus, during arc loading, 10839 of 10318 examined expressions (~95%) recognise as prefix notation at compile time. The rest requires run-time notation discovery, which should result in slight overhead for traditional expressions, and yet somewhat more substantial for infix and postfix ones. Nevertheless, I was not able to notice any of it, using simple benchmarks. This topic might require further investigation, but as far as I can see, the execution overhead seems to be low.

# Infix precedence rules

It would be a shame to introduce infix notation to a Lisp without precedence rules. GOG uses same precedence as C language does. New functions get precedence by association: you
