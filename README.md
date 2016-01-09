GOG
===
Experimental language with polynotational semantics
-----

# Installation

1. Install Rlwrap and Racket. Assuming you have ubuntu:

    sudo apt-get install racket rlwrap

2. Add GOG launcher to your path. Assuming you have put GOG into /opt/:

    sudo ln -s /opt/gog/gog /usr/bin/

After that, you can run GOG interactively and pass programs to it, for example:

    gog
    gog examples/life.gog


## Introduction

Any language is fully defined by its words and by the ways of how it combines
those words. Similar to a good vocabulary, that allows to convey one meaning
with different words, a good language enables us to express semantics in
different ways. Ideally, robust language should have enough syntactic cases to
leave an impression that it has no rules at all, so that programmers could
write expressions the way they want --  this adaptability is what will make a
language both powerful and legible, both maintainable and convenient.

GOG in one step in that direction.  While programming languages often excel at
mind-bending ways to combine their primitives, examples of elegant simplicity
are rare. Though GOG is rather complex internally, its complexity is of a kind
that makes it simple for the programmer.

GOG rules are:
* Words are separated by spaces
* Expressions are separated by either line breaks or parentheses

That's right, no strict order of tokens, and no fancy brackets and arrows.
That's because GOG uses polynotational semantics. The idea of polynotational
semantics is based on well-known observation, that languages have three main
types of notation:

* Prefix notation: `func(arg1, arg2, arg3)`
* Infix notation: `x = 2 + 2 * 3`
* Postfix notation: `ls | rev | tac | split`

Each notation has its strengths, and GOG allows them all. Importantly,
most of the time, GOG understands what you mean without additional syntactic
clues. For example:

    gog> + 1 2 3
    6
    gog> 1 + 2 + 3
    6
    gog> 9 sqrt prn prn
    3
    3
    3

This reduces basic syntax to just alphanumeric keys plus `-*+()`, and you can
write very sophisticated programs in GOG with those. On top of that, GOG builds
some features that do utilize your spare keyboard range.

Some of GOG unique features are:

* Lambda function semantics
* List operations through adverbs

### What you can do

Instead of

    (lambda (A B) (A + B))

you can write

     [A + B]

To distinguish lambda arguments from other names, you start them with capital
letters. Of course, you can use any notation inside lambdas too, as well as
nest lambda functions infinitely.

#### Nested example

    [map [*Elem / Num] List]

GOG understands as:

    (lambda (List Num) (map (lambda (Elem) (Elem / Num)) List))

Note that Elem starts with an asterisk. This is how you inform that Elem is a
parameter of the inner lambda. The deeper your nesting, the more asterisks you
add. Also note that List goes before Num in lambda parameters. It is because
auto-guessed lambda parameters always follow the alphabetic order.

### Fun with adverbs

Here I show how adverbs influence function behavior
Note that in the last example `+` works as identity function.

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

@ = map; $ = apply; & = collect

You can use adverbs with any expression that evaluates to a function, including
lambdas and subexpressions that return functions.  For example: `&[X + 2]`, `$(get-my-function 5)`.

### Example: sum square difference

Find the difference between the sum of the squares of the first one hundred
natural numbers and the square of their sum, i. e:
(1 + 2 + ... 100)^2 - (1^2 + 2^2 + ... 100^2)

*Prefix notation:*

    (= x (range 1 100))
    (- (**_2 ($+ x)) ($+ (@**_2 x)))

*Infix notation:*

    x = 1 range 100
    ($+ x) ** 2 - ($+ (@**_2 x))

*Postfix notation:*

    1 range_100 !x @**_2 $+ [x $+ **_2 -_X]

**Explanation of the last case**: Postfix notation passes single value from one function to another. Every token must evaluate to a function of one argument. Let's trace the expression: `1` is  the initial value. `range_100` is a modified function `range`, to which by syntax `_` was attached argument 100. `range` takes 2 arguments, `from` and `to`, and returns a list of increasing values from `from` to `to` . `!` is an adverb that takes any name and makes a function that saves its argument to a variable with that name. So `!x` assigns obtained list to variable `x`, which we will use later. `@**_2` looks complex, but it is just a modified exponentiation function `**`. `**_2` means "square", while `@` is a "mapping" adverb. It makes `**_2` map on our range list, squaring all its values. `$` is a "reduce" adverb. `$+` reduces list by summation. Finally, the last subexpression is a lambda. `[` and `]` denote a lambda function. GOG automatically guesses which variables inside a lambda are its parameters, and algorithm is very simple: lambda function parameters start with a capital letter. Let's investigate this function separately.

It starts with `[` and ends with `]`. Inside, there are 3 functions: `$+` (reduce), `**_2` (square) and `-_X` (a function that subtracts variable X from its parameter. X is an argument of the lambda function, because it starts with a capital letter. `x` is the variable which was saved earlier. This lambda function reduces `x` with summation, squares the result, and subtracts its argument from it.

# Why GOG?

As you can see, GOG expressions can be packed with meaning. Using both tacit features and compact lambda notation allows to write concise and powerful programs which are easy to understand. But more importantly, GOG allows experiments with different ways to write a program. With GOG, you can make your program shorter or longer, crystal-clear or opaque, and I'm sure that, though imperfect as it is, GOG may be very useful for people-centric experiments with programming languages.

# Relation to ARC and LISP:

GOG is built in ARC, which is a very advanced functional language by itself:

https://en.wikipedia.org/wiki/Arc_%28programming_language%29

The initial idea was to add polynotational semantics to arc as a patch.
Unfortunately, it broke some of the arc's semantics. Additionally, GOG's
adverbal syntax considerably differs from arc.

As a result, GOG has data types and features of a lisp: macros, symbols, etc.
See ARC 

# Backward compatibility

Gog is mostly backward compatible with the arc, in sense that traditional arc
expressions are recognized as prefix notation, except the following cases:

* You can not use a procedure or macro object as a data-call argument. It would classify as some arbitrary notation.
* Same refers to specifying more than one argument during data call (arc tables support second optional argument).

# Execution speed

I did no optimization whatsoever, as I wanted to keep this highly experimental code as straightforward as possible. That said, the overhead is generally insignificant. First attempt to determine notation is made at compile time. That effectively minimizes run-time overhead, especially for prefix notation. Thus, during arc loading, 10839 of 10318 examined expressions (~95%) recognise as prefix notation at compile time. The rest requires run-time notation discovery, which should result in slight overhead for traditional expressions, and yet somewhat more substantial for infix and postfix ones. Nevertheless, I was not able to notice any of it, using simple benchmarks. This topic might require further investigation, but as far as I can see, the execution overhead seems to be low.

# Infix precedence rules

It would be a shame to introduce infix notation to a Lisp without precedence rules. GOG uses same precedence as C language does. New functions get precedence by association. See ac.scm comments for details, including how to set precedence order for your function.
