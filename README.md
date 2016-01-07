GOG
===
Experimental language with polynotational semantics
-----
!!! This document is unfinished
# Introduction

What is a language, if not words plus rules of combination?
While programming languages excel at mind-bending ways to combine their primitives,
examples of elegant simplicity are rare. What if we wanted to create an absolutely minimalistic
language with no syntax rules at all? Wouldn't it be great if a programmer could write
expressions any way he wants? Well, GOG is one step in that direction.

The idea of polynotational semantics is that languages have three main types of notation:

* Prefix notation: `func(arg1, arg2, arg3)`
* Infix notation: `x = 2 + 2 * 3`
* Postfix notation: `ls | grep x | tee y`

GOG understands what you mean without any syntax clues. For example:

    > + 1 2 3
    6
    > 1 + 2 + 3
    6
    > 9 sqrt prn prn
    3
    3

### What you can do

Some of GOG features are:
* Polynotational semantics
* Simplified lambda functions
* Adverbs

### Example: sum square difference

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum, i. e. (1^2 + 2^2 + ... 100^2) - (1 + 2 + ... 100)^2

    1 range_100 !x @**_2 $+ [x $+ **_2 -_X]

Explanation: this expression is written using postfix notation. Postfix notation means that value is passed from one function to another. Every token is a function of one argument. `1` is  the initial value. `range_100` is a modified function `range`, to which by adverb `_` was attached argument 100. `range` takes 2 arguments, `from` and `to`, and returns a list of increasing values from `from` to `to` . `!` is an adverb that takes any name and makes a function that saves passed value to a variable with that name. So `!x` saves produced list to variable `x`. We use `x` later. `@**_2` looks complex, but it is just a modified function `**`, "power". `**_2` means "square", while `@` is a "mapping" adverb. It makes `**_2` map on our range list, squaring all its values. `$` is a "reduce" adverb. `$+` reduces list by summation. Finally, the last argument in the expression is a lambda. `[` and `]` denote a lambda function. GOG automatically guesses which variables inside a lambda are its parameters, and does this very simple: lambda function parameters start with a capital letter. Let's investigate this function separately.

It starts with `[` and ends with `]`. Inside it there are 3 functions: `$+` (reduce), `**_2` (square) and `-_X` (a function that subtracts variable X from its parameter. X is an argument of the lambda function, because it starts with a capital letter. `x` is the variable which was saved earlier. This lambda function reduces `x` by summation, squares the result, and subtracts its argument from it.

As you can see, GOG can be very compact, boasting both tacit features and very compact in-line lambda functions. 



# Installation

Assuming you have ubuntu:

    # sudo apt-get install racket rlwrap

After that, you can run gog:

    ./gog

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
